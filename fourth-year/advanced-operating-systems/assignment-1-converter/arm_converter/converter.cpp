#include <algorithm>
#include <arm_converter/arm_converter.hpp>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <elf.h>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace utils {
std::string convert_operand(cs_arm64_op &op);
std::string convert_register(arm64_reg &op);
std::string register_size(const cs_arm64_op &op);
std::string get_prologue_shift(const cs_insn &insn);
static const std::unordered_map<arm64_cc, x86_insn> arm_cc_to_x86_jcc = {
    {ARM64_CC_EQ, X86_INS_JE},  {ARM64_CC_NE, X86_INS_JNE},
    {ARM64_CC_HS, X86_INS_JAE}, {ARM64_CC_LO, X86_INS_JB},
    {ARM64_CC_MI, X86_INS_JS},  {ARM64_CC_PL, X86_INS_JNS},
    {ARM64_CC_VS, X86_INS_JO},  {ARM64_CC_VC, X86_INS_JNO},
    {ARM64_CC_HI, X86_INS_JA},  {ARM64_CC_LS, X86_INS_JBE},
    {ARM64_CC_GE, X86_INS_JGE}, {ARM64_CC_LT, X86_INS_JL},
    {ARM64_CC_GT, X86_INS_JG},  {ARM64_CC_LE, X86_INS_JLE}};

} // namespace utils

namespace arm_converter {

converter::converter(program *program)
    : prog(program), ks_handle(nullptr), unresolved_jumps(), offsets(),
      addr_maps() {

  if (cs_open(CS_ARCH_X86, CS_MODE_64, &handle) != CS_ERR_OK) {
    throw std::runtime_error("Failed to initialize Capstone");
  }
  auto cs_err = cs_option(handle, CS_OPT_DETAIL, CS_OPT_ON);
  if (cs_err != CS_ERR_OK) {
    cs_close(&handle); // Clean up handle before throwing
    throw std::runtime_error("Failed to set Capstone detail option");
  }
  ks_engine *ks;
  auto err = ks_open(KS_ARCH_X86, KS_MODE_64, &ks);
  if (err != KS_ERR_OK) {
    ks_close(ks_handle);
    throw std::runtime_error("Failed to initialize Keystone: " +
                             std::string(ks_strerror(err)));
  }
  ks_handle = ks;
  for (size_t i = 0; i < prog->sections.size(); i++) {
    offsets.push_back(0);
    addr_maps.push_back({});
  }
}

converter::~converter() {
  if (ks_handle) {
    ks_close(ks_handle);
  }
  cs_close(&handle);
}

void converter::convert() {
  convert_headers();
  convert_relocations();
  convert_functions();
}

void converter::convert_relocations() {
  for (auto sec : prog->sections) {
    if (std::shared_ptr<relocation_table> rel_table =
            std::dynamic_pointer_cast<relocation_table>(sec)) {
      for (auto rel : rel_table->relocations) {
        rel->r_info = ELF64_R_INFO(ELF64_R_SYM(rel->r_info), R_X86_64_64);
      }
    }
  }
}
void converter::convert_headers() { prog->header->e_machine = EM_X86_64; }
void converter::convert_functions() {
  for (auto sym : prog->smtable->symbols) {
    if (std::shared_ptr<function> f = dynamic_pointer_cast<function>(sym)) {
      convert_function(f);
    }
  }
  prog->smtable->save_functions();
}
void converter::convert_function(std::shared_ptr<function> func) {
  curr_buff = std::vector<cs_insn *>();
  curr_relocations.clear();
  unresolved_jumps.clear();
  curr_offset = &offsets[func->_underlying->st_shndx];
  curr_addr_map = &addr_maps[func->_underlying->st_shndx];
  curr_relocations = prog->get_function_relocations(func);
  func->_underlying->st_value = *curr_offset;
  func->arch = KS_ARCH_X86;
  func->mode = KS_MODE_64;
  convert_prologue(utils::get_prologue_shift(*func->insn[0]));

  for (size_t i = 2; i < func->insn.size() - 2; i++) {
    curr_addr_map->insert({func->insn[i]->address, *curr_offset});
    convert_instruction(*func->insn[i]);
  }

  curr_addr_map->insert(
      {func->insn[func->insn.size() - 2]->address, *curr_offset});
  curr_addr_map->insert(
      {func->insn[func->insn.size() - 1]->address, *curr_offset});

  convert_epilogue();
  resolve_jumps();

  func->_underlying->st_size = *curr_offset - func->_underlying->st_value;
  func->free_insn();
  func->is_converted = true;
  func->insn = curr_buff;
}

void converter::resolve_jumps() {
  for (auto jump : unresolved_jumps) {
    auto [old_addr, id, ptr, offset] = jump;
    size_t res = curr_addr_map->at(old_addr);
    std::stringstream ss;

    ss << res;
    auto new_instr = compose_x86_instr(id, ss.str(), "", offset);
    assert(new_instr->size <= MAX_JMP_SIZE);
    size_t num_nop = MAX_JMP_SIZE - new_instr->size;

    size_t index = (size_t)std::distance(
        curr_buff.begin(), std::find(curr_buff.begin(), curr_buff.end(), ptr));
    assert(index < curr_buff.size());
    curr_buff[index] = new_instr;
    for (size_t i = 0; i < num_nop; i++) {
      cs_insn *noop_instr = compose_x86_instr(X86_INS_NOP, "", "");
      curr_buff.insert(curr_buff.begin() + (long)index + 1, noop_instr);
    }
  }
}

void converter::convert_instruction(cs_insn &instr) {
  cur_insn = &instr;

  switch (instr.id) {
  case ARM64_INS_LDR:
    convert_ldr();
    break;
  case ARM64_INS_STR:
    convert_str();
    break;
  case ARM64_INS_ADRP:
    convert_adrp();
    break;
  case ARM64_INS_MOV:
  case ARM64_INS_CMP:
    convert_mov_cmp();
    break;
  case ARM64_INS_ADD:
    convert_add();
    break;
  case ARM64_INS_BL:
    convert_bl();
    break;
  case ARM64_INS_B:
    convert_b();
    break;
  case ARM64_INS_NOP:
    break;
  default:
    throw std::runtime_error(std::string("Unsupported instruction: ") +
                             instr.mnemonic);
  }
}

void converter::convert_prologue(std::string prologue_shift) {
  // push rbp
  cs_insn *push_rbp_instr = compose_x86_instr(X86_INS_PUSH, "rbp", "");
  curr_buff.push_back(push_rbp_instr);
  *curr_offset += push_rbp_instr->size;

  // mov rbp, rsp
  cs_insn *mov_rbp_rsp_instr = compose_x86_instr(X86_INS_MOV, "rbp", "rsp");
  curr_buff.push_back(mov_rbp_rsp_instr);
  *curr_offset += mov_rbp_rsp_instr->size;

  // sub rsp, #prologue_shift
  cs_insn *sub_rsp_instr =
      compose_x86_instr(X86_INS_SUB, "rsp", prologue_shift);
  curr_buff.push_back(sub_rsp_instr);
  *curr_offset += sub_rsp_instr->size;
}

void converter::convert_epilogue() {
  // mov rax, rdi
  cs_insn *mov_rax_rdi_instr = compose_x86_instr(X86_INS_MOV, "rax", "rdi");
  curr_buff.push_back(mov_rax_rdi_instr);
  *curr_offset += mov_rax_rdi_instr->size;

  // leave
  cs_insn *leave_instr = compose_x86_instr(X86_INS_LEAVE, "", "");
  curr_buff.push_back(leave_instr);
  *curr_offset += leave_instr->size;

  // ret
  cs_insn *ret_instr = compose_x86_instr(X86_INS_RET, "", "");
  curr_buff.push_back(ret_instr);
  *curr_offset += ret_instr->size;
}

void converter::convert_ldr() {
  // ldr reg, [base, disp] (load from memory)
  // The following code shall be generated:
  //
  // mov {op1}, {size_qualifier} [{op2.base} + {op2.disp}]
  // where {size_qualifier} is qword ptr if {op1} is 64-bit and dword ptr if
  // it is 32-bit.
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 2) {
    throw std::runtime_error("Invalid number of operands in LDR instruction");
  }
  std::string op1 = utils::convert_operand(arm->operands[0]);

  // mov {op1}, {size_qualifier} [{op2.base} + {op2.disp}]
  std::string addr =
      convert_addr(utils::register_size(arm->operands[0]), arm->operands[1]);
  cs_insn *new_instr = compose_x86_instr(X86_INS_MOV, op1, addr);
  curr_buff.push_back(new_instr);
  *curr_offset += new_instr->size;
}

void converter::convert_str() {
  // str reg, [base, disp] (store to memory)
  // The following code shall be generated:
  //
  // mov {size_qualifier} [{op2.base} + {op2.disp}], {op1}
  // where {size_qualifier} is defined like above.
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 2) {
    throw std::runtime_error("Invalid number of operands in STR instruction");
  }

  std::string op1 = utils::convert_operand(arm->operands[0]);

  // mov {size_qualifier} [{op2.base} + {op2.disp}], {op1}
  std::string addr =
      convert_addr(utils::register_size(arm->operands[0]), arm->operands[1]);
  auto new_instr = compose_x86_instr(X86_INS_MOV, addr, op1);
  curr_buff.push_back(new_instr);
  *curr_offset += new_instr->size;
}
void converter::convert_adrp() {
  // adrp reg, imm (get page address)
  // We assume that the immediate (ofkjkkfset) has a relocation of type
  // R_AARCH64_ADR_PREL_PG_HI21. The relocation shall be converted to a
  // relocation of type R_X86_64_PC32 and the following code shall be
  // generated:
  //
  // lea {op1}, [rip + 0x7fffffff] # the displacement forces the assembler to
  // use a 32-bit immediate; it is relocated and {op1}, ~0xfff # set 12 lowest
  // bits to 0
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 2) {
    throw std::runtime_error("Invalid number of operands in STR instruction");
  }

  auto op1 = utils::convert_operand(arm->operands[0]);

  // lea {op1}, [rip + 0x7fffffff] # the displacement forces the assembler to
  // use a 32-bit immediate; it is relocated
  auto new_instr = compose_x86_instr(
      X86_INS_LEA, op1,
      "[rip + 0x7fffffff]"); // the displacement forces the assembler to use a
                             // 32-bit immediate; it is relocated
  curr_buff.push_back(new_instr);
  convert_curr_relocation(R_X86_64_PC32,
                          new_instr->detail->x86.encoding.disp_offset);
  *curr_offset += new_instr->size;

  // and {op1}, ~0xfff # set 12 lowest bits to 0
  auto and_instr = compose_x86_instr(X86_INS_AND, op1, "~0xfff");
  curr_buff.push_back(and_instr);
  *curr_offset += and_instr->size;
}

void converter::convert_mov_cmp() {
  // mov/cmp reg, reg/imm
  // The following code shall be generated:
  //
  // {mnemonic} {op1}, {op2}
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 2) {
    throw std::runtime_error("Invalid number of operands in MOV instruction");
  }
  auto op1 = utils::convert_operand(arm->operands[0]);
  auto op2 = utils::convert_operand(arm->operands[1]);
  auto id = cur_insn->id == ARM64_INS_MOV ? X86_INS_MOV : X86_INS_CMP;

  // {mnemonic} {op1}, {op2}
  auto new_instr = compose_x86_instr(id, op1, op2);
  curr_buff.push_back(new_instr);
  *curr_offset += new_instr->size;
}

void converter::convert_add() {
  // add reg, reg, reg/imm
  // If {op1} is the same as {op2} (that is, it is the same register), the
  // following code shall be generated:

  // {add op3 to op1}
  // Otherwise if {op1} is the same as {op3}, the following code shall be
  // generated:

  // {add op2 to op1}
  // Otherwise the following code shall be generated:

  // mov {op1}, {op2}
  // {add op3 to op1}
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 3) {
    throw std::runtime_error("Invalid number of operands in ADD instruction");
  }
  if (arm->operands[0].type != ARM64_OP_REG ||
      arm->operands[1].type != ARM64_OP_REG ||
      (arm->operands[2].type != ARM64_OP_REG &&
       arm->operands[2].type != ARM64_OP_IMM)) {
    throw std::runtime_error("Invalid operand types in ADD instruction");
  }
  arm64_reg op1_arm = arm->operands[0].reg;
  arm64_reg op2_arm = arm->operands[1].reg;
  size_t op1;
  size_t op2;
  if (op1_arm == op2_arm) {
    // add op3 to op1
    op1 = 2;
    op2 = 0;

  } else if (arm->operands[2].type == ARM64_OP_REG &&
             op1_arm == arm->operands[2].reg) {
    // add op2 to op1
    op1 = 1;
    op2 = 0;
  } else {
    // mov op1, op2
    auto mov_instr =
        compose_x86_instr(X86_INS_MOV, utils::convert_register(op1_arm),
                          utils::convert_register(op2_arm));
    curr_buff.push_back(mov_instr);
    *curr_offset += mov_instr->size;
    op1 = 0;
    op2 = 1;
  }
  _convert_add(op1, op2);
}
void converter::_convert_add(size_t arg1_idx, size_t arg2_idx) {
  // Where the definition of {add opy to opx} is as follows:

  // If {opy} is an immediate and has a relocation of type
  // R_AARCH64_ADD_ABS_LO12_NC, the relocation shall be converted to a
  // relocation of type R_X86_64_32 and the following code shall be generated:

  // mov {tmp}, 0x7fffffff # the immediate is relocated
  // and {tmp}, 0xfff
  // add {opx}, {tmp}
  // Otherwise the following code shall be generated:
  // add {opx}, {opy}

  cs_arm64 *arm = &(cur_insn->detail->arm64);
  cs_arm64_op opx_arm = arm->operands[arg2_idx];
  cs_arm64_op opy_arm = arm->operands[arg1_idx];

  std::string opx_str = utils::convert_operand(opx_arm);
  std::string opy_str = utils::convert_operand(opy_arm);

  bool has_add_reloc = curr_relocations.size() > 0 &&
                       curr_relocations.front()->r_offset == cur_insn->address;

  if (opy_arm.type == ARM64_OP_IMM && has_add_reloc) {
    bool is_opx_32bit = utils::register_size(opx_arm) == "dword ptr";
    std::string tmp_reg = is_opx_32bit ? "r11d" : "r11";

    // mov {tmp}, 0x7fffffff # the immediate is relocated
    auto mov_instr = compose_x86_instr(X86_INS_MOV, tmp_reg, "0x7fffffff");
    curr_buff.push_back(mov_instr);
    convert_curr_relocation(R_X86_64_32,
                            mov_instr->detail->x86.encoding.imm_offset);

    *curr_offset += mov_instr->size;

    // and {tmp}, 0xfff
    auto and_instr = compose_x86_instr(X86_INS_AND, tmp_reg, "0xfff");
    curr_buff.push_back(and_instr);
    *curr_offset += and_instr->size;

    // add {opx}, {tmp}
    auto add_instr = compose_x86_instr(X86_INS_ADD, opx_str, tmp_reg);
    curr_buff.push_back(add_instr);
    *curr_offset += add_instr->size;

  } else {
    // add {opx}, {opy}
    auto add_instr = compose_x86_instr(X86_INS_ADD, opx_str, opy_str);
    curr_buff.push_back(add_instr);
    *curr_offset += add_instr->size;
  }
}

void converter::convert_bl() {

  // bl imm (function call)
  // We assume that the immediate (offset) has a relocation of type
  // R_AARCH64_CALL26. The relocation shall be converted to a relocation of type
  // R_X86_64_PC32 and the following code shall be generated:
  //
  // call 0x7fffffff # the offset is relocated
  // mov rdi, rax # put the return value in the register to which x0 maps
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 1 || arm->operands[0].type != ARM64_OP_IMM) {
    throw std::runtime_error("Invalid operands in BL instruction");
  }

  // call 0x7fffffff # the offset is relocated
  auto call_instr = compose_x86_instr(X86_INS_CALL, "0x7fffffff", "");
  curr_buff.push_back(call_instr);
  convert_curr_relocation(R_X86_64_PC32,
                          call_instr->detail->x86.encoding.imm_offset);
  *curr_offset += call_instr->size;

  // mov rdi, rax # put the return value in the register to which x0 maps
  auto mov_instr = compose_x86_instr(X86_INS_MOV, "rdi", "rax");
  curr_buff.push_back(mov_instr);
  *curr_offset += mov_instr->size;
}
void converter::convert_b() {
  // Branches (jumps)
  // The offset of each branch must be adjusted to the converted code - so
  // that it branches to the instructions to which the original target
  // instruction was converted. In the description of the generated code this
  // adjustment is denoted by {adjust(op1)}. We assume that branches always
  // happen within a function.
  //
  // b imm (branch)
  // The following code shall be generated:
  //
  // jmp {adjust(op1)}
  // b.cond imm (conditional branch)
  // The following code shall be generated:
  //
  // j{cond} {adjust(op1)}
  // The mapping of the conditions in conditional branches is as follows:

  //
  // eq -> e
  // ne -> ne
  // hs -> ae
  // lo -> b
  // mi -> s
  // pl -> ns
  // vs -> o
  // vc -> no
  // hi -> a
  // ls -> be
  // ge -> ge
  // lt -> l
  // gt -> g
  // le -> le
  //
  cs_arm64 *arm = &(cur_insn->detail->arm64);
  if (arm->op_count != 1 || arm->operands[0].type != ARM64_OP_IMM) {
    throw std::runtime_error("Invalid operands in B instruction");
  }

  auto target_addr = arm->operands[0].imm;
  std::string target_op = std::to_string(target_addr);
  unsigned int x86_insn_id = X86_INS_INVALID; // Initialize

  if (arm->cc == ARM64_CC_AL || arm->cc == ARM64_CC_INVALID) {
    x86_insn_id = X86_INS_JMP;
  } else {
    auto it = utils::arm_cc_to_x86_jcc.find(arm->cc);
    if (it != utils::arm_cc_to_x86_jcc.end()) {
      x86_insn_id = it->second;
    } else {
      throw std::runtime_error(
          "Unsupported AArch64 condition code for B instruction: " +
          std::to_string(arm->cc));
    }
  }
  cs_insn new_instr{};
  curr_buff.push_back(&new_instr);
  unresolved_jumps.push_back({target_addr, x86_insn_id,
                              curr_buff[curr_buff.size() - 1], *curr_offset});
  *curr_offset += MAX_JMP_SIZE;
}

void converter::convert_curr_relocation(int type, size_t instr_size) {
  auto reloc = curr_relocations.front();
  curr_relocations.erase(curr_relocations.begin());
  reloc->r_offset = *curr_offset + instr_size;
  if (type == R_X86_64_PC32) {
    reloc->r_addend -= 4;
  }
  reloc->r_info = ELF64_R_INFO(ELF64_R_SYM(reloc->r_info), type);
}

std::string converter::convert_addr(std::string size, cs_arm64_op &op) {
  auto addr = op.mem;
  arm64_reg base_reg = addr.base;
  std::string base = utils::convert_register(base_reg);
  auto disp = std::to_string(addr.disp);
  std::string converted_addr = size + " [" + base + " + " + disp + "]";
  return converted_addr;
}

cs_insn *converter::compose_x86_instr(unsigned int id, std::string op1,
                                      std::string op2,
                                      std::optional<size_t> offset) {

  unsigned char *encode = nullptr;
  size_t encode_size = 0, count = 0;
  cs_insn *dis_insn = nullptr;
  size_t dis_count = 0;
  std::string op_str = op1;
  size_t offset_val = offset.value_or(*curr_offset);
  std::stringstream ss;
  int err;

  const char *mnemonic = cs_insn_name(handle, id);

  if (mnemonic == nullptr) {
    throw std::runtime_error("Failed to get mnemonic for instruction ID");
  }

  if (!op2.empty()) {
    op_str += ", " + op2;
  }

  ss << mnemonic << " " << op_str << "\n";

  err = ks_asm(ks_handle, ss.str().c_str(), offset_val, &encode, &encode_size,
               &count);
  if (!(err == KS_ERR_OK && count == 1 && encode_size > 0)) {
    throw std::runtime_error("Keystone assembly failed");
  }
  dis_count = cs_disasm(handle, encode, encode_size, offset_val, 1, &dis_insn);

  if (encode)
    ks_free(encode);

  if (!(dis_count == 1 && dis_insn)) {
    if (dis_count == 0) {
      throw std::runtime_error("Disassembly failed");
    } else {
      cs_free(dis_insn, dis_count);
      throw std::runtime_error("Disassembly returned more than 1 instruction");
    }
  }

  if (dis_insn->detail == nullptr) {
    throw std::runtime_error("Capstone instruction details are missing.");
  }

  return dis_insn;
}

} // namespace arm_converter

namespace utils {
// # Caller-saved:
// x0 -> rdi # 1. argument
// x1 -> rsi # 2. argument
// x2 -> rdx # 3. argument
// x3 -> rcx # 4. argument
// x4 -> r8 # 5. argument
// x5 -> r9 # 6. argument
// x9 -> rax
// x10 -> r10
// # Callee-saved:
// x29 -> rbp
// x19 -> rbx
// x20 -> r12
// x21 -> r13
// x22 -> r14
// x23 -> r15
// sp -> rsp
// The 32-bit registers, which have a w prefix instead of x (e.g. w0, w23),
// shall be mapped analogously to 32-bit x86-64 registers (e.g. edi, r15d). sp
// in AArch64 is not a general-purpose register, and thus can only be used in
// certain instructions.
//
// Additionally, the special-purpose registers xzr and wzr, which are always
// equal to 0, shall be mapped to an immediate 0 whenever they appear in an
// instruction.

static std::unordered_map<arm64_reg, x86_reg> reg_map = {
    {ARM64_REG_X0, X86_REG_RDI}, // 1st argument
    {ARM64_REG_X1, X86_REG_RSI}, // 2nd argument
    {ARM64_REG_X2, X86_REG_RDX}, // 3rd argument
    {ARM64_REG_X3, X86_REG_RCX}, // 4th argument
    {ARM64_REG_X4, X86_REG_R8},  // 5th argument
    {ARM64_REG_X5, X86_REG_R9},  // 6th argument
    {ARM64_REG_X9, X86_REG_RAX},
    {ARM64_REG_X10, X86_REG_R10},

    // Callee-saved registers
    {ARM64_REG_X19, X86_REG_RBX},
    {ARM64_REG_X20, X86_REG_R12},
    {ARM64_REG_X21, X86_REG_R13},
    {ARM64_REG_X22, X86_REG_R14},
    {ARM64_REG_X23, X86_REG_R15},
    {ARM64_REG_X29, X86_REG_RBP},
    {ARM64_REG_SP, X86_REG_RSP},

    // 32-bit equivalents
    {ARM64_REG_W0, X86_REG_EDI},
    {ARM64_REG_W1, X86_REG_ESI},
    {ARM64_REG_W2, X86_REG_EDX},
    {ARM64_REG_W3, X86_REG_ECX},
    {ARM64_REG_W4, X86_REG_R8D},
    {ARM64_REG_W5, X86_REG_R9D},
    {ARM64_REG_W9, X86_REG_EAX},
    {ARM64_REG_W10, X86_REG_R10D},
    {ARM64_REG_W19, X86_REG_EBX},
    {ARM64_REG_W20, X86_REG_R12D},
    {ARM64_REG_W21, X86_REG_R13D},
    {ARM64_REG_W22, X86_REG_R14D},
    {ARM64_REG_W23, X86_REG_R15D},
    {ARM64_REG_W29, X86_REG_EBP},

    // Special-purpose zero register
    {ARM64_REG_XZR, X86_REG_INVALID},
    {ARM64_REG_WZR, X86_REG_INVALID}};

static const std::unordered_map<x86_reg, std::string> reg_str_map = {
    {X86_REG_RDI, "rdi"},   {X86_REG_RSI, "rsi"},   {X86_REG_RDX, "rdx"},
    {X86_REG_RCX, "rcx"},   {X86_REG_R8, "r8"},     {X86_REG_R9, "r9"},
    {X86_REG_RAX, "rax"},   {X86_REG_R10, "r10"},   {X86_REG_RBX, "rbx"},
    {X86_REG_R12, "r12"},   {X86_REG_R13, "r13"},   {X86_REG_R14, "r14"},
    {X86_REG_R15, "r15"},   {X86_REG_RBP, "rbp"},   {X86_REG_RSP, "rsp"},
    {X86_REG_EDI, "edi"},   {X86_REG_ESI, "esi"},   {X86_REG_EDX, "edx"},
    {X86_REG_ECX, "ecx"},   {X86_REG_R8D, "r8d"},   {X86_REG_R9D, "r9d"},
    {X86_REG_EAX, "eax"},   {X86_REG_R10D, "r10d"}, {X86_REG_EBX, "ebx"},
    {X86_REG_R12D, "r12d"}, {X86_REG_R13D, "r13d"}, {X86_REG_R14D, "r14d"},
    {X86_REG_R15D, "r15d"}, {X86_REG_EBP, "ebp"},   {X86_REG_INVALID, "0"}};

std::string x86_reg_to_string(x86_reg reg) {
  auto it = reg_str_map.find(reg);
  if (it != reg_str_map.end()) {
    return it->second;
  } else {
    throw std::runtime_error("Unknown register");
  }
}

std::string convert_operand(cs_arm64_op &op) {
  if (op.type == ARM64_OP_REG) {
    return ::utils::convert_register(op.reg);
  } else if (op.type == ARM64_OP_IMM) {
    return std::to_string(op.imm);
  } else {
    throw std::runtime_error("Unsupported operand type");
  }
}

std::string convert_register(arm64_reg &reg) {
  auto it = reg_map.find(reg);
  if (it != reg_map.end()) {
    return x86_reg_to_string(it->second);
  } else {
    throw std::runtime_error("Unsupported register");
  }
}

std::string register_size(const cs_arm64_op &op) {
  if (op.type == ARM64_OP_REG) {
    if ((op.reg >= ARM64_REG_W0 && op.reg <= ARM64_REG_W30) ||
        op.reg == ARM64_REG_WZR) {
      return "dword ptr";
    } else if ((op.reg >= ARM64_REG_X0 && op.reg <= ARM64_REG_X28) ||
               op.reg == ARM64_REG_XZR || op.reg == ARM64_REG_SP ||
               op.reg == ARM64_REG_X29 || op.reg == ARM64_REG_X30) {
      return "qword ptr";
    } else {
      throw std::runtime_error("Unsupported register operand");
    }
  } else if (op.type == ARM64_OP_IMM) {
    return "qword ptr";
  } else {
    throw std::runtime_error("Unsupported operand type (must be REG or IMM)");
  }
}

std::string get_prologue_shift(const cs_insn &insn) {
  // stp x29, x30, [sp, #-prologue_shift]!
  std::stringstream ss;
  ss << "0x" << std::hex << std::abs(insn.detail->arm64.operands[2].mem.disp);
  return ss.str();
}
} // namespace utils
