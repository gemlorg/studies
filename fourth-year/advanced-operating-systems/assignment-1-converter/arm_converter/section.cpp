#include <algorithm>
#include <arm_converter/arm_converter.hpp>
#include <cassert>
#include <cstdint>
#include <elf.h>
#include <iostream>
#include <memory>
#include <pstl/glue_algorithm_defs.h>
#include <sstream>

#include <csignal>
#include <unordered_map>

namespace arm_converter {

std::string string_table::get_name(Elf64_Word offset) {
  return std::string((char *)contents->data() + offset);
}

relocation_table::relocation_table(std::shared_ptr<section> sec) {
  this->header = sec->header;
  this->name = sec->name;
  this->contents = sec->contents;
  Elf64_Rela *relas = (Elf64_Rela *)contents->data();
  relocations = std::vector<Elf64_Rela *>();
  for (size_t i = 0; i < header->sh_size / header->sh_entsize; i++) {
    relocations.push_back(&relas[i]);
  }
}
symbol_table::symbol_table(std::shared_ptr<section> sec) {
  this->header = sec->header;
  this->name = sec->name;
  this->contents = sec->contents;
}
void symbol_table::parse_symbols(
    std::vector<std::shared_ptr<section>> sections) {
  Elf64_Sym *syms = (Elf64_Sym *)contents->data();
  symbols = std::vector<std::shared_ptr<symbol>>();
  for (size_t i = 0; i < header->sh_size / header->sh_entsize; i++) {
    symbols.push_back(symbol_factory(&syms[i], sections));
  }
}
void symbol_table::save_functions() {
  for (auto symobj : symbols) {
    if (std::shared_ptr<function> func =
            std::dynamic_pointer_cast<function>(symobj)) {
      func->section_ptr->contents->clear();
      func->section_ptr->header->sh_size = 0;
    }
  }
  for (auto symobj : symbols) {
    if (std::shared_ptr<function> func =
            std::dynamic_pointer_cast<function>(symobj)) {
      func->commit_insn();
    }
  }
}

void symbol_table::fix_indices(
    std::unordered_map<Elf64_Half, Elf64_Half> &remove_map) {
  section::fix_indices(remove_map);
  for (auto sym : symbols) {
    sym->fix_indices(remove_map);
  }
}

void symbol::fix_indices(
    std::unordered_map<Elf64_Half, Elf64_Half> &remove_map) {
  if (_underlying->st_shndx != SHN_UNDEF) {
    _underlying->st_shndx = remove_map[_underlying->st_shndx];
  }
}

std::shared_ptr<symbol>
symbol_factory(Elf64_Sym *sym, std::vector<std::shared_ptr<section>> sections) {
  std::shared_ptr<section> secptr = nullptr;
  if (sym->st_shndx > SHN_UNDEF && sym->st_shndx < SHN_LORESERVE) {
    secptr = sections[sym->st_shndx];
  }
  if (ELF64_ST_TYPE(sym->st_info) == STT_FUNC) {
    return std::make_shared<function>(sym, secptr);
  } else {
    return std::make_shared<symbol>(sym, secptr);
  }
}

void section::fix_indices(
    std::unordered_map<Elf64_Half, Elf64_Half> &remove_map) {
  if (header->sh_link < SHN_LORESERVE)
    header->sh_link = remove_map[(Elf64_Section)header->sh_link];
}

void function::parse_insn() {
  csh handle;
  cs_insn *insn_val = nullptr;

  cs_err err = cs_open(CS_ARCH_ARM64, CS_MODE_ARM, &handle);
  if (err != CS_ERR_OK) {
    throw std::runtime_error("Failed to initialize Capstone");
  }
  err = cs_option(handle, CS_OPT_DETAIL, CS_OPT_ON);
  if (err != CS_ERR_OK) {
    cs_close(&handle);
    throw std::runtime_error("Failed to set Capstone detail option: " +
                             std::string(cs_strerror(err)));
  }

  uint8_t *function_code =
      (uint8_t *)section_ptr->contents->data() + _underlying->st_value;
  size_t count = cs_disasm(handle, function_code, _underlying->st_size,
                           _underlying->st_value, 0, &insn_val);
  if (count == 0) {
    cs_err detail_err = cs_errno(handle);
    throw std::runtime_error("Failed to disassemble given code: " +
                             std::string(cs_strerror(detail_err)));
  }
  insn = std::vector<cs_insn *>();
  for (size_t i = 0; i < count; i++) {
    insn.push_back(&insn_val[i]);
  }
  cs_close(&handle);
}

void function::free_insn() {
  if (is_converted) {

    for (cs_insn *instr_ptr : insn) {
      if (instr_ptr) {
        cs_free(instr_ptr, 1);
      }
    }
  } else {
    cs_free(insn[0], insn.size());
  }
  insn.clear();
}

void function::commit_insn() {
  ks_engine *ks;
  ks_err err;
  size_t assemble_count;
  unsigned char *encode = nullptr;
  size_t encode_size;

  err = ks_open(arch, mode, &ks);
  if (err != KS_ERR_OK) {
    throw std::runtime_error("Failed to initialize Keystone: " +
                             std::string(ks_strerror(err)));
  }

  int asm_result = ks_asm(ks, insn_to_string().c_str(), insn[0]->address,
                          &encode, &encode_size, &assemble_count);

  if (asm_result != KS_ERR_OK) {
    ks_err detail_err = ks_errno(ks);
    ks_close(ks);
    throw std::runtime_error("Keystone failed to assemble code: " +
                             std::string(ks_strerror(detail_err)));
  }
  assert(encode_size == _underlying->st_size);

  size_t new_size = std::max(section_ptr->header->sh_size,
                             _underlying->st_value + encode_size);

  section_ptr->contents->resize(new_size);
  uint8_t *destination_start =
      section_ptr->contents->data() + _underlying->st_value;
  std::copy((uint8_t *)encode, (uint8_t *)encode + encode_size,
            destination_start);

  section_ptr->header->sh_size = new_size;
  assert(section_ptr->contents->size() == new_size);

  ks_free(encode);
  ks_close(ks);
  free_insn();
}

std::string function::insn_to_string() {
  std::ostringstream oss;
  for (auto instr : insn) {
    oss << instr->mnemonic << " " << instr->op_str << "\n";
  }
  return oss.str();
}

} // namespace arm_converter
