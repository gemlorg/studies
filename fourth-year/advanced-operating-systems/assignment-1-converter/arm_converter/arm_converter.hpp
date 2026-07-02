#pragma once
#include <capstone/capstone.h>
#include <capstone/x86.h>
#include <capstone/arm64.h>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <elf.h>
#include <keystone/keystone.h>
#include <memory>
#include <optional>
#include <string>
#include <unistd.h>
#include <unordered_map>
#include <vector>

namespace arm_converter {
constexpr size_t MAX_JMP_SIZE = 6;

class section {
public:
  Elf64_Shdr *header;
  std::string name;
  std::shared_ptr<std::vector<uint8_t>> contents;
  section() = default;
  ~section() = default;
  section(Elf64_Shdr *header, std::string name, char *data)
      : header(header), name(std::string(name)) {
    contents =
        std::make_shared<std::vector<uint8_t>>(data, data + header->sh_size);
  }
  virtual void
  fix_indices(std::unordered_map<Elf64_Section, Elf64_Section> &remove_map);
};

class symbol {

public:
  Elf64_Sym *_underlying;
  std::shared_ptr<section> section_ptr;
  void
  fix_indices(std::unordered_map<Elf64_Section, Elf64_Section> &remove_map);
  symbol(Elf64_Sym *sym, std::shared_ptr<section> sec)
      : _underlying(sym), section_ptr(sec) {};
  virtual ~symbol() = default;
};

class function : public symbol {
public:
  std::vector<cs_insn *> insn;
  ks_arch arch = KS_ARCH_ARM64;
  ks_mode mode = KS_MODE_LITTLE_ENDIAN;
  bool is_converted = false;

  function(Elf64_Sym *sym, std::shared_ptr<section> sec) : symbol(sym, sec) {
    parse_insn();
  };
  ~function() { free_insn(); }
  void parse_insn();
  void commit_insn();
  void free_insn();
  std::string insn_to_string();
};

class symbol_table : public section {
public:
  std::vector<std::shared_ptr<symbol>> symbols;

  symbol_table() = default;
  ~symbol_table() = default;
  symbol_table(Elf64_Shdr *header, std::string name, char *data)
      : section(header, name, data) {}

  symbol_table(std::shared_ptr<section> sec);
  void parse_symbols(std::vector<std::shared_ptr<section>> sections);
  void
  fix_indices(std::unordered_map<Elf64_Section, Elf64_Section> &remove_map);
  void save_functions();
};

class relocation_table : public section {
public:
  std::vector<Elf64_Rela *> relocations;
  relocation_table() = default;
  ~relocation_table() = default;
  relocation_table(Elf64_Shdr *header, std::string name, char *data)
      : section(header, name, data) {}
  relocation_table(std::shared_ptr<section> sec);
  void put_symbols(symbol_table *smtable);
  void commit_relocations();
};

class string_table : public section {
public:
  string_table() = default;
  ~string_table() = default;
  string_table(Elf64_Shdr *header, std::string name, char *data)
      : section(header, name, data) {}

  string_table(std::shared_ptr<section> sec) {
    this->header = sec->header;
    this->name = sec->name;
    this->contents = sec->contents;
  }
  std::string get_name(Elf64_Word offset);
};

class program {
private:
  void set_tables();
  void init_sections();
  void fix_indices();
  void format_sec_strtable();
  size_t fix_offsets();

protected:
  std::unordered_map<Elf64_Section, Elf64_Section> remove_map{};

public:
  std::vector<uint8_t> elf_contents;
  std::vector<std::shared_ptr<section>> sections;
  std::shared_ptr<string_table> sm_strtable;
  std::shared_ptr<string_table> sec_strtable;
  std::shared_ptr<symbol_table> smtable;
  std::vector<std::shared_ptr<relocation_table>> reltables;
  Elf64_Ehdr *header;
  program() = default;
  ~program() = default;
  void add_sections(char *path);
  void filter_sections(std::string filter);
  void to_file(std::string path);
  std::vector<Elf64_Rela *>
  get_function_relocations(std::shared_ptr<function> f);
};

class converter {
private:
  program *prog;
  cs_insn *cur_insn;
  csh handle;
  ks_engine *ks_handle;
  std::vector<cs_insn *> curr_buff;
  std::vector<std::tuple<uint64_t, unsigned int, cs_insn *, size_t>>
      unresolved_jumps;
  std::vector<Elf64_Rela *> curr_relocations;
  size_t *curr_offset;
  std::unordered_map<size_t, size_t> *curr_addr_map;
  std::vector<size_t> offsets;
  std::vector<std::unordered_map<size_t, size_t>> addr_maps;

  void convert_headers();
  void convert_functions();
  void convert_relocations();
  void convert_function(std::shared_ptr<function> func);
  void convert_instruction(cs_insn &instr);
  void convert_ldr();
  void convert_str();
  void convert_adrp();
  void convert_mov_cmp();
  void convert_add();
  void _convert_add(size_t arg1, size_t arg2);
  void convert_bl();
  void convert_b();
  void convert_prologue(std::string prologue_shift);
  void convert_epilogue();
  void resolve_jumps();
  void convert_curr_relocation(int type, size_t instr_size);
  cs_insn *compose_x86_instr(unsigned int id, std::string op1, std::string op2,
                             std::optional<size_t> offset = std::nullopt);
  std::string convert_addr(std::string size, cs_arm64_op &op);

public:
  converter(program *prog);
  ~converter();
  void convert();
};

void verify_args(char *arm_path, char *output_path);
std::shared_ptr<symbol>
symbol_factory(Elf64_Sym *sym, std::vector<std::shared_ptr<section>> sections);
} // namespace arm_converter
