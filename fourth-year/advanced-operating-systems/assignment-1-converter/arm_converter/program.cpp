#include <arm_converter/arm_converter.hpp>
#include <elf.h>
#include <fstream>
#include <iostream>
#include <memory>
#include <regex>

namespace arm_converter {

std::vector<uint8_t> read_file(char *path) {
  std::ifstream file(path, std::ios::binary);
  if (!file) {
    throw std::runtime_error("Failed to open file");
  }
  auto res = std::vector<uint8_t>((std::istreambuf_iterator<char>(file)), {});
  file.close();
  return res;
}

void program::init_sections() {

  header = (Elf64_Ehdr *)elf_contents.data();
  Elf64_Shdr *shdrs = (Elf64_Shdr *)(elf_contents.data() + header->e_shoff);

  Elf64_Shdr *shstrtab = &shdrs[header->e_shstrndx];
  char *strtab = (char *)(elf_contents.data() + shstrtab->sh_offset);
  std::vector<char> strtab_name{strtab, strtab + shstrtab->sh_size};
  string_table strtable(shstrtab, std::string(strtab_name.data()), strtab);

  for (int i = 0; i < header->e_shnum; i++) {
    Elf64_Shdr *header = &shdrs[i];
    char *data = (char *)(elf_contents.data() + header->sh_offset);
    std::string name = strtable.get_name(header->sh_name);
    std::shared_ptr<section> sec =
        std::make_shared<section>(header, name, data);
    sections.push_back(sec);
  }
}

void program::add_sections(char *path) {
  elf_contents = read_file(path);
  init_sections();
  set_tables();
  return;
}

void program::set_tables() {
  for (auto &sec : sections) {
    switch (sec->header->sh_type) {
    case SHT_SYMTAB:
      smtable = std::make_shared<symbol_table>(sec);
      sec = smtable;
      break;
    case SHT_STRTAB:
      if (sec->name == ".strtab") {
        assert(sm_strtable == nullptr);
        sm_strtable = std::make_shared<string_table>(sec);
        sec = sm_strtable;
      } else {
        sec_strtable = std::make_shared<string_table>(sec);
        sec = sec_strtable;
      }
      break;
    case SHT_RELA:
      auto new_sec = std::make_shared<relocation_table>(sec);
      if (sec->name == ".rela.text") {
        reltables.push_back(new_sec);
      }
      sec = new_sec;
      break;
    }
  }
  smtable->parse_symbols(sections);
}

void program::filter_sections(std::string filter) {
  // sections called .note.gnu.property or *.eh_frame (for example .eh_frame,
  // .rela.eh_frame) - they shall be deleted (also from the section name
  // table)
  std::regex re(filter);
  size_t num_removed = 0;
  size_t i = 0;
  std::vector<std::shared_ptr<section>> new_sections;

  for (auto &sec : sections) {
    if (std::regex_search(sec->name, re)) {
      num_removed++;
      remove_map.insert({i, 0});
    } else {
      remove_map.insert({i, i - num_removed});
      new_sections.push_back(sec);
    }
    i++;
  }
  sections = new_sections;
  fix_indices();
}

void program::fix_indices() {
  header->e_shstrndx = remove_map[header->e_shstrndx];
  for (auto &sec : sections) {
    sec->fix_indices(remove_map);
  }
}

void program::format_sec_strtable() {
  char *old_data = (char *)sec_strtable->contents->data();
  std::vector<uint8_t> new_data{};

  for (auto sec : sections) {
    sec->header->sh_name = (Elf64_Word)new_data.size();
    char *sec_name = old_data + sec->header->sh_name;
    size_t name_len = std::strlen(sec_name);
    new_data.insert(new_data.end(), sec_name, sec_name + name_len + 1);
  }
  sec_strtable->contents = std::make_shared<std::vector<uint8_t>>(new_data);
}

std::vector<Elf64_Rela *>
program::get_function_relocations(std::shared_ptr<function> f) {
  std::vector<Elf64_Rela *> relocations;
  for (auto reltable : reltables) {
    for (auto rel : reltable->relocations) {
      if (rel->r_offset >= f->_underlying->st_value &&
          rel->r_offset <
              f->_underlying->st_value + f->section_ptr->header->sh_size &&
          ELF64_R_TYPE(rel->r_info) != R_AARCH64_ABS64) {
        relocations.push_back(rel);
      }
    }
  }
  std::sort(
      relocations.begin(), relocations.end(),
      [](Elf64_Rela *a, Elf64_Rela *b) { return a->r_offset < b->r_offset; });
  return relocations;
}

size_t program::fix_offsets() {
  header->e_shnum = (Elf64_Section)sections.size();
  header->e_shoff = sizeof(Elf64_Ehdr);

  size_t offset = sizeof(Elf64_Ehdr) + sizeof(Elf64_Shdr) * sections.size();
  for (auto sec : sections) {
    sec->header->sh_offset = offset;
    sec->header->sh_size = sec->contents->size();
    offset += sec->header->sh_size;
  }
  return offset;
}
void program::to_file(std::string path) {
  size_t new_size = fix_offsets();

  std::vector<uint8_t> out(new_size, 0);
  std::memcpy(out.data(), header, sizeof(Elf64_Ehdr));
  for (size_t i = 0; i < sections.size(); i++) {
    auto sec = sections[i];
    Elf64_Shdr *sec_header = sec->header;
    std::copy((uint8_t *)sec_header, (uint8_t *)sec_header + sizeof(Elf64_Shdr),
              out.data() + header->e_shoff + i * sizeof(Elf64_Shdr));
  }
  for (auto sec : sections) {
    Elf64_Shdr *sec_header = sec->header;
    auto sec_data = sec->contents;
    std::copy(sec_data->begin(), sec_data->end(),
              out.data() + sec_header->sh_offset);
  }
  std::ofstream file(path, std::ios::binary | std::ios::trunc);
  std::copy(out.begin(), out.end(), std::ostreambuf_iterator<char>(file));

  file.close();
}

} // namespace arm_converter
