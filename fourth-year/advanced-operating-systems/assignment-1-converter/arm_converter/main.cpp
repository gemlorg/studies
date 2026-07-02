#include <arm_converter/arm_converter.hpp>
#include <exception>
#include <iostream>
using namespace arm_converter;

int main(int argc, char **argv) {

  if (argc != 3) {
    std::cerr << "Usage: " << argv[0] << " <arm path> <output path>"
              << std::endl;
    return 1;
  }
  try {
    verify_args(argv[1], argv[2]);
    program prog;
    prog.add_sections(argv[1]);
    prog.filter_sections(R"(\.note\.gnu\.property|.*\.eh_frame)");
    converter conv(&prog);
    conv.convert();
    prog.to_file(argv[2]);
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}
