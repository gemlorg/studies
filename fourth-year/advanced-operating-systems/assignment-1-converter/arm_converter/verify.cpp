#include <arm_converter/arm_converter.hpp>
#include <filesystem>

namespace arm_converter {
void verify_args(char *arm_path, char *output_path) {
  std::filesystem::path arm_path_str(arm_path);
  std::filesystem::path output_path_str(output_path);
  // not empty
  if (arm_path_str.empty() || output_path_str.empty()) {
    throw std::runtime_error("Paths cannot be empty");
  }
  // check if the file exists
  if (!std::filesystem::exists(arm_path_str)) {
    throw std::runtime_error("Arm file does not exist");
  }
  return;
}

} // namespace arm_converter
