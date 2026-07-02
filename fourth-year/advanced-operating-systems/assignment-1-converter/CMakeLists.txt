cmake_minimum_required(VERSION 3.25)
project(converter_project LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)


# Find and link Capstone
find_package(PkgConfig REQUIRED)
pkg_check_modules(KEYSTONE REQUIRED keystone)
pkg_check_modules(CAPSTONE REQUIRED capstone)


file(GLOB SOURCES "arm_converter/*.cpp")
file(GLOB HEADERS "arm_converter/*.hpp")
# Add executable from main.cpp
add_executable(converter ${SOURCES} ${HEADERS})

# Include directories for Capstone and Keystone
target_include_directories(converter PRIVATE ${PROJECT_SOURCE_DIR} ${CAPSTONE_INCLUDE_DIRS} ${KEYSTONE_INCLUDE_DIRS} )

# Optional: Show found libs in output
message(STATUS "Capstone include dirs: ${CAPSTONE_INCLUDE_DIRS}")
message(STATUS "Keystone include dirs: ${KEYSTONE_INCLUDE_DIRS}")
# Link Capstone and Keystone libraries
target_link_libraries(converter PRIVATE ${CAPSTONE_LIBRARIES} ${KEYSTONE_LIBRARIES})

# Optional: Add compiler warnings
target_compile_options(converter PRIVATE -Wall -Wextra  -Wunused -Wconversion -pedantic  -g)

# Exclude tests folder (handled by its own Makefile)
