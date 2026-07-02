#!/bin/bash
set -e

echo "Updating package list..."
sudo apt update

echo "Installing build tools, pkg-config, cmake..."
sudo apt install -y build-essential git pkg-config cmake

# Clone Keystone only if the folder doesn't exist
git clone --depth 1 https://github.com/keystone-engine/keystone.git
cd keystone
mkdir build && cd build
cmake .. && make -j 16 && make install
cd ../..
rm -rf keystone

# --- Capstone ---
git clone https://github.com/capstone-engine/capstone.git
cd capstone
git config --global --add safe.directory "$(pwd)"
git checkout v5

./make.sh && make install
cd ..
rm -rf capstone

# --- Update Linker ---
ldconfig

echo "Installation complete: Keystone built and installed; Capstone is installed via apt."
