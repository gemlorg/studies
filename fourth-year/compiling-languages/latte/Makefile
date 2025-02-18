TARGET = ./latc_llvm
TEST_DIR = ./tests
LIB_DIR = ./lib
LIB_TARGET = $(LIB_DIR)/runtime.ll

.PHONY: build 
build: lib
	cabal install --installdir=. --overwrite-policy=always

.PHONY: lib
lib:
	clang -S $(LIB_DIR)/runtime.c -o $(LIB_TARGET) -emit-llvm

.PHONY: test
test: 
	./test.sh $(TARGET)

.PHONY: test clean clean-target clean-tests

# Clean only the target files
clean-target:
	cabal clean
	rm -f $(TARGET)

# Clean only the test-related files
clean-tests:
	find $(TEST_DIR) -type f \( -name "*.ll" -name "*.bc" -name "*.s" -name "*.o" -o ! -name "*.*" \) -exec rm -f {} +
	find $(TEST_DIR) -type f \( -name "*.bc" \) -exec rm -f {} +

# Clean only the lib-related files
clean-lib:
	rm -f $(LIB_TARGET)

# General clean (invokes clean-target, clean-tests, and clean-lib)
clean: clean-target clean-tests clean-lib
