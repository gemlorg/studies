TARGET = ./latc_x86_64
TEST_DIR = ./tests
.PHONY: build 

build:
	cabal install --installdir=. --overwrite-policy=always
	

.PHONY: test
test: 
	./test.sh $(TARGET)
.PHONY: test clean clean-target clean-tests

# Test target
test:
	./test.sh $(TARGET)

# Clean only the target files
clean-target:
	cabal clean
	rm -f ./latc_x86_64

# Clean only the test-related files
clean-tests:
	find $(TEST_DIR) -type f \( -name "*.ll" -o -name "*.bc" -o ! -name "*.*" \) -exec rm -f {} +

# General clean (invokes both clean-target and clean-tests)
clean: clean-target clean-tests
	 

