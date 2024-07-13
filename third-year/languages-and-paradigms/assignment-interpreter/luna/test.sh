#!/bin/bash

# Define colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Define the directory paths
good_examples_dir="./examples/good"
bad_examples_dir="./examples/bad"

# Function to run interpret and assert output for good files
interpret_and_assert_good() {
	file="$1"
	output=$(./interpreter "$file" 2>&1)
	echo $file
	echo $output
	if [[ $output =~ ^error ]]; then
		echo -e "${RED}ERROR in $file${NC}"
		return 1
	fi
}

# Function to run interpret and assert output for bad files
interpret_and_assert_bad() {
	file="$1"
	output=$(./interpreter "$file" 2>&1)
	echo $file
	echo $output

	if ! [[ $output =~ ^error: || $output =~ "BNFC Parse error:" ]]; then
		echo -e "${RED}NO ERROR in $file${NC}"
		return 1
	fi
}

# Process good examples
echo "Processing good examples..."
good_test_passed=true
for file in "$good_examples_dir"/*; do
	if [ -f "$file" ]; then
		interpret_and_assert_good "$file"
		if [ $? -eq 1 ]; then
			good_test_passed=false
		fi
	fi
done

if $good_test_passed; then
	echo -e "${GREEN}Test successful!${NC}"
else
	echo -e "${RED}Test failed.${NC}"
fi

# Process bad examples
echo "Processing bad examples..."
bad_test_passed=true
for file in "$bad_examples_dir"/*; do
	if [ -f "$file" ]; then
		interpret_and_assert_bad "$file"
		if [ $? -eq 1 ]; then
			bad_test_passed=false
		fi
	fi
done

# Print test results
if $bad_test_passed; then
	echo -e "${GREEN}Test successful!${NC}"
else
	echo -e "${RED}Test failed.${NC}"
fi
