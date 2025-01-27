#!/usr/bin/env bash
# source: https://github.com/mikimowski/Latte-Compiler

shopt -s extglob
bad_tests_dir1="tests/examples/bad/*"
bad_tests_dir2="tests/lattests/bad/*"
bad_tests_dir3="tests/mrjp-tests/bad/*/*"
good_tests_dir1="tests/examples/good/*/*"
good_tests_dir2="tests/lattests/good/*"
good_tests_dir3="tests/mrjp-tests/good/basic/*"
good_tests_dir5="tests/extensions/*"

extension_tests_dir1="tests/lattests/extensions/*/*"


program_path=$1

NC='\033[0m' # No Color
RED='\033[0;31m'
GREEN='\033[0;32m'

total_tests=0
total_ok=0
total_bad=0
compiled_cnt=0
bad_compilation_failure=0
compiled_ok=0
runtime_ok=0

function generate {
  for file in $1; do
    if [[ "$file" == *".lat" ]]; then
      ((total_tests++))
      echo "$file"
      ./"$program_path" $file
      exit_code="$?"

      if [ $2 -eq 1 ]; then
        ((total_bad++))
      else
        ((total_ok++))
      fi

      if [ $exit_code -eq $2 ]; then
        ((compiled_cnt++))
        if [ $2 -eq 1 ]; then
          ((bad_compilation_failure++))
        else
          ((compiled_ok++))
        fi
        echo -e "${GREEN}OK${NC}"
      else
        echo -e "${RED}TEST FAILED FOR ${file}${NC}"
      fi
    fi
  done
}

function run_tests_in {
  for file in $1; do
    if [[ "$file" == *".lat" ]]; then
      outfile=${file%.lat}".output"
      compiled_file=${file%.lat}".s"
      binary_file=${file%.lat}".o"
      exec_file=${file%.lat}
      echo "$file"

      if [ $(ls -l ${file%.lat}".output" 2>/dev/null | wc -l) == 1 ]; then
        if [ $(ls -l ${file%.lat}".input" 2>/dev/null | wc -l) == 1 ]; then
          DIFF=$(diff <($exec_file <${file%.lat}".input") $outfile)
          if [ "$DIFF" != "" ]; then
            echo -e "${RED}TEST{1} FAILED FOR ${file}${NC}"
          else
            diff <(cat ${file%.lat}".input" | $exec_file) <(cat $outfile)
            if [ "$?" -eq $2 ]; then
              ((runtime_ok++))
              echo -e "${GREEN}OK${NC}"
            else
              echo -e "${RED}TEST{2} FAILED FOR ${file}${NC}"
            fi
          fi
        else
          DIFF=$(diff <($exec_file) $outfile)
          if [ "$DIFF" != "" ]; then
            echo -e "${RED}TEST{3} FAILED FOR ${file}${NC}"
          else
            diff <($exec_file) $outfile
            if [ "$?" -eq $2 ]; then
              ((runtime_ok++))
              echo -e "${GREEN}OK${NC}"
            else
              echo -e "${RED}TEST{4} FAILED FOR ${file}${NC}"
            fi
          fi
        fi
      else
        if [ $(ls -l ${file%.lat}".input" 2>/dev/null | wc -l) == 1 ]; then
          $exec_file <${file%.lat}".input"
        else
          $exec_file
        fi

        if [ "$?" -eq $2 ]; then
          ((runtime_ok++))
          echo -e "${GREEN}OK${NC}"
        else
          echo -e "${RED}TEST FAILED FOR ${file}${NC}"
        fi
      fi
    fi
  done
}

generate "$bad_tests_dir1" 1
generate "$bad_tests_dir2" 1
generate "$bad_tests_dir3" 1
generate "$good_tests_dir1" 0
generate "$good_tests_dir2" 0
generate "$good_tests_dir3" 0
generate "$good_tests_dir5" 0
generate "$extension_tests_dir1" 0

run_tests_in "$good_tests_dir1" 0
run_tests_in "$good_tests_dir2" 0
run_tests_in "$good_tests_dir3" 0
run_tests_in "$good_tests_dir5" 0
run_tests_in "$extension_tests_dir1" 0

echo "total tests: $total_tests"
echo "total ok: $total_ok"
echo "total bad: $total_bad"
echo "compiled with expected failure/success: $compiled_cnt"
echo "passed bad (failed during compilation as expected): $bad_compilation_failure"
echo "passed ok compilation: $compiled_ok"
echo "passed ok runtime: $runtime_ok"
