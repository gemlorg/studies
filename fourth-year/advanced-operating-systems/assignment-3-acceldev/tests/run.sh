#!/bin/sh
TESTS="crossmap_contexts code_buffer_slot run_bad_descriptor run_nop run_fill mmap invalid_cmd overflow page_fault1 run_data_buffer too_many_buffers too_many_contexts run_unbound_buffer run_fill_pagefault too_big_buffer invalid_ioctl run_wrong_offset multiple_buffers_fill"
# TESTS="crossmap_contexts code_buffer_slot run_bad_descriptor run_nop run_fill mmap invalid_cmd overflow page_fault1 run_data_buffer wait_wakes_on_error too_many_buffers too_many_contexts run_unbound_buffer run_fill_pagefault multiple_buffers_fill contexts_do_not_interfere error_tied_to_context too_big_buffer run_wrong_offset invalid_ioctl"

for x in $TESTS; do
  echo === $x ===
  if ./$x; then
    echo OK
  else
    echo Failed
  fi
done
