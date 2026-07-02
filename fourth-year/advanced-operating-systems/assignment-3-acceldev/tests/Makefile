TESTS=crossmap_contexts code_buffer_slot run_bad_descriptor run_nop run_fill mmap invalid_cmd overflow page_fault1 run_data_buffer wait_wakes_on_error too_many_buffers too_many_contexts run_unbound_buffer run_fill_pagefault multiple_buffers_fill contexts_do_not_interfere error_tied_to_context too_big_buffer run_wrong_offset invalid_ioctl
all: $(TESTS)

$(TESTS): %: %.o
	$(CC) -std=c11 -Wall -o $@ $< 

clean:
	rm -f $(TESTS) *.o

