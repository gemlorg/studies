# Compiler and flags
CC = gcc
CFLAGS = -Wall -g -fstack-protector-all
SEC_LDFLAGS := -Wl,-z,relro,-z,now

# Relative path from current directory
GLIBC_DIR_REL := ../glibc/glibc_2.28_no-tcache
# Absolute path for use in binaries
GLIBC_DIR_ABS := $(abspath $(GLIBC_DIR_REL))

# Explicit loader and libc from the custom glibc root
GLIBC_LD   := $(GLIBC_DIR_ABS)/ld.so.2
GLIBC_LIBC := $(GLIBC_DIR_ABS)/libc.so.6

# Fail fast if either file is missing
ifeq ($(wildcard $(GLIBC_LD)),)
$(error Missing dynamic loader: $(GLIBC_LD))
endif
ifeq ($(wildcard $(GLIBC_LIBC)),)
$(error Missing libc: $(GLIBC_LIBC))
endif

# Link against GLIBC_LIBC explicitly and bake in the custom loader
LDFLAGS := $(SEC_LDFLAGS) \
  -Wl,--dynamic-linker=$(GLIBC_LD) \
  $(GLIBC_LIBC) \
  -Wl,-rpath,$(GLIBC_DIR_ABS)

# Find all .c files in subdirectories
SOURCES := $(wildcard *.c)

# Generate target names (basename without .c extension)
TARGETS := $(SOURCES:.c=)

# Pattern rule for building any .c file into an executable
%: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)
	patchelf --set-interpreter "$(GLIBC_LD)" \
	         --set-rpath "$(GLIBC_DIR_ABS)" \
	         $@

all: $(TARGETS)

# Clean
clean:
	rm -f $(TARGETS)

# Verify security settings and interpreter via patchelf
check: $(TARGETS)
	@for target in $(TARGETS); do \
		echo "Checking $$target:"; \
		checksec --file=$$target; \
		echo "\nLinked libraries:"; \
		ldd $$target; \
		echo "\nPT_INTERP (patchelf):" && patchelf --print-interpreter $$target; \
		echo "\n---\n"; \
	done

.PHONY: all clean check
