all: test

%.o: %.c
%.o: %.s
%: %.o

FIXED_REGS = x6 x7 x8 x11 x12 x13 x14 x15 x16 x17 x18 x24 x25 x26 x27 x28

X86_CC := x86_64-linux-gnu-gcc
X86_OBJDUMP := x86_64-linux-gnu-objdump

AARCH_CC := aarch64-linux-gnu-gcc
AARCH_OBJDUMP := aarch64-linux-gnu-objdump
# Always generate a frame pointer
AARCH_CFLAGS += -O0 -mno-omit-leaf-frame-pointer -fno-omit-frame-pointer
# Restrict register use to a subset
AARCH_CFLAGS += $(addprefix -ffixed-,$(FIXED_REGS))
# Don't generate any advanced relocations
AARCH_CFLAGS += -fno-pic -fno-pie -fno-plt -fno-jump-tables

test: %:  %.c test-aarch64-x64.o
	$(X86_CC) $^ -no-pie -o test

test-aarch64.o: %.o: %.c
	$(AARCH_CC) $(AARCH_CFLAGS) -c $< -o $@

test-aarch64-x64.o: test-aarch64.o
	./converter $< $@

clean:
	rm -f *.o test

.PHONY: all clean
