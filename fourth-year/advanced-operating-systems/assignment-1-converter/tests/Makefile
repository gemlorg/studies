.SECONDARY:

%.o: %.c
%.o: %.s
%: %.o

STEMS = data multiargs rodata recurse data-relocation loop multifunc condbranch zeroeg callee loadstore newloops
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

tests: $(addprefix test-,$(STEMS))  $(addsuffix -aarch64-x64.objdump,$(STEMS))
inputs: $(addsuffix -aarch64.o,$(STEMS)) $(addsuffix -aarch64.objdump,$(STEMS))

common.o: %.o: %.c
	$(X86_CC) $< -c -no-pie -o $@

test-%: %-x64.c %-aarch64-x64.o common.o
	$(X86_CC) $^ -no-pie -o $@

%-aarch64.o: %-aarch64.c
	$(AARCH_CC) $(AARCH_CFLAGS) -c $< -o $@

%-aarch64.objdump: %-aarch64.o
	$(AARCH_OBJDUMP) -afhprd $< > $@

%-aarch64-x64.o: converter %-aarch64.o
	./converter $(filter-out $<,$^) $@

%-aarch64-x64.objdump: %-aarch64-x64.o
	$(X86_OBJDUMP) -afhprd $< > $@

clean:
	rm -f *.o test-* *.objdump

.PHONY: all clean tests inputs
