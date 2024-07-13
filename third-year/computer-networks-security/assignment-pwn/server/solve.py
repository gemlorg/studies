#!/usr/bin/env python3
from pwn import *

exe = ELF("./easy")
#exe = ELF("./medium")
# hard chall is dynamically linked, so here's helper
# patched version to load proper ld and libc
#exe = ELF("./hard_patched")
libc = ELF("./libc.so.6")
ld = ELF("./ld-linux-x86-64.so.2")

context.binary = exe
index_number = b"8"


def conn():
    r = process([exe.path, index_number])
    #gdb.attach(r)
    #r = remote("bsk.bonus.re", 13337)
    return r


def main():
    r = conn()
    # good luck!
    r.interactive()


if __name__ == "__main__":
    main()
