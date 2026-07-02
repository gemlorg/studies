## Use of AI

1. Used AI to set up the docker env, as well as the script that connects & runs gdb
2. Verified that AI solves the entire challenge in about 8 minutes
3. Writing my own exploit & explanation, and using ai to explain parts i didnt understand (e.g. i didnt realise at first that we effectively borrow 8 bytes from the next chunk, which means that when we overflow by 8 bytes we actually overflow the next chunk by 16)

## Thinking process

After looking at the source code, a few obvious observations can be made:

1. It suffices to overwrite `secretes.authenticated` with 0x1337 to pass the challenge
2. It also suffices to leak the bss section to pass the challenge
3. There is a buffer overflow at `read(0, notes[idx]->content, actual + 8);`
4. The address of `secrets.authenticated` is leaked with `printf("DEBUG: auth @ %p\n\n", &secrets.authenticated);`
5. The address of the note contents is leaked with `printf("DEBUG: Note %d created at %p\n", idx, notes[idx]->content);`

What requires most investigation is the buffer overflow. we're writing 8 bytes past the usable size. So we request note of size 1, recieving a 32 byte chunk (as it is the smallest one that can be allocated), with 16 bytes for the prev_size and size fields at the beginning, i.e. the usable space ends 8 bytes after the end of the chunk (the next chunk's prev_size).

```

1. Create note
2. Show note
3. Admin login
4. Exit
> $ 1
Size: $ 1
DEBUG: Allocating memory for note struct (16 bytes)
Content: $ AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
DEBUG: Note 0 created at 0x561c143bc030
```

then using pwngdb:

```
gef➤  x/8 notes[0]->content
0x561c143bc030: 0x4141414141414141      0x4141414141414141
0x561c143bc040: 0x4141414141414141      0x4141414141414141
0x561c143bc050: 0x0000000000000000      0x0000000000000000
0x561c143bc060: 0x0000000000000000      0x0000000000000000
gef➤  heap chunk  notes[0]->content
Chunk(addr=0x561c143bc030, size=0x20, flags=PREV_INUSE | IS_MMAPPED | NON_MAIN_ARENA)
Chunk size: 32 (0x20)
Usable size: 24 (0x18)
Previous chunk size: 0 (0x0)
PREV_INUSE | IS_MMAPPED | NON_MAIN_ARENA
```

which confirms that 16 bytes slip out of the chunk and overwrite the topchunk's size.

This is the situation that we looked at, at the labs. Overwriting the top chunk's size with an enormous value, we can allocate a huge chunk first, to move the the next allocation pointer to our target, which means that with the next alloction we will receive the target in writeable space.

This works because:
\[AI\] The glibc 2.28, which predates the two checks that normally kill this technique (introduced in versions 2.29 and 2.30). 2.28 still has one check — REQUEST_OUT_OF_RANGE, which rejects any request ≥ 0xffffffffffffffc0.

All info required to make the exploit work is leaked, as observed earlier: we know the address at which the top chunk is, because we know the address of the note (obervation 5), and we know the address of `secrets.authenticated` (observation 4).

## Exploit

So, we first overwrite the topchunk's size by creating a note of size 1, then filling it with 32 0xff bytes.

Next, create one more note to move the next allocation pointer to target:
lets say note 0 was allocated at ADDR0 (the addr shown in debug), and the address of the target is TARGET.
lets also name the actual size of note 1 chunk as NOTE1, and the requested size NOTE1_REQUEST_SIZE.

ADDR0 - begins note 0
\+ 0x10 - begins the next chunk
\+ 0x20 - note 1 struct
\+ NOTE1 - size of note 1
\+ 0x20 - note 2 struct
\+ 0x10 - header of note 2
equals TARGET

i.e.
ADDR0 + 0x30 + NOTE1 + 0x30 = TARGET
so
NOTE1 = TARGET - ADDR0 - 0x60

All that remains is invertin req2size, i.e.

NOTE1_REQUEST_SIZE = req2size^-1(TARGET - ADDR0 - 0x60)
What req2size does is add 8 bytes and align to be a multiple of 16. In our case inversion is just subtracting 8 bytes, because we happen to already be aligned to a multiple of 16. The result size comes out negative so we also have to look at it mod 2^64, to pass the REQUEST_OUT_OF_RANGE check.

i.e. NOTE1_REQUEST_SIZE =( TARGET - ADDR0 - 0x68) & 0xffffffffffffffff

after creating the second note, all we have to do is create one more note that writes p64(0x1337), and choose admin login.
