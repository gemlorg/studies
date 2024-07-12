#!/usr/bin/env python3
from pwn import *
from utils import *
from random import randbytes
import hashlib
import sys



def conn():

    # r = remote( "cryptotask2023.tailcall.net", 30007)
    # r = remote( "localhost", 13371)
    r = remote(sys.argv[1], int(sys.argv[2]))

    return r



choice = b"2"

#flag{still-not-a-csprng}
def flag1(r):

        # class NcgPrng:
    #     def __init__(self):
    #         self.state = random.randint(0, 2**64-1)
    #         self.a = random.randint(0, 2**64-1)
    #         self.c = random.randint(0, 2**64-1)
    #         self.m = 2**64

    #     def next(self):
    #         """Gets a next 64bit random number."""
    #         self.state = (self.state * self.a ^ self.c) % self.m
    #         return self.state

    l = list()
    r.recvuntil(b"> ")
    for i in range(5):
        l.append(int(r.recvline().strip()))
    a = int(0)
    c = int(0)
    m = 2**64 
    guesses = list()
    bit = 0
    while bit < 64:
        found_guess = False
        for a_bit in range(2):
            for c_bit in range(2):
                is_correct = True
                a_guess = (a & ~(1 << bit))|(a_bit << bit)
                c_guess = (c & ~(1 << bit))|(c_bit << bit)

                for i in range(4):
                    res = (l[i] * (a_guess)) ^ (c_guess )
                    if res % (2**(bit+1)) != l[i+1] % (2**(bit+1)):
                        is_correct = False
                        break
                if is_correct:
                    if(found_guess):
                        # print("Multiple guesses found for bit", bit)
                        guesses.append((a_guess, c_guess, bit))
                    else:
                        found_guess = True
                        a = a_guess
                        c = c_guess
        if not found_guess:
            print("No guess found for bit", bit, "number of existing guesses ", len(guesses))
            if(len(guesses) == 0):
                print("No guesses found, exiting")
                for i in range(5):
                    print("l: "," {0:b}".format(l[i])[::-1])
                print("a: "," {0:b}".format(a)[::-1])
                print("c: "," {0:b}".format(c)[::-1])
                print("bit: ", bit)
                exit(1)
            first_guess = guesses.pop(0)
            a = first_guess[0]
            c = first_guess[1]
            bit = first_guess[2]
        bit += 1
    final = (l[4] * a ^ c )% m
    
    r.sendline(str(final).encode())
    r.recvline()
    print(b"FLAG 1 is " + r.recvline().strip())


def get_hex_line(r): return bytes.fromhex(r.recvline().decode("utf-8"))
def msg_to_str(msg): return bytes.fromhex(msg.decode("utf-8"))

def get_command(hashed_hello, msg):
        orig_iv = hashed_hello[:16]
        orig_cipher = hashed_hello[16:]
        orig_plain = pad(b'Hello')
        wanted_plain = pad(msg)
        #dec(orig_cipher) \op orig_iv \op orig_plain \op wanted_plain = orig_plain \op orig_plain \op wanted_plain = wanted_plain
        #                     --------------------------------------- new iv
        new_iv = xor( orig_iv, xor(orig_plain, wanted_plain))
        return new_iv + orig_cipher

# get command without msg padding
def get_text(hashed_hello, msg):
        assert len(msg) <= 16
        orig_iv = hashed_hello[:16]
        orig_cipher = hashed_hello[16:]
        orig_plain = pad(b'Hello')
        wanted_plain = msg
        #dec(orig_cipher) \op orig_iv \op orig_plain \op wanted_plain = orig_plain \op orig_plain \op wanted_plain = wanted_plain
        #                     --------------------------------------- new iv
        new_iv = xor( orig_iv, xor(orig_plain, wanted_plain))
        return new_iv + orig_cipher

def get_new_hello(r, old_hello):
    seq = randbytes(9)
    seq_hash = hashlib.sha256(seq).digest()
    cmd = get_command(old_hello, b"hash?"+seq)
    r.sendline(cmd.hex().encode())
    h = get_hex_line(r)
    want = pad(b'Hello')
    new_iv = xor(want, xor(h[:16], seq_hash[:16]))
    return new_iv + h[16:32]


def xor_bytes(a, n, b):
    return a[:n] + xor(a[n:n+len(b)], b) + a[n+len(b):]

def get_hash_of(r, hashed_hello, msg):
    l = len(msg)
    assert l <= 10 
    payload = get_command(hashed_hello, b" " * (10 - l) + b"hash?" + msg + b"\x01")
    r.sendline(payload.hex().encode())
    return get_hex_line(r)


def hash_password_part(hashed_hello, enc_flag, r, n):


    enc_flag_juice = enc_flag[16:]
    pload = get_text(hashed_hello, b" " * 11 + b"hash?") + enc_flag_juice + get_text(hashed_hello, b" "*15 +  chr(32 + len(enc_flag_juice)-n).encode())

    r.sendline(pload.hex().encode())
    return get_hex_line(r)



# flag{sh0rt-fl4G}
#flag{p4dding-1s-h4rd-but-re4ly-just-s1gn-y0ur-c1phert3xts}
def flag3(r, flag_name = b"FLAG!"):
    r.recvuntil(b"> ")
    encoded_hello = bytes.fromhex(r.recvline().decode("utf-8"))

    r.sendline(get_command(encoded_hello, flag_name).hex().encode())
    encoded_flag = get_hex_line(r)
    FULL_FLAG = b""
    is_end = False

    while len(encoded_flag) > 16 and not is_end:
        while True:
            decrypted_flag = b''
            encoded_hello = get_new_hello(r, encoded_hello)
            is_ok = True
            
            #we wil get first four bytes of the flag

            print("looking for first 4 bytes of the block")
            #in the notes i said we only need only 4 bytes but it sometimes fails, so let's just take 5 instead
            for i in range (1,6):
                hp = hash_password_part(encoded_hello, encoded_flag, r, i)
                for j in range(256):

                    if j in [0x27, 0x22, 0x3f, 0x5c, 0x07,0x8,0x9,0xa, 0xb, 0xc, 0xd, 20, 0]: 
                        continue 

                    hb = get_hash_of(r, encoded_hello, decrypted_flag+bytes([j]))
                    if hb == hp:
                        decrypted_flag += bytes([j])
                        print(decrypted_flag)
                        break
                    if j == 255:
                        print("unlucky xor, trying again")
                        #about 10characters are s.c. special sequences, so if the match is not found, the flag contains them
                        # probability of that event is 1-(245/255)^5 = 0.181291084692
                        #, meaning that probability of the hash being good is about 82%. Hence we expect that we will have to try about 1/0.82 = 1.12 times
                        #which means that we can try again if no matches are found, and the amount of requests we'll have to send is reasonable

                        #try again with a different encoded_hello(use hash for that). 
                        is_ok = False
                        break
                if not is_ok: break

            if is_ok: break



        # what we have is: dec(enc_flag) \op get_text(hashed_hello, b" " * 11 + b"hash?")[16:] 
        # what we need is: dec(enc_flag) \op initial_iv 
        thexor = xor(encoded_flag[:16], encoded_hello[16:])[:len(decrypted_flag)]
        # print("flag enc is ", encoded_flag )
        flag_beginning = xor(thexor,decrypted_flag)


        print("block beginning afte stage 1 is ", flag_beginning)

        #flag len(flag_beginning) == 16 implies we got the whole flag
        is_ok = True
        len_beg = len(flag_beginning)
        print("deciphering the rest of the block")
        while len(flag_beginning) < 16:
            if(len_beg != len(flag_beginning)):
                break
            len_beg+= 1

            for i in range(255):
                seq = flag_beginning + bytes([i])
                need = b"\x20"*(len(seq)-5) + b"FLAG!"
                fiv = xor(need, xor(encoded_flag[:len(seq)], seq)) + encoded_flag[len(seq):16]
                fc = encoded_flag[16:]
                r.sendline((fiv+fc).hex().encode())
                answer = get_hex_line(r)
                if len(answer) > 33:
                    flag_beginning = seq 
                    print("block beginning is  ", flag_beginning)
                    if bytes([i]) == b"}":
                        is_ok = False
                        is_end = True
                    break

            if not is_ok: break

        FULL_FLAG += flag_beginning
        encoded_flag = encoded_flag[16:]
        print("total for now: ", FULL_FLAG)
    print("ANSWER FOR ", flag_name, ": ", FULL_FLAG)

def skip_print():
    for _ in range(10):
        print("-----------------")

def main():
    print("Looking for flag 1")
    r = conn()
    r.sendline(b"1")
    flag1(r)
    r.close()
    skip_print() 
    print("Looking for flag 2")
    r = conn()
    r.sendline(b"2")
    flag3(r, b"flag?")
    r.close()
    skip_print()
    print("Looking for flag 3")
    r = conn()
    r.sendline(b"2")
    flag3(r, b"FLAG!")
    r.close()


if __name__ == "__main__":
    main()
