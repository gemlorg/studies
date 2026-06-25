/*
 * MIT License
 *
 * Copyright (c) 2025 Davide Guerri <davide.guerri@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <malloc.h>
#include <sys/prctl.h>
#include <linux/seccomp.h>
#include <linux/filter.h>
#include <linux/audit.h>
#include <sys/syscall.h>
#include <errno.h>

#define MAX_NOTES 3
#define FLAG_MAX_LEN 2048

typedef struct {
    char win_flag[FLAG_MAX_LEN];
    long unsigned int authenticated;
} secrets_t;

secrets_t secrets = { .authenticated = 0, .win_flag = "" };

typedef struct {
    char* content;
    size_t size;
} note_t;

note_t* notes[MAX_NOTES];


void setup_seccomp() {
    struct sock_filter filter[] = {
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, arch)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, AUDIT_ARCH_X86_64, 1, 0),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_read, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_write, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_close, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_fstat, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_mmap, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_mprotect, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_munmap, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_brk, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_exit_group, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_newfstatat, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getrandom, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
    };

    struct sock_fprog prog = {
        .len = sizeof(filter) / sizeof(filter[0]),
        .filter = filter
    };

    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) {
        perror("prctl(NO_NEW_PRIVS)");
        exit(1);
    }

    if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &prog) != 0) {
        perror("prctl(SECCOMP)");
        exit(1);
    }
}

size_t read_number() {
    char buffer[32];
    memset(buffer, 0, sizeof(buffer));

    if (read(0, buffer, sizeof(buffer) - 1) <= 0) {
        exit(0);
    }

    return strtoul(buffer, NULL, 0);
}

void create_note() {
    int idx = -1;
    for (int i = 0; i < MAX_NOTES; i++) {
        if (!notes[i]) {
            idx = i;
            break;
        }
    }

    if (idx == -1) {
        puts("No more space for notes!");
        return;
    }

    printf("Size: ");
    size_t size = read_number();

    printf("DEBUG: Allocating memory for note struct (%ld bytes)\n", sizeof(note_t));
    notes[idx] = malloc(sizeof(note_t));
    notes[idx]->size = size;
    notes[idx]->content = malloc(size);

    printf("Content: ");
    size_t actual = malloc_usable_size(notes[idx]->content);
    read(0, notes[idx]->content, actual + 8);

    printf("DEBUG: Note %d created at %p\n", idx, notes[idx]->content);
}

void show_note() {
    printf("Index: ");
    size_t raw_idx = read_number();

    if (raw_idx >= MAX_NOTES || !notes[raw_idx]) {
        puts("Invalid note!");
        return;
    }

    int idx = (int)raw_idx;
    printf("Content: %s\n", notes[idx]->content);
}

void admin_login() {
    if (secrets.authenticated == 0x1337) {
        puts("\n*** ADMIN ACCESS GRANTED ***");
        printf("Secret: %s\n", secrets.win_flag);
    } else {
        printf("Access denied (auth = 0x%lx)\n", secrets.authenticated);
    }
}

int main() {
    setvbuf(stdout, NULL, _IONBF, 0);

    // Read flag from file before seccomp is applied (open/openat are blocked after)
    {
        int fd = open("/home/ctf/flag.txt", O_RDONLY);
        if (fd < 0) {
            // Fall back to a placeholder during development
            strncpy(secrets.win_flag, "FLAG{placeholder_set_flag_file}", FLAG_MAX_LEN - 1);
            secrets.win_flag[FLAG_MAX_LEN - 1] = '\0';
        } else {
            ssize_t n = read(fd, secrets.win_flag, FLAG_MAX_LEN - 1);
            if (n < 0) n = 0;
            // Strip trailing newline if present
            if (n > 0 && secrets.win_flag[n - 1] == '\n') n--;
            secrets.win_flag[n] = '\0';
            close(fd);
        }
    }

    puts("\n=== Simple Note System ===");

    printf("DEBUG: auth @ %p\n\n", &secrets.authenticated);

    // Enable seccomp after all initialization is done
    setup_seccomp();

    while (1) {
        puts("\n1. Create note");
        puts("2. Show note");
        puts("3. Admin login");
        puts("4. Exit");
        printf("> ");

        int choice = read_number();

        switch (choice) {
        case 1:
            create_note();
            break;
        case 2:
            show_note();
            break;
        case 3:
            admin_login();
            break;
        case 4:
            exit(0);
        default:
            puts("Invalid option");
        }
    }
}