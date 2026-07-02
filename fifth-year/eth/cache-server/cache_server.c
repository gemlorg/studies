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
#include <stdbool.h>
#include <time.h>
#include <stddef.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <linux/seccomp.h>
#include <linux/filter.h>
#include <linux/audit.h>

#define MAX_KEYS 128
#define KEY_SIZE 32
#define VALUE_SIZE 90
#define CMD_BUF_SIZE 256
#define TOKEN_SIZE 64

typedef struct {
  char key[KEY_SIZE];
  char* value;
  bool active;
  time_t timestamp;
} cache_entry_t;

typedef struct {
  cache_entry_t entries[MAX_KEYS];
  size_t count;
  bool permissive_mode;
  size_t total_gets;
  size_t total_sets;
  size_t total_dels;
} cache_state_t;

cache_state_t cache = {
  .count = 0,
  .permissive_mode = false,
  .total_gets = 0,
  .total_sets = 0,
  .total_dels = 0
};


void print_banner() {
  printf("\n");
  printf("╔══════════════════════════════════════════════╗\n");
  printf("║      FastCache v1.2 - In-Memory Cache        ║\n");
  printf("║       Redis-like, but with more bugs         ║\n");
  printf("╚══════════════════════════════════════════════╝\n");
  printf("\n");
}

void print_help() {
  printf("\nAvailable Commands:\n");
  printf("  SET <key> <value>   - Store a key-value pair\n");
  printf("  GET <key>           - Retrieve value by key\n");
  printf("  DEL <key>           - Delete a key\n");
  printf("  KEYS                - List all keys\n");
  printf("  INFO                - Show cache statistics\n");
  printf("  CONFIG <file>       - Load configuration from file\n");
  printf("  FLUSH               - Clear all entries\n");
  printf("  HELP                - Show this help\n");
  printf("  QUIT                - Exit\n");
  printf("\n");
}

int find_entry(const char* key) {
  // searches all slots, not just active ones
  for (int i = 0; i < MAX_KEYS; i++) {
    if (strcmp(cache.entries[i].key, key) == 0) {
      return i;
    }
  }
  return -1;
}

int find_free_slot() {
  for (int i = 0; i < MAX_KEYS; i++) {
    if (!cache.entries[i].active) {
      return i;
    }
  }
  return -1;
}

void cmd_set(const char* key, const char* value) {
  if (strlen(key) == 0 || strlen(key) >= KEY_SIZE) {
    printf("ERR invalid key length\n");
    return;
  }

  if (strlen(value) >= VALUE_SIZE) {
    printf("ERR value too large (max %d bytes)\n", VALUE_SIZE - 1);
    return;
  }

  int idx = find_entry(key);

  // treat inactive (previously DEL'd) entries as not found, so a new slot is
  // allocated rather than silently doing nothing
  if (idx != -1 && !cache.entries[idx].active) {
    idx = -1;
  }

  if (idx != -1 && cache.entries[idx].active) {
    // update in place
    strncpy(cache.entries[idx].value, value, VALUE_SIZE - 1);
    cache.entries[idx].value[VALUE_SIZE - 1] = '\0';
    cache.entries[idx].timestamp = time(NULL);
    printf("OK updated\n");
  } else if (idx == -1) {
    idx = find_free_slot();
    if (idx == -1) {
      printf("ERR cache full\n");
      return;
    }

    // always malloc, even if reusing slot
    char* val = (char*)malloc(VALUE_SIZE);
    if (!val) {
      printf("ERR allocation failed\n");
      return;
    }

    strncpy(cache.entries[idx].key, key, KEY_SIZE - 1);
    cache.entries[idx].key[KEY_SIZE - 1] = '\0';

    strncpy(val, value, VALUE_SIZE - 1);
    val[VALUE_SIZE - 1] = '\0';

    cache.entries[idx].value = val;
    cache.entries[idx].active = true;
    cache.entries[idx].timestamp = time(NULL);
    cache.count++;

    printf("OK stored at %p\n", val);
  }

  cache.total_sets++;
}

void cmd_get(const char* key) {
  int idx = find_entry(key);

  if (idx == -1 || !cache.entries[idx].active) {
    printf("NIL\n");
    return;
  }

  printf("\"%s\" @ %p\n", cache.entries[idx].value, cache.entries[idx].value);
  cache.total_gets++;
}

void cmd_del(const char* key) {
  int idx = find_entry(key);

  if (idx == -1) {
    printf("(nil) key not found\n");
    return;
  }

  printf("DEL freeing %p\n", cache.entries[idx].value);

  free(cache.entries[idx].value);
  cache.entries[idx].active = false;

  cache.count--;

  printf("OK deleted\n");
  cache.total_dels++;
}

void cmd_keys() {
  printf("\n"  "Cached Keys (%ld):\n", cache.count);

  int count = 0;
  for (int i = 0; i < MAX_KEYS; i++) {
    if (cache.entries[i].active) {
      printf("  %3d. %s (value @ %p)\n",
        ++count, cache.entries[i].key, cache.entries[i].value);
    }
  }

  if (count == 0) {
    printf("  " "(empty)\n");
  }
  printf("\n");
}

void cmd_info() {
  printf("\n"  "Cache Statistics:\n");
  printf("════════════════════════════════════════════════\n");
  printf("  Entries:      %ld / %d\n", cache.count, MAX_KEYS);
  printf("  Value size:   %d bytes (fixed)\n", VALUE_SIZE);
  printf("  Permissive:   %s\n", cache.permissive_mode ? "ENABLED" : "DISABLED");
  printf("  Total SETs:   %lu\n", cache.total_sets);
  printf("  Total GETs:   %lu\n", cache.total_gets);
  printf("  Total DELs:   %lu\n", cache.total_dels);
  printf("════════════════════════════════════════════════\n");
  printf("\n");
}

void cmd_config(const char* filename) {
  // gated by permissive_mode flag (not settable via commands)
  if (!cache.permissive_mode) {
    printf("ERR config loading disabled (requires permissive mode)\n");
    return;
  }

  if (!filename || strlen(filename) == 0) {
    printf("ERR usage: CONFIG <file>\n");
    return;
  }

  FILE* fp = fopen(filename, "r");
  if (!fp) {
    printf("ERR could not open config file: %s\n", filename);
    return;
  }

  char first_line[512];
  if (!fgets(first_line, sizeof(first_line), fp)) {
    fclose(fp);
    printf("ERR config file is empty\n");
    return;
  }

  bool valid_header = (strncasecmp(first_line, "fastcache config", 16) == 0);

  if (!valid_header) {
    // in permissive mode, show file contents even if invalid
    printf("WARN error parsing config file, dumping contents (first 20 lines):\n\n");
    printf("════════════════════════════════════════════════\n");

    rewind(fp);
    char line[512];
    int line_num = 0;
    while (fgets(line, sizeof(line), fp) && line_num < 20) {
      line_num++;
      printf("%4d | %s", line_num, line);
    }

    fclose(fp);
    printf("════════════════════════════════════════════════\n");
    printf("Failed to parse configuration (invalid header)\n");
    printf("\n");
    return;
  }

  printf("Loading configuration from %s (first 20 lines)...\n\n", filename);
  printf("════════════════════════════════════════════════\n");

  printf("   1 | %s", first_line);

  char line[512];
  int line_num = 1;
  while (fgets(line, sizeof(line), fp) && line_num < 20) {
    line_num++;
    printf("%4d | %s", line_num, line);
  }

  fclose(fp);
  printf("════════════════════════════════════════════════\n");
  printf("Configuration loaded successfully!\n");
  printf("\n");
}

void cmd_flush() {
  int freed = 0;
  for (int i = 0; i < MAX_KEYS; i++) {
    if (cache.entries[i].active && cache.entries[i].value) {
      free(cache.entries[i].value);
      cache.entries[i].value = NULL;
      cache.entries[i].active = false;
      freed++;
    }
  }
  cache.count = 0;
  printf("OK flushed %d entries\n", freed);
}

void execute_command(char* ptr) {
  size_t len = strlen(ptr);
  if (len > 0 && ptr[len - 1] == '\n') {
    ptr[len - 1] = '\0';
  }

  if (strlen(ptr) == 0) {
    return;
  }

  char cmd[TOKEN_SIZE] = { 0 };
  char arg1[KEY_SIZE] = { 0 };
  char arg2[VALUE_SIZE] = { 0 };

  char* token = strtok(ptr, " ");
  if (token) {
    strncpy(cmd, token, TOKEN_SIZE - 1);

    token = strtok(NULL, " ");
    if (token) {
      strncpy(arg1, token, sizeof(arg1) - 1);

      // grab rest of line for value (allows spaces)
      token = strtok(NULL, "");
      if (token) {
        strncpy(arg2, token, sizeof(arg2) - 1);
      }
    }
  }

  for (int i = 0; cmd[i]; i++) {
    if (cmd[i] >= 'a' && cmd[i] <= 'z') {
      cmd[i] = cmd[i] - 'a' + 'A';
    }
  }

  if (strcmp(cmd, "SET") == 0) {
    if (strlen(arg1) == 0 || strlen(arg2) == 0) {
      printf("ERR usage: SET <key> <value>\n");
      return;
    }
    cmd_set(arg1, arg2);
  } else if (strcmp(cmd, "GET") == 0) {
    if (strlen(arg1) == 0) {
      printf("ERR usage: GET <key>\n");
      return;
    }
    cmd_get(arg1);
  } else if (strcmp(cmd, "DEL") == 0) {
    if (strlen(arg1) == 0) {
      printf("ERR usage: DEL <key>\n");
      return;
    }
    cmd_del(arg1);
  } else if (strcmp(cmd, "KEYS") == 0) {
    cmd_keys();
  } else if (strcmp(cmd, "INFO") == 0) {
    cmd_info();
  } else if (strcmp(cmd, "CONFIG") == 0) {
    cmd_config(arg1);
  } else if (strcmp(cmd, "FLUSH") == 0) {
    cmd_flush();
  } else if (strcmp(cmd, "HELP") == 0) {
    print_help();
  } else if (strcmp(cmd, "QUIT") == 0 || strcmp(cmd, "EXIT") == 0) {
    printf("Goodbye!\n");
    exit(0);
  } else {
    printf("ERR unknown command '%s'\n", cmd);
  }
}

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
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_writev, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_close, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_close_range, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_openat, 0, 5),
    BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, args[2])),
    BPF_JUMP(BPF_JMP | BPF_JSET | BPF_K, O_WRONLY | O_RDWR, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EACCES),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_lseek, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_pread64, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_dup, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_dup2, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_pipe2, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_ioctl, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_fcntl, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_fadvise64, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_fstat, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_newfstatat, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_statx, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getdents64, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getxattr, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_lgetxattr, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_listxattr, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_access, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_faccessat2, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_readlinkat, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getcwd, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_statfs, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_brk, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_mmap, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_mprotect, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_munmap, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_vfork, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_clone, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_clone3, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_execve, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_wait4, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_exit, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_exit_group, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_rt_sigaction, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_rt_sigprocmask, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_rt_sigreturn, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_sigaltstack, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_arch_prctl, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_set_tid_address, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_set_robust_list, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_rseq, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_futex, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_prlimit64, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getrandom, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_uname, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getuid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_geteuid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getgid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getegid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getpid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getppid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getpgrp, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_setsid, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_getgroups, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_socket, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_connect, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_sendto, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_recvmsg, 0, 1),
    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
  };

  struct sock_fprog prog = {
    .len = (unsigned short)(sizeof(filter) / sizeof(filter[0])),
    .filter = filter,
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

int main(int argc, char* argv[]) {
  char cmd_buf[CMD_BUF_SIZE] = { 0 };

  // disable buffering for interactive use
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stdin, NULL, _IONBF, 0);

  print_banner();
  printf("Type HELP for available commands\n");

  memset(&cache, 0, sizeof(cache));

  setup_seccomp();

  while (1) {
    printf("cache> ");

    if (!fgets(cmd_buf, sizeof(cmd_buf), stdin)) {
      break;
    }

    execute_command(cmd_buf);
  }

  printf("\n");
  return 0;
}
