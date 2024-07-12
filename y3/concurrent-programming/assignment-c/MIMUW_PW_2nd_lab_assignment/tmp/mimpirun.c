/**
 * This file is for implementation of mimpirun program.
 * */
#define _GNU_SOURCE
#include "channel.h"
#include "mimpi_common.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>


#include <sys/stat.h>


#define MAX_DESC 1024


extern char **environ;


void setenv_int(char *name, int n) {
   char strn[10];
  ASSERT_SYS_OK(snprintf(strn, 10, "%d", n));
  ASSERT_SYS_OK(setenv(name, strn, 1));
}

int child(char *prog, char **args, int n, int id) {
  ASSERT_SYS_OK(execvpe(prog, args, environ));
  return 0;
}

void create_channels(int n, int read_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL],
                     int write_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL]) {
  int next_largest_value = 1020;

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j)
        continue;

      int pipe_dsc[2];
      ASSERT_SYS_OK(channel(pipe_dsc));
      if(pipe_dsc[0] < 20) {
        ASSERT_SYS_OK(dup2(pipe_dsc[0], next_largest_value));
        ASSERT_SYS_OK(close(pipe_dsc[0]));
        pipe_dsc[0] = next_largest_value;
        next_largest_value--;
      }
      if(pipe_dsc[1] < 20) {
        ASSERT_SYS_OK(dup2(pipe_dsc[1], next_largest_value));
        ASSERT_SYS_OK(close(pipe_dsc[1]));
        pipe_dsc[1] = next_largest_value;
        next_largest_value--;
      }
      assert(pipe_dsc[0] >= 20);
      assert(pipe_dsc[1] >= 20);
      read_pipes[i][j] = pipe_dsc[0];
      write_pipes[i][j] = pipe_dsc[1];

    }
  }
}

void close_channels(int id, int n, int read_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL],
                    int write_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL]) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j)
        continue;
      if (i != id)
        ASSERT_SYS_OK(close(write_pipes[i][j]));
      if (j != id)
        ASSERT_SYS_OK(close(read_pipes[i][j]));
    }
  }
}
void add_channels_to_env(int id, int n, int read_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL],
                         int write_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL]) {
                  

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
        char s1[MIMPI_ENOUGH_SPACE];
        
      if (i == j)
        continue;
      if (i == id) {
        MIMPI_WRITE_FD(i,j, s1);
        setenv_int(s1, write_pipes[i][j]);
      }
      if (j == id) {
        MIMPI_READ_FD(i,j, s1);
        setenv_int(s1, read_pipes[i][j]);
      }
    }
  }

}

int main(int argc, char **argv) {
  int n;
  char *prog;
  char **args;
  int read_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL];
  int write_pipes[MIMPI_MAX_TOTAL][MIMPI_MAX_TOTAL];

  if (argc < 3) {
    fprintf(stderr, "usage: %s <n> <prog> <args...>\n", argv[0]);
    return 1;
  }
  n = atoi(argv[1]);
  prog = argv[2];
  args = argv + 2;
  ASSERT_TRUE(n >= MIMPI_MIN_TOTAL && n <= MIMPI_MAX_TOTAL, EINVAL);
  
  create_channels(n, read_pipes, write_pipes);


  // Create n children.
  for (int i = 0; i < n; ++i) {

    pid_t pid;
    ASSERT_SYS_OK(pid = fork());
    if (!pid) {
      
      close_channels(i, n, read_pipes, write_pipes);
      
      add_channels_to_env(i, n, read_pipes, write_pipes);
      
      setenv_int(MIMPI_TOTAL, n);
      setenv_int(MIMPI_ID, i);
      
      return child(prog, args, n, i);
    }
  }
  // close all pipes
  close_channels(-1, n, read_pipes, write_pipes);

  // Wait for each child.
  for (int i = 0; i < n; ++i)
    ASSERT_SYS_OK(wait(NULL));

  return 0;
}
