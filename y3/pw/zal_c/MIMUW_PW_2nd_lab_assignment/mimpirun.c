/**
 * This file is for implementation of mimpirun program.
 * */

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
  //  printf("setenv_int: %s=%d\n", name, n);
  ASSERT_SYS_OK(snprintf(strn, 10, "%d", n));
  ASSERT_SYS_OK(setenv(name, strn, 1));
}

int child(char *prog, char **args, int n, int id) {

    // printf("child: MIMPI_TOTAL=%s\n", getenv(MIMPI_TOTAL));
  // printf("child: MIMPI_ID=%s\n", getenv(MIMPI_ID));
  // printf("PATH IS %s\n", getenv("PATH"));
  // execve("ls",  , environ);
  // setenv("PATH", getenv("PATH"), 1);
  ASSERT_SYS_OK(execve(prog, args, environ));
  return 0;
}

void create_channels(int n, int read_pipes[MAX_TOTAL][MAX_TOTAL],
                     int write_pipes[MAX_TOTAL][MAX_TOTAL]) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j)
        continue;

      int pipe_dsc[2];
      ASSERT_SYS_OK(channel(pipe_dsc));
      read_pipes[i][j] = pipe_dsc[0];
      write_pipes[i][j] = pipe_dsc[1];

    }
  }
}

void close_channels(int id, int n, int read_pipes[MAX_TOTAL][MAX_TOTAL],
                    int write_pipes[MAX_TOTAL][MAX_TOTAL]) {
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
void add_channels_to_env(int id, int n, int read_pipes[MAX_TOTAL][MAX_TOTAL],
                         int write_pipes[MAX_TOTAL][MAX_TOTAL]) {
                  

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
        char s1[ENOUGH_SPACE];
        
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
  int read_pipes[MAX_TOTAL][MAX_TOTAL];
  int write_pipes[MAX_TOTAL][MAX_TOTAL];
  if (argc < 3) {
    fprintf(stderr, "usage: %s <n> <prog> <args...>\n", argv[0]);
    return 1;
  }
  n = atoi(argv[1]);
  prog = argv[2];
  args = argv + 2;
  ASSERT_TRUE(n >= MIN_TOTAL && n <= MAX_TOTAL, EINVAL);
  
  // end check params
  //close first 1024 fds 
  // for (int i = 3; i < MAX_DESC; i++) {
  //     close(i);
  // }
    //open 20 file descriptors to make sure that channels have numbers >19 
    int fds[20];
    for (int i = 0; i < 20; i++) {
        fds[i] = open("/dev/null", O_RDONLY);
    }


  create_channels(n, read_pipes, write_pipes);

  //close 20 file descriptors
  for (int i = 0; i < 20; i++) {
      close(fds[i]);
  }

 //sync_pipe
  int c_pipe[2];
  int p_pipe[2];
  char c;
  ASSERT_SYS_OK(pipe(c_pipe));
  ASSERT_SYS_OK(pipe(p_pipe));

  // Create n children.
  for (int i = 0; i < n; ++i) {

    pid_t pid;
    ASSERT_SYS_OK(pid = fork());
    if (!pid) {
      
      close_channels(i, n, read_pipes, write_pipes);
      
      add_channels_to_env(i, n, read_pipes, write_pipes);
      
      setenv_int(MIMPI_TOTAL, n);
      setenv_int(MIMPI_ID, i);
      
      //notify parent that child is ready
      ASSERT_SYS_OK(close(p_pipe[0]));
      ASSERT_SYS_OK(close(p_pipe[1]));

      //we don't need that 
      ASSERT_SYS_OK(close(c_pipe[1]));

      //don't check because it's supposed to end with error
      read(c_pipe[0], &c, 1);
      ASSERT_SYS_OK(close(c_pipe[0]));
      
      return child(prog, args, n, i);
    }
  }
  // close all pipes
  close_channels(-1, n, read_pipes, write_pipes);

  //wait for children
  ASSERT_SYS_OK(close(p_pipe[1]));
  read(p_pipe[0], &c, 1);
  ASSERT_SYS_OK(close(p_pipe[0]));
  // printf("yall children are ready\n");
  //notify children
  ASSERT_SYS_OK(close(c_pipe[0]));
  ASSERT_SYS_OK(close(c_pipe[1]));


  // Wait for each child.
  for (int i = 0; i < n; ++i)
    ASSERT_SYS_OK(wait(NULL));

  return 0;
}
