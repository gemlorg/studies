/**
 * This file is for declarations of  common interfaces used in both
 * MIMPI library (mimpi.c) and mimpirun program (mimpirun.c).
 * */

#ifndef MIMPI_COMMON_H
#define MIMPI_COMMON_H

#include <assert.h>
#include <stdbool.h>
#include <stdnoreturn.h>


/*
    Assert that expression doesn't evaluate to -1 (as almost every system
   function does in case of error).

    Use as a function, with a semicolon, like: ASSERT_SYS_OK(close(fd));
    (This is implemented with a 'do { ... } while(0)' block so that it can be
   used between if () and else.)
*/
#define ASSERT_SYS_OK(expr)                                                    \
  do {                                                                         \
    if ((expr) == -1)                                                          \
      syserr("system command failed: %s\n\tIn function %s() in %s line "       \
             "%d.\n\tErrno: ",                                                 \
             #expr, __func__, __FILE__, __LINE__);                             \
  } while (0)

/* Assert that expression evaluates to zero (otherwise use result as error
 * number, as in pthreads). */
#define ASSERT_ZERO(expr)                                                      \
  do {                                                                         \
    int const _errno = (expr);                                                 \
    if (_errno != 0)                                                           \
      syserr("Failed: %s\n\tIn function %s() in %s line %d.\n\tErrno: ",       \
             #expr, __func__, __FILE__, __LINE__);                             \
  } while (0)

/* Prints with information about system error (errno) and quits. */
_Noreturn extern void syserr(const char *fmt, ...);

/* Prints (like printf) and quits. */
_Noreturn extern void fatal(const char *fmt, ...);

#define TODO fatal("UNIMPLEMENTED function %s", __PRETTY_FUNCTION__);

/////////////////////////////////////////////
// Put your declarations here

/* Assert that expression evaluates to zero (otherwise use result as error
 * number, as in pthreads). */
#define ASSERT_TRUE(expr, code)                                                \
  do {                                                                         \
    int const _errno = (expr);                                                 \
    if (_errno == 0) {                                                         \
      errno = (code);                                                          \
      syserr("Failed: %s\n\tIn function %s() in %s line %d.\n\tErrno: ",       \
             #expr, __func__, __FILE__, __LINE__);                             \
    }                                                                          \
  } while (0)

#define MIMPI_TOTAL "MIMPI_TOTAL"
#define MIMPI_ID "MIMPI_ID"
#define MIMPI_MAX_TOTAL 16
#define MIMPI_MIN_TOTAL 1
#define MIMPI_READ "MIMPI_READ_FD"
#define MIMPI_WRITE "MIMPI_WRITE_FD"
#define MIMPI_ENOUGH_SPACE 38

// void mimpi_read_fd(int i, int j, char *ij) {
//   ASSERT_SYS_OK(sprintf(ij, "%s_%d_%d", MIMPI_READ_FD, i, j));
// }

#define MIMPI_READ_FD(i, j, ij) ASSERT_SYS_OK(sprintf(ij, "%s_%d_%d", MIMPI_READ, i, j));
#define MIMPI_WRITE_FD(i, j, ij) ASSERT_SYS_OK(sprintf(ij, "%s_%d_%d", MIMPI_WRITE, i, j));







#endif // MIMPI_COMMON_H
