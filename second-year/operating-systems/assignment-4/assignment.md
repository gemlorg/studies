# Exclusive File Locking by Users

The goal of this task is to extend the VFS server with a mechanism that allows users to exclusively lock access to selected files. Unlike standard flock locks, this mechanism will be mandatory and will operate at the user level, not the process level. Unlike standard file access permissions, this mechanism will implement temporary locking that does not require changes to file attributes.

## VFS

VFS (Virtual File System) is a subsystem of the operating system that enables uniform access to files located on different file systems. It acts as an intermediary layer between applications and subsystems implementing specific file systems (MFS, ext2, procfs, etc.). It processes system calls that perform file operations, implements actions common to various file systems, and forwards requests to the appropriate file systems. It also manages all the files used in the system and all mounted file systems.

In MINIX, the virtual file system is implemented as the `vfs` server. More about its structure and operation can be read on the MINIX Wiki: [VFS internals](https://wiki.minix3.org/doku.php?id=developersguide:vfs).

## System Calls `VFS_FEXCLUSIVE` and `VFS_EXCLUSIVE`

The file locking mechanism is based on new system calls `VFS_FEXCLUSIVE` and `VFS_EXCLUSIVE` handled by the `vfs` server. Using these, a user can temporarily block other users from performing the following actions on the specified file: opening the file (`VFS_OPEN` and `VFS_CREAT`), reading (`VFS_READ`), writing (`VFS_WRITE`), truncating (`VFS_TRUNCATE` and `VFS_FTRUNCATE`), moving and renaming (`VFS_RENAME`, whether the locked file is the first or second argument), and deleting the file (`VFS_UNLINK`). The user who locked the file can perform these operations without restrictions from different processes. However, attempts by another user to perform them should result in an `EACCES` error.

The arguments of the `VFS_FEXCLUSIVE` system call are a file descriptor and a flag indicating the action being performed. The supported actions are:

- `EXCL_LOCK`: Locks the file indicated by the descriptor exclusively for the user calling this system call. If the file is not explicitly unlocked by the user, it will be automatically unlocked when the descriptor that was the argument of the locking call is closed.
- `EXCL_LOCK_NO_OTHERS`: Works like `EXCL_LOCK`, but the file is locked exclusively if it is not currently open by another user (the locking user can have this file open any number of times). Otherwise, the call ends with an `EAGAIN` error.
- `EXCL_UNLOCK`: Unlocks the file indicated by the descriptor. The file can only be unlocked by the user who locked it.
- `EXCL_UNLOCK_FORCE`: Unlocks the file indicated by the descriptor. The file can be unlocked by the user who locked it, the user who owns the file, or the superuser (root, user with UID = 0).

The arguments of the `VFS_EXCLUSIVE` system call are a file path and a flag indicating the action being performed. The supported actions are:

- `EXCL_LOCK`: Locks the file indicated by the given path exclusively for the user calling this system call. The file remains locked until explicitly unlocked by the user. The exception to this rule is when the locked file is deleted by the user (using the `VFS_UNLINK` system call) or replaced by another file (the locked file is the second argument of `VFS_RENAME`). In this case, the file will be automatically unlocked when it is no longer used by any user (no process has the file open).
- `EXCL_LOCK_NO_OTHERS`: Works like `EXCL_LOCK`, but the file is locked exclusively if it is not currently open by another user (the locking user can have this file open any number of times). Otherwise, the call ends with an `EAGAIN` error.
- `EXCL_UNLOCK`: Unlocks the file indicated by the given path. The file can only be unlocked by the user who locked it.
- `EXCL_UNLOCK_FORCE`: Unlocks the file indicated by the given path. The file can be unlocked by the user who locked it, the user who owns the file, or the superuser (root, user with UID = 0).

## File Locking Mechanism Specifications

- Only regular files are locked. Attempts to lock a directory, pseudo-device, pipe, fifo, etc., end with an `EFTYPE` error.
- A specific file indicated by the descriptor or path at the time of locking or unlocking is locked or unlocked. The file does not cease to be locked due to subsequent moves (within the partition), renames, or references through a link. Technically, a specific v-node or i-node is locked.
- File locks are not retained after unmounting the file system. The presence of locked files does not prevent the unmounting of the file system.
- The lock is in effect from the moment the file is locked until it is unlocked. The system calls listed above check file access permission at each call. Thus, a situation is possible where a user successfully opens a file, and then another user locks it, so the next operation by the first user (e.g., reading the file) results in an `EACCES` error. When the second user unlocks the file, the first user's subsequent operation (e.g., retrying to read the file) succeeds.
- The user is identified by their real UID, regardless of the effective UID.
- Using `VFS_FEXCLUSIVE`, a file can only be locked if the given descriptor is open for reading or writing. Otherwise, the system call ends with an `EBADF` error. Using `VFS_EXCLUSIVE`, a file can only be locked if the calling user has read or write permissions for the file indicated by the given path. Otherwise, the system call ends with an `EACCES` error.
- If `VFS_FEXCLUSIVE` and `VFS_EXCLUSIVE` system calls cannot succeed, they end with the appropriate error. For example: `EINVAL` – if an unsupported flag was given or the file indicated for unlocking is not locked; `EBADF` – if the given descriptor is invalid; `EALREADY` – if the file indicated for locking is already locked; `EPERM` – if the user is not authorized to unlock the indicated file, etc.
- At most, `NR_EXCLUSIVE` files can be locked simultaneously (this constant is defined in the task's appendix). Calls that would exceed this limit should end with an `ENOLCK` error.
- `VFS_FEXCLUSIVE` and `VFS_EXCLUSIVE` system calls implement the same file locking mechanism. It is possible to lock a file using one system call and unlock it using the other.

## Task Attachments

The task includes a patch `zadanie5-szablon.patch` that defines the constants described in the task and adds the `VFS_FEXCLUSIVE` and `VFS_EXCLUSIVE` system calls implemented by the `int do_fexclusive(void)` and `int do_exclusive(void)` functions added to the `vfs` server.

The task also includes example programs `test-fexclusive.c`, `test-exclusive-lock.c`, and `test-exclusive-unlock.c`, illustrating the use of `VFS_FEXCLUSIVE` and `VFS_EXCLUSIVE` system calls to lock a file specified as a program argument from other users. An example usage scenario of the `test-fexclusive.c` program:

```sh
# make test-fexclusive
clang -O2   -o test-fexclusive test-fexclusive.c
# touch /tmp/test.txt
# ./test-fexclusive /tmp/test.txt
Blokuję plik...
Wynik VFS_FEXCLUSIVE: 0, errno: 0
Czekam... Naciśnij coś
```

At this point, attempts to open the file `/tmp/test.txt` by another user will fail:

```sh
# cat /tmp/test.txt
cat: /tmp/test.txt: Permission denied
```

Continuing the `test-fexclusive` program will close the `/tmp/test.txt` file, unlocking it and allowing other users to access it.

An example usage scenario of the `test-exclusive-lock.c` program:

```sh
# make test-exclusive-lock
clang -O2   -o test-exclusive-lock test-exclusive-lock.c
# touch /tmp/test.txt
# ./test-exclusive-lock /tmp/test.txt
Blokuję plik...
Wynik VFS_EXCLUSIVE: 0, errno: 0
#
```

At this point, attempts to open the file `/tmp/test.txt` by another user will fail:

```sh
# cat /tmp/test.txt
cat: /tmp/test.txt: Permission denied
```

The file can be unlocked using the `test-exclusive-unlock.c` program:

```sh
# make test-exclusive-unlock
clang -O2   -o test-exclusive-unlock test-exclusive-unlock.c
# ./test-exclusive-unlock /tmp/test.txt
Odblokowuję plik...
Wynik VFS_EXCLUSIVE: 0, errno: 0
#
```

Now other users can access it again.

Comments within the example programs also illustrate the use of other system call flags.
