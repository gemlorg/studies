# Payment System in MINIX

The goal of this task is to enable processes in the MINIX system to possess money and transfer funds between each other.

## Task Description

Each process receives an initial balance of `INIT_BALANCE` currency units. Processes can then transfer money to each other, i.e., process `P` can transfer `n` currency units to process `Q`. For the transfer to succeed, process `P` must have at least `n` units of currency (the account balance of the process cannot be negative), and process `Q` must have no more than `MAX_BALANCE - n` currency units. Additionally, as a basic anti-money laundering measure, processes `P` and `Q` must not be in a parent-child relationship. If the transfer succeeds, process `P`'s account balance decreases by `n` units, and process `Q`'s account balance increases by `n` units.

The money of processes is not inherited - when a process terminates, the currency units it accumulated disappear.

**Note**: Granting each process new currency units inevitably leads to inflation, but we will leave this problem to economists.

## New System Call

The task involves adding a system call `PM_TRANSFER_MONEY` and a library function `int transfermoney(pid_t recipient, int amount)`. The function should be declared in the `unistd.h` file. The constants `INIT_BALANCE = 100` and `MAX_BALANCE = 1000` should be defined in the `minix/config.h` file.

The function call `int transfermoney(pid_t recipient, int amount)` should transfer `amount` currency units from the account of the calling process to the account of the process identified by `recipient`. If the transfer is successful, the function returns the balance of the calling process after the transfer.

**Note**: A process can check its account balance, for example, by transferring 0 currency units to itself.

If the transfer fails, the `transfermoney` function returns `-1` and sets `errno` to the appropriate error code:

- If `recipient` is not the identifier of a currently running process, `errno` is set to `ESRCH`.
- If `recipient` is the identifier of a process that is a child or ancestor of the process calling `transfermoney`, `errno` is set to `EPERM`.
- If the `amount` value is negative, or the calling process has less than `amount` currency units, or the process identified by `recipient` has more than `MAX_BALANCE - amount` currency units, `errno` is set to `EINVAL`.

The `transfermoney()` function should use the new `PM_TRANSFER_MONEY` system call, which should be added to the PM server. To pass parameters, a custom message type should be defined.

## Solution Format

Below, we assume that `ab123456` denotes the student's identifier solving the task. You should prepare a patch with changes in the `/usr` directory. The patch file named `ab123456.patch` is created using the command:

```sh
diff -rupNEZbB original-sources/usr/ my-solution/usr/ > ab123456.patch
```

where `original-sources` is the path to the unchanged MINIX sources, and `my-solution` is the path to the MINIX sources containing the solution. This command recursively scans the files in the `original-sources/usr` path, compares them with the files in the `my-solution/usr` path, and generates a file `ab123456.patch` summarizing the differences. This file will be used to automatically apply changes to a clean copy of MINIX, where solution testing will be conducted. More information about the `diff` command can be found in the manual (`man diff`).

Applying the patch in the root directory of a clean MINIX copy and executing the command:

```sh
patch -p1 < ab123456.patch
```

should result in applying all expected changes required by the solution. Ensure that the patch contains only the necessary differences.

After applying the patch, the following commands will be executed:

```sh
make && make install in the directories /usr/src/minix/fs/procfs, /usr/src/minix/servers/pm, /usr/src/minix/drivers/storage/ramdisk, /usr/src/minix/drivers/storage/memory, and /usr/src/lib/libc
make do-hdboot in the /usr/src/releasetools directory
reboot
```

Submit the solution in the form of the `ab123456.patch` file on Moodle.

## Notes

- The PM server stores information about processes in the `mproc` table declared in the `mproc.h` file.
- It is worth analyzing how PM implements system calls. More information about the operation of this server will be provided in laboratory 8.
- The task mentions executing the `make` command in the `/usr/src/minix/fs/procfs`, `/usr/src/minix/drivers/storage/ramdisk`, and `/usr/src/minix/drivers/storage/memory` directories because they contain files that include the `mproc.h` file.
- Test the solution independently. One basic scenario is as follows: start process A, which then starts processes B and C; processes B and C start transferring money to each other.
- No points will be awarded for a solution where the patch does not apply correctly, does not compile, or causes a kernel panic during system startup.
