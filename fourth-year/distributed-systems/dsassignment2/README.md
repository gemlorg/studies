# Large Assignment 2 — Distributed atomic register

A distributed block device built as a set of TCP-connected processes that together implement a
linearizable register over disk sectors, using the **(N, N)-AtomicRegister** algorithm. Each
process persists its sectors to stable storage and recovers after a crash. See
[`ASSIGNMENT.md`](ASSIGNMENT.md) for the full statement (the appended section documents the
provided Linux block-device driver).

Key properties:

- **Linearizability** comes from running the NNAR read-impose-write-consult-majority algorithm
  independently per sector, gated on responses from a majority of processes.
- **Durability / recovery**: sector contents and their logical timestamps are fsync-ed and written
  atomically, so a process restart resumes without violating the register semantics.
- **Security**: every TCP message carries an HMAC (system key between processes, client key for
  client commands); messages failing verification are rejected.

## Build & test

```bash
cd solution && cargo build
cd ../public-tests && cargo test    # expects ../solution and ../test-utils next to it
```

