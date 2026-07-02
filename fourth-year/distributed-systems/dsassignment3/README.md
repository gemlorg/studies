# Large Assignment 3 — Raft

An implementation of the **Raft** consensus algorithm as a Rust library, following Diego
Ongaro's dissertation. The assignment is split into four parts; this solution implements three of
them. See [`ASSIGNMENT.md`](ASSIGNMENT.md) for the full statement.


Key points:

- **Persistence.** Current term, vote, log, and snapshot metadata are written through
  `StableStorage` so a server recovers its committed state after a restart.
- **Timers.** Randomized election timeouts and leader heartbeats are driven by the module
  system's `request_tick`, keeping all logic on the single handler thread.
- **Sessions.** Client sessions track the lowest sequence number without a response to make
  retried client commands idempotent, and expire after `session_expiration`.

## Build & test

```bash
cd solution && cargo build
cd ../public-tests && cargo test    # expects ../solution and ../test-utils next to it
```

Test suites cover `basic_raft`, `snapshots`, `client_sessions`, `cluster_membership_changes`, and
a `distributed_set` state machine.
