# Large Assignment 1 — Module system

An asynchronous, message-passing **module system** implemented as a Rust library on top of
Tokio. Modules are user-defined types that handle typed messages one at a time, in arrival
order. See [`ASSIGNMENT.md`](ASSIGNMENT.md) for the full statement.

## Public interface

- `System::new()` / `System::shutdown()` — start the system and shut it down gracefully
  (waits for in-flight handlers and for modules to be dropped, but not for the queue to drain).
- `System::register_module(module)` → `ModuleRef<T>` — register a module and get a handle to it.
- `ModuleRef::send(msg)` — enqueue a message; a module of type `T` accepts `M` when it
  implements `Handler<M>`.
- `ModuleRef::clone()` — additional references to the same module.
- `ModuleRef::request_tick(msg, interval)` → `TimerHandle` — deliver `msg` periodically;
  `TimerHandle::stop()` cancels that stream of ticks.

## Build & test

```bash
cd solution && cargo build          # build the library
cd ../public-tests && cargo test    # run the provided tests (expects ../solution next to it)
```
