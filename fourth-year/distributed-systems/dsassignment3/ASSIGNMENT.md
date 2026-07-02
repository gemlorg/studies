
# Distributed Systems Large Assignment 3

### Raft

Your task is to implement the Raft consensus algorithm. The solution shall take the form of a Rust library. A template for the solution, public tests, and additional files are provided in [this package](https://www.mimuw.edu.pl/~iwanicki/courses/ds/2024/labs/LA3/./dsassignment3.tgz).

### Assignment overview

The solution shall implement the Raft consensus algorithm, following its description in Diego Ongaro’s dissertation [\[1\]](#bib1). More specifically, the scope of the assignment is divided into four parts:

1.  basic Raft (10 points),
2.  snapshots (5 points),
3.  client sessions (4 points),
4.  cluster membership changes (6 points).

The numbers in parentheses denote the maximal number of points for each part of the solution. An implementation of basic Raft (point 1) is mandatory to receive any points for the assignment. Implementations of snapshots (point 2), client sessions (point 3), and cluster membership changes (point 4) are voluntary: you can choose to implement any subset of them. However, if you do not implement some part, you *must* provide a mock interface as specified below; otherwise, you may lose more points than the respective part’s value.

#### Basic Raft

Basic Raft is the part described in Chapter 3 of [\[1\]](#bib1). Your implementation shall follow Figure 3.1 of [\[1\]](#bib1) and the following remarks:

- Do not optimize the number of rejected *AppendEntries* (Chapter 3.5 of [\[1\]](#bib1)).
- Assume that the state machine is volatile (it is relevant, for example, in Chapter 3.8 of [\[1\]](#bib1)).
- You do not have to implement the leadership transfer extension (Chapter 3.10 of [\[1\]](#bib1)).
- When a follower receives a client request meant for a leader, it shall reject the request and include the leader’s identifier in the response if it knows the current term’s leader (Chapter 6.2 of [\[1\]](#bib1)).
- When a server becomes a leader, it must append a `NoOp` entry to the log (nextIndex must be initialized with the index of this entry).
- A leader should convert itself into a follower if an election timeout elapses without a successful round of heartbeats with a majority of the cluster (Chapter 6.2 and Figure 6.1 of [\[1\]](#bib1)). More precisely, the timing requirements for this behavior are analogous to the ones for timeouts in cluster membership changes, that is, this condition can be checked periodically.
- The log shall initially contain one `Configuration` entry with term `0`, timestamp `first_log_entry_timestamp` and servers `config.servers` (see `src/lib.rs` and `src/domain.rs`). The index of this entry is `0`.
- In every *AppendEntries* message:
  - if `nextIndex == matchIndex + 1`, send as many log entries as possible, but no more than `config.append_entries_batch_size` (see `src/domain.rs`) (Chapter 3.5 of [\[1\]](#bib1)),
  - otherwise send no log entries.
- When a leader has log entries to send to a follower, it should send *AppendEntries* immediately (rather than send *AppendEntries* only on heartbeat timeouts).
- A server shall ignore a *RequestVote* received within the minimum election timeout of hearing from a current leader (Chapter 4.2.3 of [\[1\]](#bib1)). As a consequence of that, a leader shall always ignore a *RequestVote*.
- A server must not send any messages to itself.

#### Snapshots

Snapshots are described in Chapter 5.1 of [\[1\]](#bib1). Your implementation shall follow Figure 5.3 of [\[1\]](#bib1) and the following remarks:

- A snapshot shall also contain the state of client sessions from the moment of the snapshot. This information shall be sent only with the first chunk.
- To send a snapshot to a follower you can create a copy of the snapshot and keep it until the whole snapshot is sent (but you still have to send the snapshot chunk by chunk).
- Before sending the next chunk the leader shall wait for an acknowledgment that the current chunk has been received successfully.
- Your Raft implementation shall not take snapshots on its own. A snapshot shall be created only when a *Snapshot* client request (see `src/domain.rs`) is received.
- An *InstallSnaphot* message is considered a heartbeat just like *AppendEntries* (but you must not send *InstallSnapshot* when there is no reason to use it instead of *AppendEntries*).

If you do not implement snapshots, your implementation shall panic by calling:

``` numberSource
unimplemented!("Snapshots omitted")
```

when it receives a *Snapshot* client request or *InstallSnapshot* and *InstallSnapshotResponse* messages (see `src/domain.rs`).

#### Client sessions

Client sessions are described in Chapter 6 of [\[1\]](#bib1). Your implementation shall follow Figure 6.1 of [\[1\]](#bib1) and the following remarks:

- You do not have to implement read-only commands (*ClientQuery*, Chapter 6.4 of [\[1\]](#bib1)).
- Finding the cluster (Chapter 6.1 of [\[1\]](#bib1)) is beyond the scope of this assignment.
- You shall use the log index of the `RegisterClient` log entry (see `src/domain.rs`) as the client identifier.
- You shall allow concurrent requests from a single client (Chapter 6.3 of [\[1\]](#bib1)).
- You shall use timestamps of committed entries to expire client sessions (Chapter 6.3 of [\[1\]](#bib1)). The expiration interval is specified in the `ServerConfig` struct (see `src/domain.rs`). A session’s last activity time is the timestamp of the most recently committed log entry with this session’s client identifier. When committing a log entry, you should expire sessions **before** you update the last activity time.
- There are no assumptions about clock synchronization between servers.

If you do not implement client sessions:

- When a *RegisterClient* client request (`src/domain.rs`) is received, your implementation shall commit a `RegisterClient` log entry (`src/domain.rs`), and reply with this entry’s log index once it is committed (Figure 6.1 of [\[1\]](#bib1)). However, the implementation does not have to allocate a session.
- `lowest_sequence_num_without_response` (`src/domain.rs`) shall be stored in the log, and `client_id` and `sequence_num` (`src/domain.rs`) shall be stored in the log and used when responding to messages, but they do not have to be verified in any way.
- Client sessions in snapshots shall be empty.

#### Cluster membership changes

Cluster membership changes are described in Chapter 4 of [\[1\]](#bib1). Your implementation shall follow Figure 4.1 of [\[1\]](#bib1) and the following remarks:

- Pay special attention to the last part of Chapter 4.1 of [\[1\]](#bib1) as it is important for the safety of the algorithm.

- When a leader removes itself from the cluster, it should step down as described in Chapter 4.2.2 of [\[1\]](#bib1).

- You must use all responses to membership change requests according to their descriptions in `src/domain.rs`. When multiple responses would make sense, prefer the one that appears first in `src/domain.rs`.

- A leader must wait until it has committed an entry from the current term before appending a configuration entry to the log. (This fixes a bug in the membership changes described in [\[1\]](#bib1). You can read about this bug [here](https://groups.google.com/g/raft-dev/c/t4xj6dJTP6E/m/d2D9LrWRza8J).)

- Regarding timeouts during catching servers up, it is enough to satisfy the following:

  - When a server does not make progress for `2 * max election timeout`, or the last round takes longer than `2 * max election timeout`, there is a *TIMEOUT* reply.
  - When a server always makes progress in less than `min election timeout`, and the last round takes shorter than `min election timeout`, there is no *TIMEOUT* reply.

  In other words, you are allowed to periodically check if there was progress during the last interval. The period has to be within the election timeout range and might be randomized on every timeout.

If you do not implement cluster membership changes:

- Your implementation shall panic by calling:

  ``` numberSource
  unimplemented!("Cluster membership changes omitted")
  ```

  when it receives *AddServer* and *RemoveServer* client requests (see `src/domain.rs`),

- The cluster configuration shall still be included in snapshots.

### Assignment specification

To run the system you should use the module system you implemented as the first Large Assignment.

#### Interface

Your solution must conform to the public interface provided in the template.

You have to implement three functions (see `src/lib.rs`):

- `Raft::new()`,
- `<Raft as Handler<ClientRequest>>::handle()`,
- `<Raft as Handler<RaftMessage>>::handle()`.

You do not have to provide any implementations of `StableStorage`, `RaftSender` and `StateMachine` traits.

You are not required to optimize stable storage updates (saving the entire persistent state on every update is fine).

`Raft` modules shall communicate with each other through the `message_sender` provided in `Raft::new()` (see `src/lib.rs`) and send and handle messages in a way consistent with the description of the Raft algorithm.

For more information about the interface, see doc comments in the template and [\[1\]](#bib1).

#### Testing

You are given a subset of official tests (see `public-tests/` in the package). They test individual parts of the assignment, and the system as a whole by implementing on top of it a distributed set that stores integers. Their intention is to make sure that the public interface of your solution is correct and to evaluate basic functionality.

Your solution will be tested with the latest stable Rust version.

#### Varia

You can use logging if you want to, but do not emit a large amount of logs at levels `>= INFO` when the system is operating properly. All logging must be done via the `log` crate.

You can only use crates specified in the provided `Cargo.toml` file.

For a more compact description of Raft you can take a look at [\[2\]](#bib2). Keep in mind, though, that it can differ from [\[1\]](#bib1), which is the main source for this assignment. In particular, it describes a different, more complex implementation of cluster membership changes.

The Raft papers use RPCs (Remote Procedure Calls), which are a slightly different model of communication than messages in the module system. Because of that, message responses in the template have a few fields that do not appear in Raft’s description.

#### Grading

Your solution will be graded based on results of automated tests and code inspection. The number of available points is specified in the [Passing Rules](https://www.mimuw.edu.pl/~iwanicki/courses/ds/2024/labs/LA3/../../) described on the main website of the course. Note that for this assignment there are no required or guaranteed points!

#### Asking questions

Questions **must** be asked on a dedicated Moodle forum. This way everybody will be able to read the answer. Try to ask questions early if there are any. We will try not to require any changes to existing solutions when providing answers.

#### Submitting solution

Your solution must be submitted as a single `.zip` file with its name being your login at students (e.g., `ab123456.zip`). After unpacking the archive, a directory path named `ab123456/solution/` must be created. In the `solution` subdirectory there must be a Rust library crate that implements the required interface. Project `public-tests` must be able to be built and tested cleanly when placed next to the `solution` directory.

 Remember that for this assignment no delays will be tolerated! 

### Bibliography

- \[1\] Diego Ongaro *“Consensus: Bridging Theory and Practice”* (<https://raw.githubusercontent.com/ongardie/dissertation/master/online.pdf>)

- \[2\] Diego Ongaro, John Ousterhout *“In Search of an Understandable Consensus Algorithm (Extended Version)”* (<https://raft.github.io/raft.pdf>)

------------------------------------------------------------------------

Authors: F. Plata, K. Iwanicki, M. Banaszek, W. Ciszewski.
