# Enhanced Internet Radio Transmitter and Receiver

This project extends the functionality of the internet radio transmitter and receiver from task 1. It introduces features such as multicast broadcasting, retransmission requests, and user interface for station switching.

## Variables Used

- **MCAST_ADDR**: Multicast address set by the mandatory `-a` parameter of the transmitter.
- **DISCOVER_ADDR**: Address used by the receiver to discover active transmitters, set by the `-d` parameter of the receiver, defaulting to `255.255.255.255`.
- **DATA_PORT**: UDP port used for data transmission, set by the `-P` parameter of the transmitter, defaulting to `20000 + (album_number % 10000)`.
- **CTRL_PORT**: UDP port used for control packet transmission, set by the `-C` parameter of both the transmitter and receiver, defaulting to `30000 + (album_number % 10000)`.
- **UI_PORT**: TCP port for the simple text interface to switch between stations, defaulting to `10000 + (album_number % 10000)`; set by the `-U` parameter of the receiver.
- **PSIZE**: Size in bytes of the `audio_data` field in a packet, set by the `-p` parameter of the transmitter, defaulting to 512 bytes.
- **BSIZE**: Buffer size in bytes, set by the `-b` parameter of the receiver, defaulting to 64KB (65536 bytes).
- **FSIZE**: Size in bytes of the transmitter's FIFO queue, set by the `-f` parameter of the transmitter, defaulting to 128KB.
- **RTIME**: Time (in milliseconds) between sending subsequent reports of missing packets (for receivers) and time between retransmissions of packets, set by the `-R` parameter, defaulting to 250 ms.
- **NAME**: Name of the transmitter, set by the `-n` parameter, defaulting to "Unnamed Transmitter".

## Part A: Transmitter

The transmitter sends the data stream received on standard input to the receivers. It should send data packed in UDP datagrams to the `DATA_PORT` at the multicast address `MCAST_ADDR`. Data should be transmitted in packets of `PSIZE` bytes.

### Retransmissions

The transmitter maintains a FIFO queue of the last `FSIZE` bytes read from the input to resend packets requested by the receivers.

The transmitter continuously collects retransmission requests from receivers, sends a series of retransmissions after collecting requests for `RTIME`, then repeats the process.

The transmitter listens on UDP `CTRL_PORT` for control packets, handling two types of messages:

- **LOOKUP**: Requests for identification, responded immediately with a **REPLY** message.
- **REXMIT**: Requests for packet retransmission, processed periodically.

After sending all input data, the transmitter exits with code 0. If the read data size is not divisible by `PSIZE`, the last incomplete packet is discarded.

### Running the Transmitter

To send an MP3 file in CD quality:

```sh
sox -S "05 Muzyczka.mp3" -r 44100 -b 16 -e signed-integer -c 2 -t raw - | pv -q -L \$((44100*4)) | ./sikradio-sender -a 239.10.11.12 -n "Radio Muzyczka"
```

To send data from a microphone:

```sh
arecord -t raw -f cd | ./sikradio-sender -a 239.10.11.12 -n "Radio Podcast"
```

## Part B: Receiver

The receiver receives data sent by the transmitter and outputs it to standard output.

### Station Discovery

The receiver sends an identification request (LOOKUP) to `DISCOVER_ADDR` on `CTRL_PORT` approximately every 5 seconds. It maintains a list of available stations based on received REPLY messages. A station is removed from the list if no REPLY is received within 20 seconds.

### Buffering and Playback

The receiver has a buffer of `BSIZE` bytes for storing up to `⌊BSIZE/PSIZE⌋` consecutive packets.

Upon starting playback:

1. Clears the buffer.
2. If needed, unsubscribes from the previous multicast address and subscribes to the new one.
3. Records the `session_id` and first received byte number (BYTE0) from the first audio packet.
4. Sends retransmission requests for missing packets every `RTIME` until all packets up to BYTE0 + ⌊BSIZE\*3/4⌋ are received.

### Handling Missing Packets

If the receiver detects missing packets, it sends retransmission requests. It restarts playback if any missing packets would otherwise be output to standard output.

### User Interface

The receiver listens for TCP connections on `UI_PORT`. Users can connect (e.g., using telnet) to see a simple text interface to switch stations using the up/down arrow keys. Changes in one connection are reflected in all connections.

### Running the Receiver

```sh
./sikradio-receiver -d 239.255.255.250 -U 10000 | play -t raw -c 2 -r 44100 -b 16 -e signed-integer --buffer 32768 -
```

## Audio Data Transmission Protocol

### Datagram Format

```c
uint64 session_id
uint64 first_byte_num
byte[] audio_data
```

### Control Protocol

- **LOOKUP** message:
  ```plaintext
  ZERO_SEVEN_COME_IN
  ```
- **REPLY** message:
  ```plaintext
  BOREWICZ_HERE [MCAST_ADDR] [DATA_PORT] [station_name]
  ```
- **REXMIT** message:
  ```plaintext
  LOUDER_PLEASE [list of packet numbers separated by commas]
  ```

## Additional Requirements

- Programs should support IPv4 communication.
- Large queues of messages, events, etc., should be allocated dynamically.
- Programs must handle erroneous situations and continue operation without needing restarts.
- Programs should be written clearly. See: [Linux Kernel Coding Style](https://www.kernel.org/doc/html/v5.6/process/coding-style.html)
- Programs should be resilient to delays in communication with some clients.
- The played sound must be smooth and uninterrupted.
- Follow the Robustness Principle: [Robustness Principle](https://en.wikipedia.org/wiki/Robustness_principle)
- Use fixed-width types for network binary data: [Fixed-width integer types](http://en.cppreference.com/w/c/types/integer)

## Submitting the Solution

Submit solutions for Part A or Part B, or both.

The solution must:

- Work in a Linux environment.
- Be written in C or C++ using the socket interface (do not use `boost::asio`).
- Compile with GCC (use `gcc` or `g++`) with `-Wall` and `-O2`, optionally using standards `-std=c2x`, `-std=c++20`.
- Use commonly known helper libraries if installed on `students` machine.

Submit source files and a `makefile` in an archive named `ab123456.tgz` (your standard login).

- `make` for Part A should produce `sikradio-sender`.
- `make` for Part B should produce `sikradio-receiver`.
- `make clean` should remove all compilation-generated files.
