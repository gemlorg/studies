# Internet Radio Transmitter and Receiver

This project involves writing an internet radio transmitter and receiver. It is a crucial part of task 2, which will involve extending the functionality of the transmitter and receiver.

## Constants Used

- **DEST_ADDR**: The address of the receiver, set by the mandatory `-a` parameter of the transmitter.
- **DATA_PORT**: The UDP port used for data transmission, set by the `-P` parameter of both the transmitter and receiver, defaulting to `20000 + (album_number % 10000)`.
- **PSIZE**: The size in bytes of the `audio_data` field in a packet, set by the `-p` parameter of the transmitter, defaulting to 512 bytes.
- **BSIZE**: The buffer size in bytes, set by the `-b` parameter of the receiver, defaulting to 64KB (65536 bytes).
- **NAME**: The name of the transmitter, set by the `-n` parameter, defaulting to "Unnamed Transmitter".

## Part A: Transmitter

The transmitter is used to send a data stream received on standard input to the receiver. The transmitter should receive the data stream at the rate that the receivers can process the data and then send this data packed in UDP datagrams to the `DATA_PORT` at the address `DEST_ADDR` specified in the command line. Data should be transmitted in packets of `PSIZE` bytes according to the protocol described below. The transmission rate is the same as the input rate to the receiver; the transmitter does not interfere with it in any way.

After sending the entire content of the standard input, the transmitter exits with exit code 0. If the size of the read data is not divisible by `PSIZE`, the last (incomplete) packet is discarded and not sent.

### Running the Transmitter

For example, to send your favorite song in CD quality MP3 format, you can use the following command:

```sh
sox -S "05 Muzyczka.mp3" -r 44100 -b 16 -e signed-integer -c 2 -t raw - | pv -q -L \$((44100\*4)) | ./sikradio-sender -a 10.10.11.12 -n "Radio Muzyczka"
```

To send data from a microphone (in the above format), you can use the following command:

```sh
arecord -t raw -f cd | ./sikradio-sender -a 10.10.11.12 -n "Radio Podcast"
```

The `arecord` command can be found in the `alsa-utils` package.

## Part B: Receiver

The receiver receives data sent by the transmitter and outputs it to the standard output.

The receiver has a buffer of size `BSIZE` bytes to store data from up to `⌊BSIZE/PSIZE⌋` consecutive packets.

### Starting Playback

The receiver:

1. Clears the buffer, discarding any data it contains that has not yet been output to standard output.
2. Connects to the transmitter at `DATA_PORT`. The transmitter is set by the mandatory `-a` parameter of the receiver.
3. Upon receiving the first audio packet, records the value of the `session_id` field and the number of the first received byte (call it `BYTE0`).
4. Until the byte number `BYTE0 + ⌊BSIZE*3/4⌋` or higher is received, the receiver does not output data to the standard output. When this happens, it outputs data to standard output as quickly as the standard output allows.

This procedure should be applied wherever the task specifies starting playback.

### Handling New Packets

If the receiver receives a new packet with a number higher than those received so far, it places it in the buffer and, if necessary, reserves space for missing packets before it. If needed, it removes old data not yet output to standard output.

The receiver outputs diagnostic information to standard error (stderr) regarding missing packets. Whenever it receives a packet numbered `n`, it checks the buffer and for each `i < n` for which it has not received a packet, but there is space for it in the buffer, it outputs:

```plaintext
MISSING: BEFORE n EXPECTED i
```

where `i` and `n` are replaced by the appropriate numbers. The receiver does not output messages about packets containing bytes earlier than `BYTE0` or those too old to fit in the buffer.

### Running the Receiver

```sh
./sikradio-receiver -a 10.10.11.12 | play -t raw -c 2 -r 44100 -b 16 -e signed-integer --buffer 32768 -
```

The `play` command can be found in the `sox` package.

## Audio Data Transmission Protocol

### Data Exchange

- Data exchange occurs via UDP. Communication is one-way – the transmitter sends audio packets, and the receiver receives them.
- Datagram Format: Data is transmitted in binary datagrams, following the format defined below.
- Byte Order: All numbers in messages are transmitted in network byte order (big-endian).

### Audio Packet Structure

```c
uint64 session_id
uint64 first_byte_num
byte[] audio_data
```

- The `session_id` field remains constant throughout the transmitter's operation. It is initialized with the date expressed in seconds since the epoch at the start of its operation.
- The receiver records the `session_id` from the first packet received after starting playback. If a packet is received with:
  - a lower `session_id`, it is ignored,
  - a higher `session_id`, playback restarts.

Bytes read by the transmitter from standard input are numbered starting from zero. The transmitter places the number of the first byte in `audio_data` in the `first_byte_num` field.

The transmitter sends packets where the `audio_data` field is exactly `PSIZE` bytes long (and `first_byte_num` is divisible by `PSIZE`).

## Additional Requirements

- The programs should support communication using IPv4. IPv6 support is not required.
- In the implementation, large queues of messages, events, etc., should be allocated dynamically.
- The programs must be resilient to erroneous situations that give a chance to continue operation. The intention is that the programs can run indefinitely without needing to be restarted, e.g., in case of communication problems, temporary network unavailability, regular configuration changes, etc.
- The programs should be written clearly. Valuable guidelines on this topic can be found here: [Linux Kernel Coding Style](https://www.kernel.org/doc/html/v5.6/process/coding-style.html)
- See also: [Stack Overflow: When does a UDP sendto block](https://stackoverflow.com/questions/4165174/when-does-a-udp-sendto-block)
- The played sound must be smooth, without frequent or unexplained glitches.
- We recommend following the Robustness Principle: [Robustness Principle](https://en.wikipedia.org/wiki/Robustness_principle)
- When processing network binary data, use fixed-width types: [Fixed-width integer types](http://en.cppreference.com/w/c/types/integer)
- In case of incorrect command-line arguments, the programs should print an appropriate message to standard error and return code 1.

## Submitting the Solution

You can submit a solution for only Part A (5 points) or only Part B (5 points), or both parts (10 points).

The solution must:

- Work in the Linux environment in the LK;
- Be written in C or C++ using the socket interface (not allowed to use `boost::asio`);
- Compile with GCC (use `gcc` or `g++` command) – among the parameters use `-Wall` and `-O2`, you can use standards `-std=c2x`, `-std=c++20` (to the extent supported by the compiler on the `students` machine).
- Use commonly known helper libraries (e.g., `boost::program_options`) if they are installed on the `students` machine.

Submit the source files and a `makefile` in a `ab123456.tgz` archive where `ab123456` is your standard login used on faculty machines, following the schema: initials, index number. Do not include binary files or intermediate files generated during compilation.

Executing the `make` command for Part A should produce the executable `sikradio-sender`, and for Part B, the executable `sikradio-receiver`.

Additionally, the `makefile` should handle the `clean` target, which removes all files generated during compilation.
