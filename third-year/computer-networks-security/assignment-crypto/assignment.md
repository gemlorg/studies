# Assignment Description

In this task, there are 3 flags to be obtained, worth:

- 4 points (first sub-task)
- 3 points (easier flag from the second sub-task)
- 3 points (harder flag from the second sub-task)

All flags on the server have the format `flag{XXX}` where XXX is a string of ASCII characters.

## Requirements for the Solution Script

The solution script should:

- Function correctly and print the flag or flags obtained from the remote server to stdout (additional debug information is also acceptable).
- Be written according to good practices and free from programming errors (pay special attention to the communication layer with the server to avoid race conditions).
- Accept two parameters, the server hostname and the task port (e.g., run as `python3 script.py cryptotask2023.tailcall.net 30007` or `python3 script.py localhost 13371`) - this will facilitate our evaluation and your scripting.
- Be written without any external dependencies, except optionally for the `pwntools` and `pycrypto/pycryptodome` libraries.
- Operate quickly with a locally hosted server. In particular, avoid making more requests to the server than necessary.

In some cases, a correct description of the solution idea can earn partial points. Unstable or unnecessarily slow scripts will receive fewer points even if they correctly print the flag. Remember, for late submissions, 1 point is deducted for each day of delay. Additionally, due to the grading deadline, solutions submitted after February 23 will receive zero points.
