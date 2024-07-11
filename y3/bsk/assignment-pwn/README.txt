
# Task Description

The tasks consist of three levels of difficulty and are designed to progressively become more challenging, building on each other. This means that in your solution, you only need to include the exploit for the hardest level you managed to solve. However, we recommend starting with exploiting the "easy" task, then moving to "medium" based on the script used in the previous task. If you are unable to complete the task, still submit the script (without the flag) and a brief comment. Partial solutions will also be evaluated.

The "easy" level solution is worth a maximum of 4 points, the "medium" level up to 7 points, and the "hard" level up to the maximum of 10 points.

The goal of the task is to exploit a remote service at the address and port: `bsk.bonus.re:13337`. The remote service first asks for an index number and the selected difficulty level, which should be included in your script. A simple script skeleton using the pwntools library, `solve.py`, is provided with the task.

An important part of the solution is the reasoning process. Please comment on all "magic constants" (3 words are enough).

Remember to correctly read data from the socket, for example, using `readuntil`/`readn` instead of `read`. The `read` function reads as many data as are currently available in the buffer, without relying on any end-of-data criteria, and thus may not work correctly.
