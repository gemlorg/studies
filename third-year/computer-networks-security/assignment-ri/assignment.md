# Assignment Description

Below is the description of the final assignment for the RE module. The attached file is a prototype game for the Windows platform that you need to analyze and modify to complete the following tasks:

1. **[1 point]** The game starts with an attempt to boot up the computer. Modify the game so that when attempting to enter the disk password, the password characters are displayed instead of asterisks. Describe in the report how the modification was made.
2. **[4 points]** The game verifies the correctness of the entered disk password. In the report, describe the algorithm used to verify the password. Based on the gathered information, find the correct password.
3. **[2 points]** Constantly being forced to boot up the computer by your mom every time the game restarts is quite annoying, isn't it? Modify the game so that its initial state is the same as after the attempt to boot up the computer with your mom. Describe how the current "phase" of the game is determined.
4. **[3 points]** Modify the game so that you can win the fight against Gary. Try not to use brute force alone; reducing HP or increasing attack values will not be fully credited. Investigate what constitutes the unfair advantage of the opponent and level the playing field by applying the appropriate modification.

When submitting your solution, please include:

- A report that precisely describes the solution to each task. The report is best in plaintext format - either a .txt or Markdown (.md) file.
- The modified .exe file with the working changes. It is sufficient to send one .exe file with all the changes. The task can be accomplished using both the software used in the lab (mentioned in the introductory email for the RE module) and its equivalents. So, there is no problem if you use software like Ghidra instead of the decompiler built into IDA Free.

### Additional Tips for Solving the Task:

- The Windows environment should have the Microsoft Visual C++ Redistributable package for Visual Studio 2019 x64 installed (installer: [vc_redist.x64.exe](https://aka.ms/vs/17/release/vc_redist.x64.exe)). The application was tested on Windows 10 and Windows 11. If you encounter any issues running it, please email us with your operating system version and the error message (a screenshot can also be included).
- Remember to extract all files from the .zip archive before running the application and starting the analysis.
- The task includes a .pdb file (symbols). Ensure they are available during analysis, i.e., you see function names and clicked Yes in the IDA window "Do you want to look for this file at the specified path and the Microsoft Symbol Server?"
- SDL libraries contain so-called TLS callbacks, on which x64dbg sets breakpoints. In Options -> Preferences, leave only "Entry Breakpoint" enabled and disable "System Breakpoint" and "TLS Callbacks".
- In x64dbg, you can apply patches via File -> Patch file (CTRL+P). Instructions can be changed using the assembler built into x64dbg (space bar) or pre-assembled in nasm and added binarily through \*RMB -> Edit -> Binary Edit (CTRL+E). - Remember that not all space in the section is reflected in the file (file alignment is 0x200, and page alignment is 0x1000). If not all patches can be applied to the file and you get a message like 8/14 patches applied (instead of 14/14), it means that some patches exceeded the section range available in the file.
- The addresses shown in IDA have a different base address than in x64dbg. However, offsets relative to the beginning of the mapped file will be the same. This can be useful when searching for the same instruction in both IDA and x64dbg. The "Go to" window supports RVA, so to find 0x140005D47 in x64dbg, just enter :$0x5D47.
