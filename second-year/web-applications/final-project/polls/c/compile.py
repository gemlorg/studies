import subprocess



def compile(code, id):
    id = str(id)

    subprocess.run(["touch", "./" +id +  ".c"])
       
    with open(id+".c", "w") as f:
        for line in code:
         f.write(line + "\n")

# Compile the code to assembly using sdcc
    subprocess.run(["sdcc", "-S", id+".c"])

# Grep the contents of the assembly file
    grep_result = subprocess.run(["cat", id+".asm"], capture_output=True, text=True)

# Print the result
    res = grep_result.stdout.split("\n")
    print("\n".join(res))
    return res


compile(["#include <stdio.h>", " int main(){", 'printf("Hi");', "}"], 1)

