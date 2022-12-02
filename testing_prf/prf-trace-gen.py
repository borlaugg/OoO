#!/usr/bin/python3
# (dst, op1, op2)
instrs = [((1, 2, 3), (1, 1, 1))]
lines = []

def int_to_bin3(x):
    return bin(x)[2:].zfill(3)
    
for instr in instrs:

    line = ""
    i1 = instr[0]
    i2 = instr[1]
    line = "".join(list(map(int_to_bin3, i1[::-1]))) 
    line = "".join(list(map(int_to_bin3, i2[::-1]))) + line
    line = "0" + line


    lines.append("01"+line+ " 0 0")
    lines.append("00"+line+ " 0 0")

with open("TRACEFILE.txt", "w") as f:
    f.write("\n".join(lines))
