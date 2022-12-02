#!/bin/zsh
python3 gen.py
echo "Starting Compilation"
setopt aliases
ghdl -a prf.vhdl
ghdl -e rename_registers

ghdl -a DUT.vhdl
ghdl -e DUT

# ghdl -a testbench.vhdl
# ghdl -e testbench

ghdl -r testbench --wave=waveform.ghw

echo "Compiled Successfully"