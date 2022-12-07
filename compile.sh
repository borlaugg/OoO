python3 gen.py
echo "Starting Compilation"
setopt aliases
ghdl -a prf.vhdl
ghdl -a rs_stage.vhdl
#ghdl -e rs_stage
#ghdl -e rs_stage
ghdl -a exec_unit.vhdl
#ghdl -a rob.vhdl
ghdl -a if_stage.vhdl
ghdl -a id_stage.vhdl
#ghdl -e ID_STAGE
#ghdl -e exec_unit
# ghdl -a DUT.vhdl
# ghdl -e DUT
#ghdl -e rename_registers
ghdl -a OoO_core.vhdl
#ghdl -e OoO_core
ghdl -a DUT.vhdl
ghdl -a testbench.vhdl
ghdl -e testbench
# ghdl -r testbench --wave=waveform.ghw

echo "Compiled Successfully"