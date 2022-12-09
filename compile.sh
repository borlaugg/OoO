echo "Starting Compilation"
ghdl -a utils.vhdl
ghdl -a if_stage.vhdl
ghdl -a prf.vhdl
ghdl -a id_stage.vhdl
ghdl -a dispatch.vhdl
ghdl -a ls_stage.vhdl
ghdl -a rs_stage.vhdl
ghdl -a branch_predictor.vhdl
ghdl -e rename_registers
ghdl -a Incrementor.vhdl
ghdl -a exec_unit.vhdl
ghdl -a rob.vhdl
ghdl -a OoO_core.vhdl
# # ghdl -a DUT.vhdl
# # ghdl -e DUT
# #ghdl -e rename_registers
# ghdl -a OoO_core.vhdl
# #ghdl -e OoO_core
# #ghdl -a DUT.vhdl
# ghdl -a testbench.vhdl
# ghdl -e testbench
# ghdl -r testbench --wave=waveform.ghw

echo "Compiled Successfully"