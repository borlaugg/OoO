library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

entity IF_STAGE is
port(clk, stall,rst: in std_logic;
		  mem_data_in_1, mem_data_in_2, pc_in: in std_logic_vector(15 downto 0);
		  instr_1, instr_2, mem_addr_1, mem_addr_2, pc_out, pc_out_1, pc_out_2: out std_logic_vector(15 downto 0)
	);
end IF_STAGE;

architecture behav of IF_STAGE is
begin
	process(clk)
	variable pc, zero : std_logic_vector(15 downto 0) := (others => '0');
	begin
	if (rst = '1') then
		pc_out <= zero;
		mem_addr_1 <= zero;
		mem_addr_2 <= zero;
		instr_1 <= zero;
		instr_2 <= zero;
		pc_out_1 <= zero;
		pc_out_1 <=zero;
	elsif falling_edge(clk) and stall = '0' and rst = '0' then
		instr_1 <= mem_data_in_1;
		instr_2 <= mem_data_in_2;
		mem_addr_1 <= std_logic_vector(unsigned(pc_in));
		mem_addr_2 <= std_logic_vector(unsigned(pc_in)+1);
		pc_out <= std_logic_vector(unsigned(pc_in)+2);
		pc_out_1 <= std_logic_vector(unsigned(pc_in));
		pc_out_2 <= std_logic_vector(unsigned(pc_in)+1);
	end if;
	end process;
end architecture;