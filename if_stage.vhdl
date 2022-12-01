entity IF_STAGE is
port(clk, stall: in std_logic;
		  mem_data_in_1, mem_data_in_2, pc_in: in std_logic_vector(15 downto 0);
		  instr_1, instr_2, mem_addr_1, mem_addr_2, pc_out: out std_logic_vector(15 downto 0)
	);
end IF_STAGE;

architecture behav of IF_STAGE is
begin
	process(clk)
	variable pc : std_logic_vector(15 downto 0) := (others => '0');
	begin
	if falling_edge(clk) and stall = '0' then
		instr_1 <= mem_data_in_1
		instr_2 <= mem_data_in_2
		mem_addr_1 <= std_logic_vector(unsigned(pc_in))
		mem_addr_2 <= std_logic_vector(unsigned(pc_in)+1)
		pc_out <= std_logic_vector(unsigned(pc_in)+2)
	end if;
	end process;
end architecture;