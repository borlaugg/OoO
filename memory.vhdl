library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity 	DataMemory is
	port(clk, write_en: in std_logic;
		  addr: in std_logic_vector(15 downto 0);
		  data_in: in std_logic_vector(15 downto 0);
		  data_out: out std_logic_vector(15 downto 0)
	);
end DataMemory;

architecture behav of DataMemory is
	type vec_array is array(0 to 2**5 - 1) of std_logic_vector(15 downto 0);
	signal ram: vec_array:= (others=>(others=>'1'));
begin
	process(clk)
	variable out_t : std_logic_vector(15 downto 0) := (others => '1');
	begin
	if falling_edge(clk) then
		if write_en = '1' then
			ram(to_integer(unsigned(ADDR))) <= data_in;
		end if;
	end if;
	out_t := ram(to_integer(unsigned(addr)));

	data_out <= out_t;
	end process;
end architecture;

entity 	InstructionMemory is
	port(clk: in std_logic;
		  addr1, addr2: in std_logic_vector(15 downto 0);
		  data_out_1, data_out_2: out std_logic_vector(15 downto 0)
	);
end InstructionMemory;

architecture behav of InstructionMemory is
	type vec_array is array(0 to 2**5 - 1) of std_logic_vector(15 downto 0);
	signal ram: vec_array:= (others=>(others=>'1'));
begin
	process(clk)
	begin
	data_out_1 <= ram(to_integer(unsigned(addr_1)));
	data_out_2 <= ram(to_integer(unsigned(addr_2)));
	end process;
end architecture;