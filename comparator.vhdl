library ieee;
use ieee.std_logic_1164.all;

entity comparator is
	port(
		input1: in std_logic_vector(15 downto 0);
		input2: in std_logic_vector(15 downto 0);

		status: out std_logic);
end entity;

architecture compare of comparator is
begin
	
	status <= '1' when input1 = input2 else '0';

end architecture;