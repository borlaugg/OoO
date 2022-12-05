library ieee;
use ieee.std_logic_1164.all;

entity sign_extend7 is
	port(
		input: in std_logic_vector(8 downto 0);
		output: out std_logic_vector(15 downto 0));
end entity;

architecture extend7 of sign_extend7 is
begin
	
	output(8 downto 0) <= input;
	output(15 downto 9) <= "0000000" when input(8) = '0' else "1111111";
	
end architecture;