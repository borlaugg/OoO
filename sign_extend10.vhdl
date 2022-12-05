library ieee;
use ieee.std_logic_1164.all;

entity sign_extend10 is
	port(
		input: in std_logic_vector(5 downto 0);
		output: out std_logic_vector(15 downto 0));
end entity;

architecture extend10 of sign_extend10 is
begin
	
	output(5 downto 0) <= input;
	output(15 downto 6) <= "0000000000" when input(5) = '0' else "1111111111";
	
end architecture;