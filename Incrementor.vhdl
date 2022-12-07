library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

entity Increment is
    port (pc_in: in std_logic_vector(15 downto 0);
          pc_out: out std_logic_vector(15 downto 0));
end entity;

architecture beh of Increment is
begin
    pc_out <= std_logic_vector(unsigned(pc_in)+2);
end beh;