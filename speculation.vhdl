library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity speculate is
    port (pc1, pc2, pc_bc : in std_logic_vector(15 downto 0);
          clk, stall, beq : in std_logic;
          spec1, spec2 : out std_logic);
end entity;

architecture idnarr of speculate is
begin   
    process (clk)
    begin
        if (clk' event and clk = '1') then
            if (to_integer(unsigned(pc1)) > to_integer(unsigned(pc_bc))) then
                spec1 <= '1';
            else 
                spec1 <= '0';
            end if;
            if (to_integer(unsigned(pc2)) > to_integer(unsigned(pc_bc))) then
                spec1 <= '1';
            else 
                spec1 <= '0';
            end if;
        end if;
    end process;
end idnarr; 