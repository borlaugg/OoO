-- A DUT entity is used to wrap your design.
--  This example shows how you can do this for the
--  Full-adder.

library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

entity DUT is
   port(input_vector: in std_logic_vector(20 downto 0);
       	output_vector: out std_logic_vector(0 downto 0));
end entity;

architecture DutWrap of DUT is
begin

   -- input/output vector element ordering is critical,
   -- and must match the ordering in the trace file!
	rr: entity work.rename_registers 
		-- generic map (prf_size <= )
			port map (
				gr1 => unsigned(input_vector(2 downto 0)),
				gr2 => unsigned(input_vector(5 downto 3)),
				gr3 =>  unsigned(input_vector(8 downto 6)),
				gr4 =>  unsigned(input_vector(11 downto 9)),
				gr5 => unsigned(input_vector(14 downto 12)),
				gr6 =>  unsigned(input_vector(17 downto 15)),
				reset => input_vector(18),
				clk => input_vector(19),
				stall => input_vector(20)
			);
	output_vector <= "0";

end DutWrap;

