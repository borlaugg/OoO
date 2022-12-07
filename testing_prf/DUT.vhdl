-- A DUT entity is used to wrap your design.
--  This example shows how you can do this for the
--  Full-adder.

library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

entity DUT is
   port(input_vector: in std_logic_vector(1 downto 0);
       	output_vector: out std_logic_vector(0 downto 0));
end entity;

architecture DutWrap of DUT is
begin

   -- input/output vector element ordering is critical,
   -- and must match the ordering in the trace file!
	core : entity work.OoO_core 
		-- generic map (prf_size <= )
			port map (
				clk => input_vector(1),
				reset => input_vector(0)
			);
	output_vector <= "0";

end DutWrap;

