library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;

package array_pkg is
    type addr_array is array(natural range <>) of std_logic_vector(16 downto 0);
    type prf_data_array is array(natural range <>) of std_logic_vector(16 downto 0);
    type bit16_vector is array(natural range <>) of std_logic_vector(15 downto 0);
    type exec_inps is array(0 to 1) of std_logic_vector(15 downto 0);
end package;