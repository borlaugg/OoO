library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;

package array_pkg is
    type addr_array is array(natural range <>) of std_logic_vector(16 downto 0);
    type prf_data_array is array(natural range <>) of std_logic_vector(16 downto 0);
end package;


library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;
use work.array_pkg.all;

-- r1 = r2 + r3
-- r4 = r5 + r6 

entity rename_registers is
    generic(
        prf_size: integer := 128;
        rs_size: integer := 16;
        rob_size: integer := 20
        -- there are 8 architectural registers
    ); 
    port(gr1,gr4,
    gr2,gr3,gr5,gr6: in std_logic_vector(2 downto 0);
    reset, clk, stall: in std_logic;

    -- gr1-6 are the registers from the two lines of code above(in that order)
    prf_addr_bus: in addr_array(0 to rs_size*2-1);
    prf_data_bus: out prf_data_array(0 to rs_size*2-1); -- (busy) (16 BIT DATA)

    write_en: in std_logic_vector(0 to rob_size*2-1);
    write_reg: in addr_array(0 to rob_size*2-1);
    write_data: in prf_data_array(0 to rob_size*2-1);

    rr1,rr4,
    rr2,rr3,rr5,rr6:  out std_logic_vector(15 downto 0)
    -- for args: r2, r3, r5, r6 we return the addr of rename reg
    -- for dests: r1, r4 we return the new rename reg assigned to them
    );
end entity;

architecture renaming of rename_registers is

    -- constant n : integer := 128 --size of PRF
    type rat_vector is array(0 to 7) of integer;
    signal rat: rat_vector := (0, 1, 2, 3, 4, 5, 6, 7);
   -- variable tags : std_logic_vector() 


    type int_array is array(0 to prf_size - 1) of integer;
    type val_array is array(0 to prf_size - 1) of std_logic_vector(15 downto 0);
	signal counter: int_array := (others=>0);

    -- valid - implies valid prf entry
    -- busy - pending wb
    signal valid: std_logic_vector(0 to prf_size - 1) := (7 downto 0 =>  '1', others=>'0');
    signal busy: std_logic_vector(0 to prf_size - 1) := (others=>'0');
    signal value: val_array := (others=>(others=>'0'));
begin
    process(prf_addr_bus)
    variable index: integer;
    begin
        for i in 0 to rs_size*2-1 loop
            if prf_addr_bus(i)(16) = '0' then
                index := to_integer(unsigned(prf_addr_bus(i)(15 downto 0)));
                prf_data_bus(i)(16) <= not busy(index);
                prf_data_bus(i)(15 downto 0) <= value(index);
            else
                prf_data_bus(i) <= prf_addr_bus(i);
            end if;
        end loop;
    end process;    
    process(clk)
        variable trr1: integer;
        variable temp_rat: rat_vector;
        variable temp_counter: int_array;
        variable temp_valid: std_logic_vector(0 to prf_size - 1);
        variable temp_busy: std_logic_vector(0 to prf_size - 1);
        variable temp_value: val_array;
        variable rat_gr2, rat_gr3, rat_gr5, rat_gr6: integer;
    begin
        if falling_edge(clk) then
            temp_rat := rat;
            temp_valid := valid;
            temp_busy := busy;
            temp_value := value;
            temp_counter := counter;
            rat_gr2 := temp_rat(to_integer(unsigned(gr2)));
            rat_gr3 := temp_rat(to_integer(unsigned(gr3)));
            temp_counter(rat_gr2) := temp_counter(rat_gr2) + 1;
            temp_counter(rat_gr3) := temp_counter(rat_gr3) + 1;
            
            rr2 <= std_logic_vector(to_unsigned(rat_gr2, 16));
            rr3 <= std_logic_vector(to_unsigned(rat_gr3, 16));
            
            for i in 0 to prf_size-1 loop
                if temp_valid(i) = '0' then
                    temp_rat(to_integer(unsigned(gr1))) := i;
                    temp_counter(i) := 0;
                    temp_valid(i) := '1';
                    rr1 <= std_logic_vector(to_unsigned(i, 16));
                    temp_busy(i) := '1'; 
                    exit;
                end if;
            end loop;

            rat_gr5 := temp_rat(to_integer(unsigned(gr5)));
            rat_gr6 := temp_rat(to_integer(unsigned(gr6)));

            temp_counter(rat_gr5) := temp_counter(rat_gr5) + 1;
            temp_counter(rat_gr6) := temp_counter(rat_gr6) + 1;
            
            rr5 <= std_logic_vector(to_unsigned(rat_gr5, 16));
            rr6 <= std_logic_vector(to_unsigned(rat_gr6, 16));
            
            for i in 0 to prf_size-1 loop
                if temp_valid(i) = '0' then
                    temp_rat(to_integer(unsigned(gr4))) := i;
                    temp_counter(i) := 0;
                    temp_valid(i) := '1';
                    rr4 <= std_logic_vector(to_unsigned(i, 16));
                    temp_busy(i) := '1'; 
                    exit;
                end if;
            end loop;


            if (temp_busy(rat_gr2) = '0') then
                rr2 <= temp_value(rat_gr2);
            end if;
            if (temp_busy(rat_gr3) = '0') then
                rr3 <= temp_value(rat_gr3);
            end if;
            if (temp_busy(rat_gr5) = '0') then
                rr5 <= temp_value(rat_gr5);
            end if;
            if (temp_busy(rat_gr6) = '0') then
                rr6 <= temp_value(rat_gr6);
            end if;

            -- writing back to register file from the ROB
            for i in 0 to rob_size*2-1 loop
                if write_en(i) = '1' then
                    temp_value(to_integer(unsigned(write_reg(i)))) := write_data(i);
                    temp_busy(to_integer(unsigned(write_reg(i)))) := '0';
                end if;
            end loop;

            rat <= temp_rat;
            valid <= temp_valid;
            value <= temp_value;
            busy <= temp_busy;    
            counter <= temp_counter;  
        end if;
    end process;

end renaming;