library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;

-- r1 = r2 + r3
-- r4 = r5 + r6 

entity rename_registers is
    generic(
        prf_size: integer := 128
        -- there are 8 architectural registers
    ); 
    port(gr1,gr4,
    gr2,gr3,gr5,gr6: in unsigned(2 downto 0);
    reset, clk, stall: in std_logic;

    -- gr1-6 are the registers from the two lines of code above(in that order)

    rr1,rr4,
    rr2,rr3,rr5,rr6:  out unsigned(15 downto 0)
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
	signal counter: int_array := (others=>0);
    -- valid - implies valid prf entry
    -- busy - pending wb
    signal valid: std_logic_vector(0 to prf_size - 1) := (7 downto 0 =>  '1', others=>'0');
    signal busy: std_logic_vector(0 to prf_size - 1) := (others=>'0');
    signal value: int_array := (others=>0);
begin
    process(clk)
        variable trr1: integer;
        variable temp_rat: rat_vector;
        variable temp_counter: int_array;
        variable temp_valid: std_logic_vector(0 to prf_size - 1);
        variable temp_busy: std_logic_vector(0 to prf_size - 1);
        variable temp_value: int_array;
    begin
        if falling_edge(clk) then
            temp_rat := rat;
            temp_valid := valid;
            temp_busy := busy;
            temp_value := value;
            temp_counter := counter;

            temp_counter(temp_rat(to_integer(gr2))) := temp_counter(temp_rat(to_integer(gr2))) + 1;
            temp_counter(temp_rat(to_integer(gr3))) := temp_counter(temp_rat(to_integer(gr3))) + 1;
            
            rr2 <= to_unsigned(temp_rat(to_integer(gr2)), 16);
            rr3 <= to_unsigned(temp_rat(to_integer(gr3)), 16);
            
            for i in 0 to prf_size-1 loop
                if temp_valid(i) = '0' then
                    temp_rat(to_integer(gr1)) := i;
                    temp_counter(i) := 0;
                    temp_valid(i) := '1';
                    rr1 <= to_unsigned(i, 16);
                    temp_busy(i) := '1'; 
                    exit;
                end if;
            end loop;

            temp_counter(temp_rat(to_integer(gr5))) := temp_counter(temp_rat(to_integer(gr5))) + 1;
            temp_counter(temp_rat(to_integer(gr6))) := temp_counter(temp_rat(to_integer(gr6))) + 1;
            
            rr5 <= to_unsigned(temp_rat(to_integer(gr5)), 16);
            rr6 <= to_unsigned(temp_rat(to_integer(gr6)), 16);
            
            for i in 0 to prf_size-1 loop
                if temp_valid(i) = '0' then
                    temp_rat(to_integer(gr4)) := i;
                    temp_counter(i) := 0;
                    temp_valid(i) := '1';
                    rr4 <= to_unsigned(i, 16);
                    temp_busy(i) := '1'; 
                    exit;
                end if;
            end loop;


            if (temp_busy(temp_rat(to_integer(gr2))) = '0') then
                rr2 <= to_unsigned(temp_value(temp_rat(to_integer(gr2))), 16);
            end if;
            if (temp_busy(temp_rat(to_integer(gr3))) = '0') then
                rr3 <= to_unsigned(temp_value(temp_rat(to_integer(gr3))), 16);
            end if;
            if (temp_busy(temp_rat(to_integer(gr5))) = '0') then
                rr5 <= to_unsigned(temp_value(temp_rat(to_integer(gr5))), 16);
            end if;
            if (temp_busy(temp_rat(to_integer(gr6))) = '0') then
                rr6 <= to_unsigned(temp_value(temp_rat(to_integer(gr6))), 16);
            end if;

            rat <= temp_rat;
            valid <= temp_valid;
            value <= temp_value;
            busy <= temp_busy;    
            counter <= temp_counter;  
        end if;
    end process;

end renaming;