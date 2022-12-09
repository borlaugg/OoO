LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;


library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;
use work.array_pkg.all;

-- maintainig the context of the instructions
-- r1 = r2 op r3
-- r4 = r5 op r6
-- r1 and r4 are the dest regs

entity rob is
    generic(
        rob_size: integer := 20
        -- there are 8 architectural registers
    ); 
    port(
        clk, stall, reset, exec: in std_logic; --exec is bp taken or not

        -- ports for adding to ROB (from the dispatch stage)(actually coming from rs)
        enable1, enable2: in std_logic; -- enables for channels (each channel corresponds to one entry to be added)
        r1, r4: in std_logic_vector(15 downto 0); -- dest regs
        rr1, rr4: in std_logic_vector(15 downto 0); -- rename regs assigned to the instr
        PC1, PC4, PC_BP1, PC_BP2: in std_logic_vector(15 downto 0);

        -- ports for writing back to the prf
        write_en: out std_logic_vector(0 to rob_size-1);
        write_rreg: out bit16_vector(0 to rob_size-1);
        write_destreg: out bit16_vector(0 to rob_size-1);

        -- to prf
        addr_bus: out addr_array(0 to rob_size-1);
        busy_bus: in std_logic_vector(0 to rob_size-1)

        -- inputs from the execution unit
        -- exec_inp_en: in std_logic_vector(0 to 1);
        -- exec_inp_pc: in exec_inps;
        -- exec_inp_vals: in exec_inps

    );
end entity rob;

architecture idnar of rob is
    -- type opcode_vector is array(0 to rob_size-1) of std_logic_vector(3 downto 0);
    -- type alu_op_vector is array(0 to rob_size-1) of std_logic_vector(1 downto 0);

    -- defining the cols of the ROB
    signal entry_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
    signal spec : std_logic_vector(0 to rob_size-1) := (others=>'0');
    -- signal value: bit16_vector(0 to rob_size - 1) := (others => (others=>'0'));
    -- signal value_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
    signal dest_reg: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
    signal rename_reg: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
    signal PCs: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
    -- signal branch_tag: bit16_vector(0 to rob_size-1) := (others=>'0');

    -- other state variables
    signal head: integer := 0;
    signal insert_loc: integer := 0;
    signal is_full: std_logic := '0';

begin
    

    process(clk)
        -- defining the cols of the ROB
        variable temp_entry_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
        variable temp_spec : std_logic_vector(0 to rob_size-1) := (others=>'0');
        -- variable temp_value: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
        -- variable temp_value_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
        variable temp_dest_reg: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
        variable temp_rename_reg: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
        variable temp_PCs: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
        -- signal branch_tag: bit16_vector(0 to rob_size-1) := (others=>'0');

        -- other state variables
        variable temp_head: integer := 0;
        variable temp_insert_loc: integer := 0;
        variable temp_is_full: std_logic := '0';

        -- ports for writing back to the pr
        variable temp_write_en: std_logic_vector(0 to rob_size-1) := (others=>'0');
        variable temp_write_rreg: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));
        variable temp_write_destreg: bit16_vector(0 to rob_size-1) := (others => (others=>'0'));

        variable index: integer;
        variable count: integer;
        -- variable smolindex: integer;
        variable smolcounter: integer;

    begin  
        temp_entry_valid := entry_valid;
        -- temp_value := value; 
        -- temp_value_valid := value_valid; 
        temp_dest_reg := dest_reg; 
        temp_rename_reg := rename_reg; 
        temp_spec := spec;
        -- temp_PCs := PCs;
        temp_head := head; 
        temp_insert_loc := insert_loc; 
        temp_is_full := is_full; 
        temp_write_en := (others=>'0'); 
        temp_write_rreg := (others => (others=>'0')); 
        temp_write_destreg := (others => (others=>'0')); 

        if falling_edge(clk) then
            -- filling in values gotten from the execution unit
            -- index := temp_head;
            -- count := 0;
            -- smolcounter := 0;
            -- while count <= (insert_loc - temp_head) rem rob_size loop
            --     for smolindex in 0 to 1 loop
            --         if (exec_inp_en(smolindex) = '1' and temp_entry_valid(index) = '1' and temp_PCs(index) = exec_inp_pc(smolindex)) then
            --             temp_value(index) := exec_inp_vals(smolindex);
            --             temp_value_valid(index) := '1';
            --             smolcounter := smolcounter + 1;
            --         end if;
            --     end loop;

            --     if smolcounter >= 2 then
            --         exit;
            --     end if;

            --     count := count + 1;
            --     index := (index + 1) rem rob_size;
            -- end loop;

            -- adding new entries loop
            if (insert_loc - temp_head) rem rob_size /= rob_size - 1 then
                temp_entry_valid(temp_insert_loc) := '1';
                -- temp_value(temp_insert_loc) := (others=>'0');
                -- temp_value_valid(temp_insert_loc) := '0';
                temp_dest_reg(temp_insert_loc) := r1;
                temp_rename_reg(temp_insert_loc) := rr1;
                temp_PCs(temp_insert_loc) := PC1;
                temp_is_full := '0';

                temp_insert_loc := (temp_insert_loc + 1) mod rob_size;

                if (insert_loc - temp_head) rem rob_size /= rob_size - 1 then
                    temp_entry_valid(temp_insert_loc) := '1';
                    -- temp_value(temp_insert_loc) := (others=>'0');
                    -- temp_value_valid(temp_insert_loc) := '0';
                    temp_dest_reg(temp_insert_loc) := r4;
                    temp_rename_reg(temp_insert_loc) := rr4;
                    temp_PCs(temp_insert_loc) := PC4;
                    temp_is_full := '0';

                    temp_insert_loc := (temp_insert_loc + 1) mod rob_size;
                else
                    temp_is_full := '1';
                end if;

            else
                temp_is_full := '1';
            end if;

            for i in 0 to rob_size-1 loop
                addr_bus(i)(16) <= temp_entry_valid(i);
                addr_bus(i)(15 downto 0) <= temp_rename_reg(i);
            end loop;

            -- writeback loop
            index := temp_head;
            count := 0;
            while count <= (insert_loc - temp_head) rem rob_size loop
                if temp_entry_valid(index) = '1' and busy_bus(index) = '0' then
                    temp_write_en(count) := '1';
                    temp_write_rreg(count) := temp_rename_reg(index);
                    temp_write_destreg(count) := temp_dest_reg(index);

                    temp_entry_valid(index) := '0';
                    -- temp_value_valid(index) := '0';
                    -- temp_value(index) := (others=>'0');
                    temp_dest_reg(index) := (others=>'0');
                    temp_rename_reg(index) := (others=>'0');
                    temp_PCs(index) := (others=>'0');
                    temp_head := (temp_head + 1) rem rob_size;
                    temp_is_full := '0';
                else 
                    exit;
                end if;

                count := count + 1;
                index := (index + 1) rem rob_size;
            end loop;

        end if;

        for i in 0 to rob_size-1 loop      -- speculate code     
            if (to_integer(unsigned(PC1)) > to_integer(unsigned(PC_BP1))) then
                temp_spec(i) := '1';
            elsif (to_integer(unsigned(PC1)) > to_integer(unsigned(PC_BP2))) then
                temp_spec(i) := '1';
            else 
                temp_spec(i) := '0';
            end if;
            if (to_integer(unsigned(PC4)) > to_integer(unsigned(PC_BP1))) then
                temp_spec(i) := '1';
            elsif (to_integer(unsigned(PC4)) > to_integer(unsigned(PC_BP2))) then
                temp_spec(i) := '1';
            else 
                temp_spec(i) := '0';
            end if;
        end loop;

        if ((stall = '1' and exec = '0') or (stall = '0' and exec = '1') ) then --handles mispred
            for i in 0 to rob_size-1 loop 
                if (temp_spec(i)='1') then
                    temp_entry_valid(i) := '0';
                end if;
            end loop;
        end if;

        entry_valid <= temp_entry_valid;
        -- value <= temp_value; 
        -- value_valid <= temp_value_valid; 
        dest_reg <= temp_dest_reg; 
        rename_reg <= temp_rename_reg; 
        PCs <= temp_PCs;
        head <= temp_head; 
        spec <= temp_spec;
        insert_loc <= temp_insert_loc; 
        is_full <= temp_is_full; 
        write_en <= temp_write_en; 
        write_rreg <= temp_write_rreg; 
        write_destreg <= temp_write_destreg; 
    end process;
end architecture;
