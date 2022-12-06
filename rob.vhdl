LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.array_pkg.ALL;

package array_pkg is
    type addr_array is array(natural range <>) of std_logic_vector(16 downto 0);
    type data_array is array(natural range <>) of std_logic_vector(16 downto 0);
end package;

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
        clk, stall, reset: in std_logic;
 
        -- ports for adding to ROB (from the dispatch stage)
        enable1, enable2: in std_logic; -- enables for channels (each channel corresponds to one entry to be added)
        PC1, PC4: in std_logic_vector(15 downto 0); -- PC of the instruction
        r1, r4: in std_logic_vector(15 downto 0); -- dest regs
        rr1, rr4: in std_logic_vector(15 downto 0); -- rename regs assigned to the instr

        -- ports for writing back to the prf
        write_en: out std_logic_vector(0 to rs_size*2-1);
        write_reg: out addr_array(0 to rs_size*2-1);
        write_data: out data_array(0 to rs_size*2-1);

        -- inputs from the execution unit
        exec_inp_en: in std_logic_vector(0 to 1);
        exec_inp_pc: in array(0 to 1) of std_logic_vector(15 downto 0);
        exec_inp_vals: in array(0 to 1) of std_logic_vector(15 downto 0);

    );
end entity rob;

architecture juju of rob is
    -- type opcode_vector is array(0 to rob_size-1) of std_logic_vector(3 downto 0);
    -- type alu_op_vector is array(0 to rob_size-1) of std_logic_vector(1 downto 0);
    type bit16_vector is array(0 to rob_size-1) of std_logic_vector(15 downto 0);

    -- defining the cols of the ROB
    signal entry_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
    signal value: bit16_vector := (others=>'0');
    signal value_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
    signal dest_reg: bit16_vector := (others=>'0');
    signal rename_reg: bit16_vector := (others=>'0');
    signal PCs: bit16_vector := (others=>'0');
    -- signal branch_tag: bit16_vector := (others=>'0');

    -- other state variables
    signal head: integer := 0;
    signal insert_loc: integer := 0;
    signal is_full: std_logic := 0;

begin
    

    process(clk)
        -- defining the cols of the ROB
        variable temp_entry_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
        variable temp_value: bit16_vector := (others=>'0');
        variable temp_value_valid: std_logic_vector(0 to rob_size-1) := (others=>'0');
        variable temp_dest_reg: bit16_vector := (others=>'0');
        variable temp_rename_reg: bit16_vector := (others=>'0');
        signal temp_PCs: bit16_vector := (others=>'0');
        -- signal branch_tag: bit16_vector := (others=>'0');

        -- other state variables
        variable temp_head: integer := 0;
        variable temp_insert_loc: integer := 0;
        variable temp_is_full: std_logic := 0;

        -- ports for writing back to the prf
        variable temp_write_en: out std_logic_vector(0 to rs_size*2-1);
        variable temp_write_reg: out addr_array(0 to rs_size*2-1);
        variable temp_write_data: out data_array(0 to rs_size*2-1); 

        variable index: integer;
        variable count: integer;
        variable smolindex: integer;
        variable smolcounter: integer;

    begin  
        temp_entry_valid := entry_valid;
        temp_value := value; 
        temp_value_valid := value_valid; 
        temp_dest_reg := dest_reg; 
        temp_rename_reg := rename_reg; 
        temp_PCs := PCs;
        temp_head := head; 
        temp_insert_loc := insert_loc; 
        temp_is_full := is_full; 
        temp_write_en := write_en; 
        temp_write_reg := write_reg; 
        temp_write_data := write_data; 

        if falling_edge(clk) then
            -- filling in values gotten from the execution unit
            index := temp_head;
            count := 0;
            smolcounter := 0;
            while count <= (insert_loc - temp_head) % rob_size loop
                for smolindex in 0 to 1 loop
                    if (temp_entry_valid = '1' and temp_PCs(index) = exec_inp_pc(smolindex)) then
                        temp_value(index) := exec_inp_data(smolindex);
                        temp_value_valid(index) := '1';
                        smolcounter := smolcounter + 1;
                    end if;
                end loop;

                if smolcounter >= 2 then
                    exit;
                end if;

                count := count + 1;
                index := (index + 1) % rob_size;
            end loop;

            -- adding new entries loop
            if (insert_loc - temp_head) % rob_size /= rob_size - 1 then
                temp_entry_valid(temp_insert_loc) := '1';
                temp_value(temp_insert_loc) := (others=>'0');
                temp_value_valid(temp_insert_loc) := '0';
                temp_dest_reg(temp_insert_loc) := r1;
                temp_rename_reg(temp_insert_loc) := rr1;
                temp_PCs(temp_insert_loc) := PC1;
                temp_is_full := '0';

                temp_insert_loc := (temp_insert_loc + 1) mod rob_size;

                if (insert_loc - temp_head) % rob_size /= rob_size - 1 then
                    temp_entry_valid(temp_insert_loc) := '1';
                    temp_value(temp_insert_loc) := (others=>'0');
                    temp_value_valid(temp_insert_loc) := '0';
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

            -- writeback loop
            index := temp_head;
            count := 0;
            while count <= (insert_loc - temp_head) % rob_size loop
                if temp_entry_valid(index) = '1' and temp_value_valid(index) = '1' then
                    temp_write_en(count) := '1';
                    temp_write_reg(count) := temp_rename_reg(index);
                    temp_write_data(count) := temp_value(index);
                    temp_entry_valid(index) := '0';
                    temp_value_valid(index) := '0';
                    temp_value(index) := (others=>'0');
                    temp_dest_reg(index) := (others=>'0');
                    temp_rename_reg(index) := (others=>'0');
                    temp_PCs(index) := (others=>'0');
                    temp_head := (temp_head + 1) % rob_size;
                    temp_is_full := '0';
                else 
                    exit;
                end if;

                count := count + 1;
                index := (index + 1) % rob_size;
            end loop;

        end if;

        entry_valid <= temp_entry_valid;
        value <= temp_value; 
        value_valid <= temp_value_valid; 
        dest_reg <= temp_dest_reg; 
        rename_reg <= temp_rename_reg; 
        PCs <= temp_PCs;
        head <= temp_head; 
        insert_loc <= temp_insert_loc; 
        is_full <= temp_is_full; 
        write_en <= temp_write_en; 
        write_reg <= temp_write_reg; 
        write_data <= temp_write_data; 
    end process;
end architecture;
