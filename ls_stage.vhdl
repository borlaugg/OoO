-- 3 ex
-- 1 alu + 1 l/s + branching

library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;
use work.array_pkg.all;
entity ls_rs_stage is
    generic(
        rs_size: integer := 256
        -- there are 8 architectural registers
    ); 
    port(
        clk, stall, reset: in std_logic;  --system ip
        pc_in_1, pc_in_2:  in std_logic_vector(15 downto 0); --id stage
        opcode_1, opcode_2: in std_logic_vector(3 downto 0); --id stage
        imm6_1, imm6_2: in std_logic_vector(5 downto 0);  -- id stage
        imm9_1, imm9_2 : in std_logic_vector(8 downto 0);  -- id stage
        r_a_1, r_b_1, r_a_2, r_b_2: in std_logic_vector(15 downto 0);  --rr stage
        v_a_1, v_a_2, v_b_1, v_b_2: in std_logic;  --from rr
        prf_data_bus: in prf_data_array(0 to rs_size*2-1);-- (busy) (16 BIT DATA)  --from prf
        prf_addr_bus: out addr_array(0 to rs_size*2-1); 

        mem_addr_bus: out std_logic_vector(15 downto 0); -- memory read addr
        mem_read_data_bus: in std_logic_vector(15 downto 0); -- from data memory
        mem_write_en: out std_logic; -- to memory to enable write
        mem_write_data_bus: out std_logic_vector(15 downto 0);

        prf_wb_addr: out std_logic_vector(15 downto 0); -- addr for writing
        prf_wb_en: out std_logic; -- to memory to enable write
        prf_wb_data: out std_logic_vector(15 downto 0)

    );
end entity ls_rs_stage;

architecture bham2 of ls_rs_stage is
    type opcode_vector is array(0 to rs_size-1) of std_logic_vector(3 downto 0);
    type opr_vector is array(0 to rs_size-1) of std_logic_vector(15 downto 0);
    type imm6_vector is array(0 to rs_size-1) of std_logic_vector(5 downto 0);
    type imm9_vector is array(0 to rs_size-1) of std_logic_vector(8 downto 0);
    type count_vector is array(0 to rs_size-1) of integer;
    signal opcode: opcode_vector := (others=>(others=>'0')); 

    signal opr_1: opr_vector := (others=>(others=>'0')); 
    signal opr_2: opr_vector := (others=>(others=>'0')); 

    signal dest: opr_vector := (others=>(others=>'0')); 
    -- val : validity of rs entry
    signal val: std_logic_vector(0 to rs_size-1)  := (others=>'0');

    signal val_1: std_logic_vector(0 to rs_size-1)  := (others=>'0');
    signal val_2: std_logic_vector(0 to rs_size-1)  := (others=>'0');

    signal imm6: imm6_vector := (others=>(others=>'0')); 
    signal imm9: imm9_vector  := (others=>(others=>'0'));
    signal head_pointer: integer := 0;
    signal tail_pointer: integer := 0;
begin
    process(clk)
    variable temp_opcode: opcode_vector := (others=>(others=>'0'));
    
    variable temp_opr_1: opr_vector := (others=>(others=>'0')); 
    variable temp_opr_2: opr_vector := (others=>(others=>'0'));
    variable temp_dest: opr_vector := (others=>(others=>'0')); 
    -- val : validity of rs entry
    variable temp_val: std_logic_vector(0 to rs_size-1)  := (others=>'0');

    variable temp_val_1: std_logic_vector(0 to rs_size-1)  := (others=>'0');
    variable temp_val_2: std_logic_vector(0 to rs_size-1)  := (others=>'0');

    variable temp_imm6: imm6_vector := (others=>(others=>'0')); 
    variable temp_imm9: imm9_vector  := (others=>(others=>'0'));

    variable temp_head_pointer: integer := 0;
    variable temp_tail_pointer: integer := 0;
    begin 

        if falling_edge(clk) then
            mem_write_en <= '0';
            prf_wb_en <= '0';
            temp_opcode := opcode;
            temp_opr_1 := opr_1;
            temp_opr_2 := opr_2;
            temp_dest := dest;
            temp_val := val;
            temp_val_1 := val_1;
            temp_val_2 := val_2;
            temp_imm6 := imm6;
            temp_imm9 := imm9;
            temp_head_pointer := head_pointer;
            temp_tail_pointer := tail_pointer;
            prf_wb_en <= '0';
            if temp_val(temp_head_pointer) = '1' then
                -- wb initiate
                if temp_opcode(temp_head_pointer) = "1100" and temp_val_1(temp_head_pointer) = '1' and temp_val_2(temp_head_pointer) = '1' then
                    mem_write_data_bus <= temp_opr_1(temp_head_pointer);
                    mem_addr_bus <= std_logic_vector(unsigned(temp_opr_2(temp_head_pointer))+unsigned(temp_imm6(temp_head_pointer)));
                    mem_write_en <= '1';
                    temp_val(temp_head_pointer) := '0';
                    temp_head_pointer := (temp_head_pointer+1) rem rs_size;
                end if;

                if temp_opcode(temp_head_pointer) = "0101" and temp_val_2(temp_head_pointer) = '1' then
                    mem_addr_bus <= std_logic_vector(unsigned(temp_opr_2(temp_head_pointer))+unsigned(temp_imm6(temp_head_pointer)));
                    prf_wb_data <= mem_read_data_bus;
                    prf_wb_addr <= temp_dest(temp_head_pointer);
                    prf_wb_en <= '1';
                    temp_val(temp_head_pointer) := '0';
                    temp_head_pointer := (temp_head_pointer+1) rem rs_size;
                end if;

                if temp_opcode(temp_head_pointer) = "0011" and temp_val_2(temp_head_pointer) = '1' then
                    prf_wb_data(8 downto 0) <= temp_imm9(temp_head_pointer);
                    prf_wb_data(15 downto 9) <= (others=>'0');
                    prf_wb_addr <= temp_dest(temp_head_pointer);
                    prf_wb_en <= '1';
                    temp_val(temp_head_pointer) := '0';
                    temp_head_pointer := (temp_head_pointer+1) rem rs_size;
                end if;
            end if;

            if opcode_1 = "1100" then
                temp_opcode(temp_tail_pointer) := opcode_1;
                temp_opr_1(temp_tail_pointer) := r_a_1;
                temp_opr_2(temp_tail_pointer) := r_b_1;
                temp_dest(temp_tail_pointer) := r_a_1;
                temp_val(temp_tail_pointer) := '1';
                temp_val_1(temp_tail_pointer) := v_a_1;
                temp_val_2(temp_tail_pointer) := v_b_1;
                temp_imm6(temp_tail_pointer) := imm6_1;
                temp_imm9(temp_tail_pointer) := imm9_1;
                temp_tail_pointer := (temp_tail_pointer+1) rem rs_size;
            end if;

            if opcode_1 = "0101" then
                temp_opcode(temp_tail_pointer) := opcode_1;
                temp_opr_1(temp_tail_pointer) := (others=>'1');
                temp_opr_2(temp_tail_pointer) := r_b_1;
                temp_dest(temp_tail_pointer) := r_a_1;
                temp_val(temp_tail_pointer) := '1';
                temp_val_1(temp_tail_pointer) := v_a_1;
                temp_val_2(temp_tail_pointer) := v_b_1;
                temp_imm6(temp_tail_pointer) := imm6_1;
                temp_imm9(temp_tail_pointer) := imm9_1;
                temp_tail_pointer := (temp_tail_pointer+1) rem rs_size;
            end if;

            if opcode_1 = "0011" then
                temp_opcode(temp_tail_pointer) := opcode_1;
                temp_opr_1(temp_tail_pointer) := (others=>'1');
                temp_opr_2(temp_tail_pointer) := (others=>'1');
                temp_dest(temp_tail_pointer) := r_a_1;
                temp_val(temp_tail_pointer) := '1';
                temp_val_1(temp_tail_pointer) := v_a_1;
                temp_val_2(temp_tail_pointer) := v_b_1;
                temp_imm6(temp_tail_pointer) := imm6_1;
                temp_imm9(temp_tail_pointer) := imm9_1;
                temp_tail_pointer := (temp_tail_pointer+1) rem rs_size;
            end if;

            if opcode_2 = "1100" then
                temp_opcode(temp_tail_pointer) := opcode_2;
                temp_opr_1(temp_tail_pointer) := r_a_2;
                temp_opr_2(temp_tail_pointer) := r_b_2;
                temp_dest(temp_tail_pointer) := r_a_2;
                temp_val(temp_tail_pointer) := '1';
                temp_val_1(temp_tail_pointer) := v_a_2;
                temp_val_2(temp_tail_pointer) := v_b_2;
                temp_imm6(temp_tail_pointer) := imm6_2;
                temp_imm9(temp_tail_pointer) := imm9_2;
                temp_tail_pointer := (temp_tail_pointer+1) rem rs_size;
            end if;

            if opcode_2 = "0101" then
                temp_opcode(temp_tail_pointer) := opcode_2;
                temp_opr_1(temp_tail_pointer) := (others=>'1');
                temp_opr_2(temp_tail_pointer) := r_b_2;
                temp_dest(temp_tail_pointer) := r_a_2;
                temp_val(temp_tail_pointer) := '1';
                temp_val_1(temp_tail_pointer) := v_a_2;
                temp_val_2(temp_tail_pointer) := v_b_2;
                temp_imm6(temp_tail_pointer) := imm6_2;
                temp_imm9(temp_tail_pointer) := imm9_2;
                temp_tail_pointer := (temp_tail_pointer+1) rem rs_size;
            end if;

            if opcode_2 = "0011" then
                temp_opcode(temp_tail_pointer) := opcode_2;
                temp_opr_1(temp_tail_pointer) := (others=>'1');
                temp_opr_2(temp_tail_pointer) := (others=>'1');
                temp_dest(temp_tail_pointer) := r_a_2;
                temp_val(temp_tail_pointer) := '1';
                temp_val_1(temp_tail_pointer) := v_a_2;
                temp_val_2(temp_tail_pointer) := v_b_2;
                temp_imm6(temp_tail_pointer) := imm6_2;
                temp_imm9(temp_tail_pointer) := imm9_2;
                temp_tail_pointer := (temp_tail_pointer+1) rem rs_size;
            end if;

            for i in 0 to rs_size-1 loop
                prf_addr_bus(i*2)(16) <= temp_val_1(i);
                prf_addr_bus(i*2)(15 downto 0) <= temp_opr_1(i);
                prf_addr_bus(i*2+1)(16) <= temp_val_2(i);
                prf_addr_bus(i*2+1)(15 downto 0) <= temp_opr_2(i);
            end loop;
            
            for i in 0 to rs_size-1 loop
                if temp_val(i) = '1' then
                    temp_val_1(i) := prf_data_bus(i*2)(16);
                    temp_opr_1(i) := prf_data_bus(i*2)(15 downto 0);
                    temp_val_2(i) := prf_data_bus(i*2+1)(16);
                    temp_opr_2(i) := prf_data_bus(i*2+1)(15 downto 0);
                end if;
            end loop;
            opcode <= temp_opcode;
            opr_1 <= temp_opr_1;
            opr_2 <= temp_opr_2;
            dest <= temp_dest;
            val <= temp_val;
            val_1 <= temp_val_1;
            val_2 <= temp_val_2;
            imm6 <= temp_imm6;
            imm9 <= temp_imm9;

            head_pointer <= temp_head_pointer;
            tail_pointer <= temp_tail_pointer;

        end if;
    end process;
end architecture;