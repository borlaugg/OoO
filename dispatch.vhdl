library ieee ;
use ieee.std_logic_1164.all ;
USE ieee.numeric_std.ALL;

entity dispatch is
    port(
        clk, reset: in std_logic;  --system ip

        -- input coming from ID
        id_r_1, id_r_2, id_r_3, id_r_4, id_r_5, id_r_6: in std_logic_vector(3 downto 0); -- 1111 don't care
	    id_opcode_1, id_opcode_2: in std_logic_vector(3 downto 0);
	    id_alu_op_1, id_alu_op_2: in std_logic_vector(1 downto 0);
	    id_imm6_1, id_imm6_2 : in std_logic_vector(5 downto 0);
        id_imm9_1, id_imm9_2 : in std_logic_vector(8 downto 0);
	    id_pc_out_2, id_pc_out_1  : in std_logic_vector(15 downto 0);
        prf_r_1, prf_r_2, prf_r_3, prf_r_4, prf_r_5, prf_r_6: in std_logic_vector(15 downto 0);  --rr stage
        prf_v_2, prf_v_3, prf_v_5, prf_v_6 : in std_logic;  --from rr

        -- dispatch to regular rs (everything except load-store instructions)
        rs_pc_in_1, rs_pc_in_2:  out std_logic_vector(15 downto 0); --id stage
        rs_opcode_1, rs_opcode_2: out std_logic_vector(3 downto 0);  --id stage
        rs_alu_op_1, rs_alu_op_2: out std_logic_vector(1 downto 0);  -- id stage
        rs_imm6_1, rs_imm6_2: out std_logic_vector(5 downto 0);  -- id stage
        rs_imm9_1, rs_imm9_2 : out std_logic_vector(8 downto 0);  -- id stage
        rs_r_1, rs_r_2, rs_r_3, rs_r_4, rs_r_5, rs_r_6: out std_logic_vector(15 downto 0);  --rr stage
        rs_v_2, rs_v_3, rs_v_5, rs_v_6 : out std_logic;  --from rr
    
        -- dispatch to load store rs (only load-store instructions)
        ls_rs_pc_in_1, ls_rs_pc_in_2:  out std_logic_vector(15 downto 0); --id stage
        ls_rs_opcode_1, ls_rs_opcode_2: out std_logic_vector(3 downto 0); --id stage
        ls_rs_imm6_1, ls_rs_imm6_2: out std_logic_vector(5 downto 0);  -- id stage
        ls_rs_imm9_1, ls_rs_imm9_2 : out std_logic_vector(8 downto 0);  -- id stage
        ls_rs_r_a_1, ls_rs_r_b_1, ls_rs_r_a_2, ls_rs_r_b_2: out std_logic_vector(15 downto 0);  --rr stage
        ls_rs_v_a_1 ,ls_rs_v_a_2, ls_rs_v_b_1, ls_rs_v_b_2: out std_logic;  --from rr

        -- dispatch to rob (all instructions)
        rob_enable1, rob_enable2: out std_logic; -- enables for channels (each channel corresponds to one entry to be added)
        rob_r1, rob_r4: out std_logic_vector(15 downto 0); -- dest regs
        rob_rr1, rob_rr4: out std_logic_vector(15 downto 0); -- rename regs assigned to the instr
        rob_PC1, rob_PC4: out std_logic_vector(15 downto 0)
    );
end entity dispatch;

architecture dispatching of dispatch is
begin

    rob_r1(3 downto 0) <= id_r_1;
    rob_r4(3 downto 0) <= id_r_4;
    rob_r1(15 downto 4) <= (others=>'0');
    rob_r4(15 downto 4) <= (others=>'0');
    rob_rr1 <= prf_r_1;
    rob_rr4 <= prf_r_4;
    rob_PC1 <= id_pc_out_1;
    rob_PC4 <= id_pc_out_2;

    process(clk)
    begin
        if falling_edge(clk) then
            if(id_opcode_1="0111" or id_opcode_1="0101" or id_opcode_1="0011") then --ls instructions
                ls_rs_pc_in_1 <= id_pc_out_1;
                ls_rs_opcode_1 <= id_opcode_1;
                ls_rs_imm6_1 <= id_imm6_1;
                ls_rs_imm9_1 <= id_imm9_1;
                ls_rs_r_a_1 <= prf_r_2;
                ls_rs_r_b_1 <= prf_r_3;
                ls_rs_v_a_1 <= prf_v_2;
                ls_rs_v_b_1 <= prf_v_3;
            else
                rs_pc_in_1 <= id_pc_out_1;
                rs_opcode_1 <= id_opcode_1;
                rs_imm6_1 <= id_imm6_1;
                rs_imm9_1 <= id_imm9_1;
                rs_alu_op_1 <= id_alu_op_1;
                rs_r_1 <= prf_r_1;
                rs_r_2 <= prf_r_2;
                rs_r_3 <= prf_r_3;
                rs_v_3 <= prf_v_3;
                rs_v_2 <= prf_v_2;
            end if;
        end if;
    end process;
    
    process(clk)
    begin
        if falling_edge(clk) then
            if(id_opcode_2="0111" or id_opcode_2="0101" or id_opcode_2="0011") then--ls instructions
                ls_rs_pc_in_2 <= id_pc_out_2;
                ls_rs_opcode_2 <= id_opcode_2;
                ls_rs_imm6_2 <= id_imm6_2;
                ls_rs_imm9_2 <= id_imm9_2;
                ls_rs_r_a_2 <= prf_r_5;
                ls_rs_r_b_2 <= prf_r_6;
                ls_rs_v_a_2 <= prf_v_5;
                ls_rs_v_b_2 <= prf_v_6;
            else
                rs_pc_in_2 <= id_pc_out_2;
                rs_opcode_2 <= id_opcode_2;
                rs_imm6_2 <= id_imm6_2;
                rs_imm9_2 <= id_imm9_2;
                rs_alu_op_2 <= id_alu_op_2;
                rs_r_4 <= prf_r_4;
                rs_r_5 <= prf_r_5;
                rs_r_6 <= prf_r_6;
                rs_v_5 <= prf_v_5;
                rs_v_6 <= prf_v_6;
            end if;
        end if;
    end process;
end dispatching; 