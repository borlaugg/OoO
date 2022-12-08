library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.array_pkg.all;

package rob_array_pkg is
    type bit16_vector is array(natural range <>) of std_logic_vector(15 downto 0);
    type exec_inps is array(0 to 1) of std_logic_vector(15 downto 0);
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.array_pkg.all;
use work.rob_array_pkg.all;

entity OoO_core is
    port (clk, reset : in std_logic
    );
end entity;

architecture superscalar of OoO_core is

    component IF_STAGE is
        port(clk, stall,rst,unstall: in std_logic;
            mem_data_in_1, mem_data_in_2, dest_pc,pc_in: in std_logic_vector(15 downto 0);
            instr_1, instr_2, mem_addr_1, mem_addr_2, pc_out, pc_out_1, pc_out_2: out std_logic_vector(15 downto 0)
            );
    end component;

    component Increment port (pc_in: in std_logic_vector(15 downto 0);
                              pc_out out std_logic_vector(15 downto 0));
    end component;

    component bpt is    
    port (rst, clk, b_obs, unstall: in std_logic;
          opcode: in  std_logic_vector(3 downto 0);
          pc_in: in std_logic_vector(15 downto 0);
          b_pred: out std_logic;
          pc_out: out std_logic_vector(15 downto 0)
        );
    end component;

    component ID_STAGE is
        port(clk: in std_logic;
        instr_in_1, instr_in_2, pc_in_1, pc_in_2: in std_logic_vector(15 downto 0);
        r_1, r_2, r_3, r_4, r_5, r_6: out std_logic_vector(3 downto 0); -- 1111 don't care
        opcode_1, opcode_2: out std_logic_vector(3 downto 0);
        alu_op_1, alu_op_2: out std_logic_vector(1 downto 0);
        imm6_1, imm6_2 : out std_logic_vector(5 downto 0);
        imm9_1, imm9_2 : out std_logic_vector(8 downto 0);
        pc_out_2, pc_out_1  : out std_logic_vector(15 downto 0)
	);
    end component;

    component rename_registers is
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

                -- prf inputs from rob:
                rob_write_en: in std_logic_vector(0 to rob_size-1);
                rob_write_rreg: in bit16_vector(0 to rob_size-1);
                rob_write_destreg: in bit16_vector(0 to rob_size-1);

                --prf inputs from alu wb:
                alu1_reg_data: in std_logic_vector(15 downto 0);
                alu1_reg_addr:  in std_logic_vector(15 downto 0);
                alu1_reg_en: in std_logic;

                rr1,rr4,
                rr2,rr3,rr5,rr6:  out std_logic_vector(15 downto 0);
                v2,v3,v5,v6: out std_logic
                -- for args: r2, r3, r5, r6 we return the addr of rename reg
                -- for dests: r1, r4 we return the new rename reg assigned to them

                -- to rob
                rob_addr_bus: in addr_array(0 to rob_size-1);
                rob_busy_bus: out std_logic_vector(0 to rob_size-1);
    );
    end component;

    component rs_stage is
        generic(
            rs_size: integer := 16
            -- there are 8 architectural registers
        ); 
        port(
            clk, reset: in std_logic;  --system ip
            pc_in_1, pc_in_2:  in std_logic_vector(15 downto 0); --id stage
            opcode_1, opcode_2: in std_logic_vector(3 downto 0);  --id stage
            alu_op_1, alu_op_2: in std_logic_vector(1 downto 0);  -- id stage
            imm6_1, imm6_2: in std_logic_vector(5 downto 0);  -- id stage
            imm9_1, imm9_2 : in std_logic_vector(8 downto 0);  -- id stage
            r_1, r_2, r_3, r_4, r_5, r_6: in std_logic_vector(15 downto 0);  --rr stage
            v_2, v_3, v_5, v_6 : in std_logic;  --from rr
            prf_data_bus: in prf_data_array(0 to rs_size*2-1);-- (busy) (16 BIT DATA)  --from prf
            prf_addr_bus: out addr_array(0 to rs_size*2-1);  --from prf
            alu1_dest , ls1_dest : out std_logic_vector(15 downto 0);  
            alu1_oper1, alu1_oper2, ls1_oper: out std_logic_vector(15 downto 0);  
            alu1_imm6 : out std_logic_vector(5 downto 0);
            alu1_pc : out std_logic_vector(15 downto 0);
            ls1_imm6 : out std_logic_vector(5 downto 0);
            ls1_imm9 : out std_logic_vector(8 downto 0);
            ls1_pc : out std_logic_vector(15 downto 0);
            br1_mode: out std_logic_vector(3 downto 0);
            br1_oper1, br1_oper2 : out std_logic_vector(15 downto 0);
            br1_imm6 : out std_logic_vector(5 downto 0);
            br1_imm9 : out std_logic_vector(8 downto 0);
            br1_pc : out std_logic_vector(15 downto 0);
            alu1_mode: out std_logic_vector(5 downto 0);
            ls1_mode: out std_logic_vector(3 downto 0);
            branch_mode: out std_logic_vector(2 downto 0)
    );
    end component;

    component exec_unit is
        port(
            clk, stall, reset: in std_logic;    
            alu1_dest , ls1_dest : in std_logic_vector(15 downto 0);
            alu1_oper1, alu1_oper2, ls1_oper: in std_logic_vector(15 downto 0);
            alu1_imm6 : in std_logic_vector(5 downto 0);

            ls1_imm6 : in std_logic_vector(5 downto 0);
            ls1_imm9 : in std_logic_vector(8 downto 0);
            br1_mode: in std_logic_vector(3 downto 0);
            br1_oper1, br1_oper2, br1_pc_in, ls1_pc_in, alu1_pc_in  : in std_logic_vector(15 downto 0); --need 2 operands to check for BEQ
            br1_imm6 : in std_logic_vector(5 downto 0);
            br1_imm9 : in std_logic_vector(8 downto 0);
            alu1_mode: in std_logic_vector(5 downto 0);
            ls1_mode: in std_logic_vector(3 downto 0);
            branch_mode: in std_logic_vector(2 downto 0);
            stall_pc1, stall_pc2 : in std_logic_vector(15 downto 0);

            ls_dest,ls_value : out std_logic_vector(15 downto 0);
            br_value, br_pc, alu_pc, ls_pc : out std_logic_vector(15 downto 0);
            br_mode: out std_logic_vector(3 downto 0);
            ls_mode: out std_logic_vector(3 downto 0);
            alu_c, alu_z: out std_logic;
            br_c, br_z: out std_logic;
            ls_c, ls_z: out std_logic;
            br_eq : out std_logic;
            stall_out : out std_logic;
            --connections to prf
            alu1_reg_data: out std_logic_vector(15 downto 0);
            alu1_reg_addr: out std_logic_vector(15 downto 0);
            alu1_reg_en: out std_logic
        );
    end component;

    component rob is
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
            write_en: out std_logic_vector(0 to rob_size*2-1);
            write_reg: out bit16_vector(0 to rob_size*2-1);
            write_data: out bit16_vector(0 to rob_size*2-1);
    
            -- inputs from the execution unit
            exec_inp_en: in std_logic_vector(0 to 1);
            exec_inp_pc: in exec_inps;
            exec_inp_vals: in exec_inps
    
        );
    end component;

    component InstructionMemory is
        port(clk: in std_logic;
              addr_1, addr_2: in std_logic_vector(15 downto 0);
              data_out_1, data_out_2: out std_logic_vector(15 downto 0)
        );
    end component;

    signal id_pc1, id_pc2, br_value,pc_i, pc_o, pc1, pc2, pco1, pco2, do1, do2, a1, a2 : std_logic_vector(15 downto 0) := (others=>'0');
    signal stall1, stall2, stall, unstall, b_obs1, b_obs2 : std_logic := '0';
    signal id_r1, id_r2, id_r3, id_r4, id_r5, id_r6 : std_logic_vector(3 downto 0) := (others=>'0');
    signal id_opcode1, id_opcode2 : std_logic_vector(3 downto 0) := (others=>'0');
    signal id_imm6_1, id_imm_6_2 : std_logic_vector(5 downto 0) := (others=>'0');
    signal id_imm9_1, id_imm_9_2 : std_logic_vector(8 downto 0) := (others=>'0');
    signal id_alu_op1, id_alu_op2 : std_logic_vector(1 downto 0) := (others=>'0');
    signal rr_1,rr_2,rr_3,rr_4,rr_5,rr_6 : std_logic_vector(15 downto 0);
    signal rrv_2,rrv_3,rrv_5,rrv_6 : std_logic;
    signal rs_alu1_dest,rs_ls1_dest,rs_alu1_oper1,rs_alu1_oper2,rs_ls1_oper,rs_alu1_pc,rs_ls1_pc,rs_br1_pc,rs_br1_oper1,rs_br1_oper2 : std_logic_vector(15 downto 0);
    signal rs_ls1_imm9,rs_br1_imm9 : std_logic_vector(8 downto 0); 
    signal rs_alu1_imm6,rs_ls1_imm6,rs_br1_imm6,rs_alu1_mode : std_logic_vector(5 downto 0);
    signal rs_ls1_mode,rs_br1_mode : std_logic_vector(3 downto 0);
    signal rs_branch_mode : std_logic_vector(2 downto 0);
    signal wb_alu1_reg_data, wb_alu1_reg_addr : std_logic_vector(15 downto 0);
    signal wb_alu1_reg_en : std_logic;
    signal ex_ls_value, ex_ls_addr, ex_br_pc, ex_alu_pc, ex_ls_pc : std_logic_vector(15 downto 0);
    signal ex_br_mode, ex_ls_mode : std_logic_vector(3 downto 0);
    signal ex_alu_c, ex_alu_z, ex_ls_c, ex_ls_z, ex_br_c, ex_br_z : std_logic;

begin

ins_mem : InstructionMemory port map(clk => clk, addr_1 => a1,addr_2 => a2, data_out_1 => do1,data_out_2 => do2);

if_block: IF_STAGE port map(clk => clk, stall => stall, rst => rst, unstall => unstall,
            mem_data_in_1 => do1, mem_data_in_2 => do2, pc_in => pc_i, dest_pc =>br_value,
            instr_1 =>, instr_2 =>, mem_addr_1 =>, a1 mem_addr_2 => a2, pc_out => pc_o, pc_out_1 => pc1, pc_out_2 => pc2);

incr_block: Increment port map(pc_in => pc_o, pc_out => pc_i);

bp_block1: bpt port map (rst => rst, clk => clk, b_obs => b_obs1, opcode => pc_out_1(15 downto 12), b_pred => stall1, unstall => unstall
                        pc_in => pc1, pc_out =>pco1);

bp_block2: bpt port map (rst => rst, clk => clk, b_obs =>b_obs2, opcode => pc_out_2(15 downto 12), b_pred => stall2,  unstall => unstall
                        pc_in => pc2, pc_out =>pco2);

stall <= stall1 or stall2;

id_block: ID_STAGE port map(clk => clk, 
        instr_in_1 => do1, instr_in_2 =>do2,  pc_in_1 => pc_out_1, pc_in_2 =>pc_out_2,
        r_1 => id_r1, r_2 => id_r2, r_3 => id_r3, r_4 => id_r4, r_5 => id_r5, r_6 => id_r6,
        opcode_1 => id_opcode1, opcode_2 => id_opcode2,
        alu_op_1 => id_alu_op1, alu_op_2 => id_alu_op2,
        imm6_1 => id_imm_6_1, imm6_2 => id_imm_6_2,
        imm9_1 => id_imm_9_1, imm9_2 => id_imm_9_2,
        pc_out_2 => id_pc2, pc_out_1 => id_pc1);
        
prf_block : rename_registers port map(gr1 => id_r1,gr4 => id_r4,
        gr2 => id_r2,gr3 => id_r3,gr5 => id_r5,gr6 => id_r6,
        reset => rst, clk => clk, stall => stall,

        -- gr1-6 are the registers from the two lines of code above(in that order)
        prf_addr_bus => ,
        prf_data_bus => , -- (busy) (16 BIT DATA)

        rob_write_en =>,
        rob_write_rreg =>,
        rob_write_destreg =>,

         --prf inputs from alu wb:
        alu1_reg_data => wb_alu1_reg_data,
        alu1_reg_addr => wb_alu1_reg_addr,
        alu1_reg_en => wb_alu1_reg_en,

        rob_addr_bus =>,
        rob_busy_bus =>,

        rr1 => rr_1, rr4 => rr_4,
        rr2 => rr_2, rr3 => rr_3, rr5 => rr_5, rr6 => rr_6,
        v2 => rrv_2,v3 => rrv_3, v5 => rrv_5,v6 => rrv_6
        -- for args: r2, r3, r5, r6 we return the addr of rename reg
        -- for dests: r1, r4 we return the new rename reg assigned to them
);

rs_block: rs_stage port map (
        clk=> clk, reset=> rst,
        pc_in_1 => id_pc1, pc_in_2 => id_pc2,
        opcode_1 => id_opcode1, opcode_2 => id_opcode2,
        alu_op_1=> id_alu_op1, alu_op_2 => id_alu_op1,
        imm6_1 => id_imm_6_1, imm6_2 => id_imm_6_2,
        imm9_1 => id_imm_9_1, imm9_2 => id_imm_9_2, 
        r_1 => rr_1, r_2=> rr_2, r_3 => rr_3, r_4 => rr_4, r_5 => rr_5, r_6 => rr_6,
        v_2 => rrv_2, v_3 => rrv_3, v_5 => rrv_5, v_6 => rrv_6,
        prf_data_bus=>,
        prf_addr_bus=>,
        alu1_dest => rs_alu1_dest, ls1_dest => rs_ls1_dest,
        alu1_oper1 => rs_alu1_oper1, alu1_oper2 => rs_alu1_oper2, ls1_oper => rs_ls1_oper,
        alu1_imm6 => rs_alu1_imm6,
        alu1_pc => rs_alu1_pc,
        ls1_imm6 => rs_ls1_imm6,
        ls1_imm9 => rs_ls1_imm9,
        ls1_pc => rs_ls1_pc,
        br1_mode => rs_br1_mode,
        br1_oper1 => rs_br1_oper1, br1_oper2 => rs_br1_oper2,
        br1_imm6 => rs_br1_imm6,
        br1_imm9 => rs_br1_imm9,
        br1_pc => rs_br1_pc,
        alu1_mode => rs_alu1_mode,
        ls1_mode => rs_ls1_mode,
        branch_mode => rs_branch_mode
);

exec_block : exec_unit port map(
    clk => clk, stall => stall, reset => reset, 
    alu1_dest => rs_alu1_dest, ls1_dest => rs_ls1_dest,
    alu1_oper1 => rs_alu1_oper1, alu1_oper2 => rs_alu1_oper2, ls1_oper => rs_ls1_oper,
    alu1_imm6 => rs_alu1_imm6,
    ls1_imm6 => rs_ls1_imm6,
    ls1_imm9 => rs_ls1_imm9,
    br1_mode => rs_br1_mode,
    br1_oper1 => rs_br1_oper1, br1_oper2 => rs_br1_oper2, br1_pc_in => rs_br1_pc_in, ls1_pc_in => rs_ls1_pc_in, alu1_pc_in => rs_alu1_pc_in,
    br1_imm6 => rs_br1_imm6,
    br1_imm9 => rs_br1_imm9,
    alu1_mode => rs_alu1_mode,
    ls1_mode => rs_ls1_mode,
    branch_mode => rs_branch_mode,
    stall_pc1 => pco1, stall_pc2 => pco2,
    alu_dest => ex_alu_dest, alu_value => ex_alu_value,
    ls_dest => ex_ls_dest ,ls_value => ex_ls_value,
    br_value => br_value, br_pc => ex_br_pc, alu_pc => ex_alu_pc, ls_pc => ex_ls_pc,
    alu_mode => ex_alu_mode,
    br_mode => ex_br_mode,
    ls_mode => ex_ls_mode,
    stall_out => unstall,
    alu_c => ex_alu_c, alu_z => ex_alu_z,
    br_c => ex_br_c, br_z => ex_br_z,
    ls_c => ex_ls_c, ls_z => ex_ls_z,
    br_eq1 =>b_obs1, br_eq2=>b_obs2,
    alu1_reg_data => wb_alu1_reg_data, alu1_reg_addr => wb_alu1_reg_addr, alu1_reg_en => wb_alu1_reg_en
    );
        
ROB_block:  rob port map(
            clk =>, stall =>, reset  =>,
            enable1 =>, enable2 =>,
            PC1 =>, PC4 =>,
            r1 =>, r4 =>
            rr1 =>, rr4 =>
            write_en =>,
            write_reg =>,
            write_data =>,
            exec_inp_en =>,
            exec_inp_pc =>,
            exec_inp_vals =>);

end architecture;