library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.array_pkg.all;

entity OoO_core is
    generic(
        rs_size: integer := 256;
        ls_rs_size: integer := 256;
        rob_size: integer := 256
        -- there are 8 architectural registers
    ); 
    port (clk, rst : in std_logic
    );
end entity;

architecture superscalar of OoO_core is

    component IF_STAGE is
        port(clk, stall,unstall,rst: in std_logic;
            mem_data_in_1,  dest_pc, mem_data_in_2, pc_in: in std_logic_vector(15 downto 0);
            instr_1, instr_2, mem_addr_1, mem_addr_2, pc_out, pc_out_1, pc_out_2: out std_logic_vector(15 downto 0)
            );
    end component;

    component Increment port (pc_in: in std_logic_vector(15 downto 0);
                              pc_out: out std_logic_vector(15 downto 0));
    end component;

    component bpt is    
    port (rst, clk, b_obs, unstall: in std_logic; -- lol :)
          opcode: in  std_logic_vector(3 downto 0);
          pc_in: in std_logic_vector(15 downto 0);
          dest_reg: in std_logic_vector(2 downto 0);
          b_pred: out std_logic;
          pc_out: out std_logic_vector(15 downto 0)
        );
    end component;

    component ID_STAGE is
        port(clk, stall, rst: in std_logic;
        instr_in_1, instr_in_2, pc_in_1, pc_in_2: in std_logic_vector(15 downto 0);
        r_1, r_2, r_3, r_4, r_5, r_6: out std_logic_vector(3 downto 0); -- 1111 don't care
        opcode_1, opcode_2: out std_logic_vector(3 downto 0);
        alu_op_1, alu_op_2: out std_logic_vector(1 downto 0);
        imm6_1, imm6_2 : out std_logic_vector(5 downto 0);
        imm9_1, imm9_2 : out std_logic_vector(8 downto 0);
        pc_out_2, pc_out_1  : out std_logic_vector(15 downto 0)
	);
    end component;

    component dispatch is
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
    end component dispatch;

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
                v2,v3,v5,v6: out std_logic;

                ls_rs_wb_addr: in std_logic_vector(15 downto 0); -- addr for writing
                ls_rs_wb_en: in std_logic; -- to memory to enable write
                ls_rs_wb_data: in std_logic_vector(15 downto 0);
                -- for args: r2, r3, r5, r6 we return the addr of rename reg
                -- for dests: r1, r4 we return the new rename reg assigned to them

                -- to rob
                rob_addr_bus: in addr_array(0 to rob_size-1);
                rob_busy_bus: out std_logic_vector(0 to rob_size-1)
    );
    end component;

    component ls_rs_stage is
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
    
            mem_read_data_bus: in std_logic_vector(15 downto 0); -- from data memory
            mem_addr_bus: out std_logic_vector(15 downto 0); -- addr for writing
            mem_write_en: out std_logic; -- to memory to enable write
            mem_write_data_bus: out std_logic_vector(15 downto 0);
    
            prf_wb_addr: out std_logic_vector(15 downto 0); -- addr for writing
            prf_wb_en: out std_logic; -- to memory to enable write
            prf_wb_data: out std_logic_vector(15 downto 0)
    
        );
    end component ls_rs_stage;


    component rs_stage is
        generic(
            rs_size: integer := 16;
            rob_size: integer := 20
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
            clk, stall, reset,exec: in std_logic;
     
            -- ports for adding to ROB (from the dispatch stage)
            enable1, enable2: in std_logic; -- enables for channels (each channel corresponds to one entry to be added)
            PC1, PC4, PC_BP1, PC_BP2: in std_logic_vector(15 downto 0); -- PC of the instruction
            r1, r4: in std_logic_vector(15 downto 0); -- dest regs
            rr1, rr4: in std_logic_vector(15 downto 0); -- rename regs assigned to the instr
    
            -- ports for writing back to the prf
            write_en: out std_logic_vector(0 to rob_size*2-1);
            write_rreg: out bit16_vector(0 to rob_size*2-1);
            write_destreg: out bit16_vector(0 to rob_size*2-1);
    
            -- inputs from the execution unit
            addr_bus: out addr_array(0 to rob_size-1);
            busy_bus: in std_logic_vector(0 to rob_size-1)
    
        );
    end component;

    component InstructionMemory is
        port(clk: in std_logic;
              addr_1, addr_2: in std_logic_vector(15 downto 0);
              data_out_1, data_out_2: out std_logic_vector(15 downto 0)
        );
    end component;

    component  DataMemory is
    port(clk, write_en: in std_logic;
          addr: in std_logic_vector(15 downto 0);
          data_in: in std_logic_vector(15 downto 0);
          data_out: out std_logic_vector(15 downto 0)
    );
    end component; 
    signal instr_1, instr_2: std_logic_vector(15 downto 0);


    signal pc_out_1, pc_out_2: std_logic_vector(15 downto 0);
    signal data_mem_write_en: std_logic;
    signal mem_write_en: std_logic;
    signal mem_data_write_bus: std_logic_vector(15 downto 0);
    signal mem_data_read_bus: std_logic_vector(15 downto 0);
    signal mem_data_addr_bus: std_logic_vector(15 downto 0);
    signal id_pc_out_2, id_pc_out_1,id_pc1, id_pc2, br_value,pc_i, pc_o, pc1, pc2, pco1, pco2, do1, do2, a1, a2 : std_logic_vector(15 downto 0) := (others=>'0');
    signal stall1, stall2, stall, unstall, b_obs1, b_obs2, rs_en1, rs_en2 : std_logic := '0';
    signal id_r_1, id_r_2, id_r_3, id_r_4, id_r_5, id_r_6 : std_logic_vector(3 downto 0) := (others=>'0');
    signal id_opcode_1, id_opcode_2 : std_logic_vector(3 downto 0) := (others=>'0');
    signal id_imm6_1, id_imm6_2 : std_logic_vector(5 downto 0) := (others=>'0');
    signal id_imm9_1, id_imm9_2 : std_logic_vector(8 downto 0) := (others=>'0');
    signal id_alu_op_1, id_alu_op_2 : std_logic_vector(1 downto 0) := (others=>'0');
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
    signal busy_bus: std_logic_vector(0 to rob_size-1);
    signal addr_bus: addr_array(0 to rob_size-1);
    signal rob_write_en: std_logic_vector(0 to rob_size-1);
    signal rob_write_rreg: bit16_vector(0 to rob_size-1);
    signal rob_write_destreg: bit16_vector(0 to rob_size-1);
    signal prf_addr_bus_1, prf_addr_bus_2: addr_array(0 to rs_size*2-1);
    signal prf_data_bus_1, prf_data_bus_2: prf_data_array(0 to rs_size*2-1);

    signal disp_rob_enable1, disp_rob_enable2 : std_logic;
    signal disp_rob_PC1, disp_rob_PC4: std_logic_vector(15 downto 0);
    signal disp_rob_r_1, disp_rob_r_4: std_logic_vector(3 downto 0);
    signal disp_rob_rr1, disp_rob_rr4: std_logic_vector(15 downto 0);

    signal ls_rs_wb_addr:  std_logic_vector(15 downto 0); -- addr for writing
    signal ls_rs_wb_en:  std_logic; -- to memory to enable write
    signal ls_rs_wb_data:  std_logic_vector(15 downto 0);

    signal ls_rs_pc_in_1, ls_rs_pc_in_2:   std_logic_vector(15 downto 0); --id stage
    signal ls_rs_opcode_1, ls_rs_opcode_2:  std_logic_vector(3 downto 0); --id stage
    signal ls_rs_imm6_1, ls_rs_imm6_2:  std_logic_vector(5 downto 0);  -- id stage
    signal ls_rs_imm9_1, ls_rs_imm9_2 :  std_logic_vector(8 downto 0);  -- id stage
    signal ls_rs_r_a_1, ls_rs_r_b_1, ls_rs_r_a_2, ls_rs_r_b_2:  std_logic_vector(15 downto 0);  --rr stage
    signal ls_rs_v_a_1 ,ls_rs_v_a_2, ls_rs_v_b_1, ls_rs_v_b_2:  std_logic; 
    
    signal rs_pc_in_1, rs_pc_in_2:   std_logic_vector(15 downto 0); --id stage
    signal rs_opcode_1, rs_opcode_2:  std_logic_vector(3 downto 0);  --id stage
    signal rs_alu_op_1, rs_alu_op_2:  std_logic_vector(1 downto 0);  -- id stage
    signal rs_imm6_1, rs_imm6_2:  std_logic_vector(5 downto 0);  -- id stage
    signal rs_imm9_1, rs_imm9_2 :  std_logic_vector(8 downto 0);  -- id stage
    signal rs_r_1, rs_r_2, rs_r_3, rs_r_4, rs_r_5, rs_r_6:  std_logic_vector(15 downto 0);  --rr stage
    signal rs_v_2, rs_v_3, rs_v_5, rs_v_6 :  std_logic;  --from rr

    signal prf_r_1, prf_r_2, prf_r_3, prf_r_4, prf_r_5, prf_r_6:  std_logic_vector(15 downto 0);  --rr stage
    signal prf_v_2, prf_v_3, prf_v_5, prf_v_6 :  std_logic;  --from rr

begin

ins_mem : InstructionMemory port map(clk => clk, addr_1 => a1,addr_2 => a2, data_out_1 => do1,data_out_2 => do2);
data_mem : DataMemory port map(clk => clk, write_en => mem_write_en,
          addr => mem_data_addr_bus,
          data_in => mem_data_write_bus,
          data_out => mem_data_read_bus);

if_block: IF_STAGE port map(clk => clk, stall => stall,  unstall => unstall, rst => rst,
            mem_data_in_1 => do1, dest_pc =>br_value, mem_data_in_2 => do2, pc_in => pc_i, 
            instr_1 => instr_1, instr_2 => instr_2, mem_addr_1 =>a1 ,  mem_addr_2 => a2, pc_out => pc_o, pc_out_1 => pc1, pc_out_2 => pc2);

incr_block: Increment port map(pc_in => pc_o, pc_out => pc_i);

bp_block1: bpt port map (rst => rst, clk => clk, b_obs => b_obs1, opcode => pc_out_1(15 downto 12), b_pred => stall1, unstall => unstall,
                        dest_reg => instr_1(11 downto 9), pc_in => pc1, pc_out =>pco1);

bp_block2: bpt port map (rst => rst, clk => clk, b_obs =>b_obs2, opcode => pc_out_2(15 downto 12), b_pred => stall2,  unstall => unstall,
                        dest_reg => instr_1(11 downto 9), pc_in => pc2, pc_out =>pco2);

stall <= stall1 or stall2;

id_block: ID_STAGE port map(clk => clk, stall => stall, rst => rst,
        instr_in_1 => do1, instr_in_2 =>do2,  pc_in_1 => pc_out_1, pc_in_2 =>pc_out_2,
        r_1 => id_r_1, r_2 => id_r_2, r_3 => id_r_3, r_4 => id_r_4, r_5 => id_r_5, r_6 => id_r_6,
        opcode_1 => id_opcode_1, opcode_2 => id_opcode_2,
        alu_op_1 => id_alu_op_1, alu_op_2 => id_alu_op_2,
        imm6_1 => id_imm6_1, imm6_2 => id_imm6_2,
        imm9_1 => id_imm9_1, imm9_2 => id_imm9_2,
        pc_out_2 => id_pc2, pc_out_1 => id_pc1);
    
dispatch_block: dispatch port map(
    clk =>clk, reset=> rst,
-- input coming from ID
    id_r_1 => id_r_1, id_r_2=> id_r_2, id_r_3=> id_r_3, id_r_4=> id_r_4, id_r_5=> id_r_5, id_r_6=> id_r_6,
    id_opcode_1 =>id_opcode_1, id_opcode_2=>id_opcode_2,
    id_alu_op_1 =>id_alu_op_1, id_alu_op_2=>id_alu_op_2,
    id_imm6_1 =>id_imm6_1, id_imm6_2 =>id_imm6_2,
    id_imm9_1 =>id_imm9_1, id_imm9_2=>id_imm9_2,
    id_pc_out_2 =>id_pc_out_2, id_pc_out_1 => id_pc_out_1,
    prf_r_1 => prf_r_1, prf_r_2 => prf_r_2, prf_r_3 => prf_r_3, prf_r_4 => prf_r_4, prf_r_5 => prf_r_5, prf_r_6 => prf_r_6,
    prf_v_2 => prf_v_2, prf_v_3 => prf_v_3, prf_v_5 => prf_v_5, prf_v_6 => prf_v_6,

    -- dispatch to regular rs (everything except load-store instructions)
    rs_pc_in_1 =>rs_pc_in_1, rs_pc_in_2=> rs_pc_in_2,
    rs_opcode_1 => rs_opcode_1, rs_opcode_2=> rs_opcode_2,
    rs_alu_op_1 => rs_alu_op_1, rs_alu_op_2 => rs_alu_op_2,
    rs_imm6_1 => rs_imm6_1, rs_imm6_2 =>rs_imm6_2,
    rs_imm9_1 => rs_imm9_1, rs_imm9_2 => rs_imm9_2,
    rs_r_1 => rs_r_1, rs_r_2  => rs_r_2, rs_r_3  => rs_r_3, rs_r_4 => rs_r_4, rs_r_5 => rs_r_5, rs_r_6 => rs_r_6,
    rs_v_2 => rs_v_2, rs_v_3 => rs_v_3, rs_v_5 => rs_v_5, rs_v_6 => rs_v_6,

    -- dispatch to load store rs (only load-store instructions)
    ls_rs_pc_in_1 => ls_rs_pc_in_1, ls_rs_pc_in_2 => ls_rs_pc_in_2,
    ls_rs_opcode_1 => ls_rs_opcode_1, ls_rs_opcode_2 => ls_rs_opcode_2,
    ls_rs_imm6_1 => ls_rs_imm6_1, ls_rs_imm6_2 => ls_rs_imm6_2,
    ls_rs_imm9_1 => ls_rs_imm9_1, ls_rs_imm9_2 =>ls_rs_imm9_2,
    ls_rs_r_a_1 => ls_rs_r_a_1, ls_rs_r_b_1=> ls_rs_r_b_1, ls_rs_r_a_2=> ls_rs_r_a_2, ls_rs_r_b_2=> ls_rs_r_b_2,
    ls_rs_v_a_1 =>ls_rs_v_a_1, ls_rs_v_a_2 =>ls_rs_v_a_2,ls_rs_v_b_1 =>ls_rs_v_b_1, ls_rs_v_b_2=> ls_rs_v_b_2,

    -- dispatch to rob (all instructions)
    rob_enable1 =>disp_rob_enable1, rob_enable2 =>disp_rob_enable4,
    rob_r1 =>disp_rob_r_1, rob_r4=>disp_rob_r_4,
    rob_rr1 =>disp_rob_rr1, rob_rr4 =>disp_rob_rr4,
    rob_PC1 =>disp_rob_PC1, rob_PC4=>disp_rob_PC1);
        
prf_block : rename_registers port map(gr1 => id_r_1(2 downto 0),gr4 => id_r_4(2 downto 0),
        gr2 => id_r_2(2 downto 0),gr3 => id_r3,gr5 => id_r_5(2 downto 0),gr6 => id_r_6(2 downto 0),
        reset => rst, clk => clk, stall => stall,

        -- gr1-6 are the registers from the two lines of code above(in that order)
        prf_addr_bus(31 downto 16) => prf_addr_bus_1, --1 connects to lsrs
        prf_data_bus(31 downto 16) => prf_data_bus_1, -- (busy) (16 BIT DATA)
        prf_addr_bus(15 downto 0) => prf_addr_bus_2, --2 connects to regular rs
        prf_data_bus(15 downto 0) => prf_data_bus_2,

        rob_write_en => rob_write_en,
        rob_write_rreg => rob_write_rreg,
        rob_write_destreg => rob_write_destreg,

         --prf inputs from alu wb:
        alu1_reg_data => wb_alu1_reg_data,
        alu1_reg_addr => wb_alu1_reg_addr,
        alu1_reg_en => wb_alu1_reg_en,

        rob_addr_bus => addr_bus,
        rob_busy_bus => busy_bus,

        ls_rs_wb_addr=>ls_rs_wb_addr,
        ls_rs_wb_en=>ls_rs_wb_en,
        ls_rs_wb_data=>ls_rs_wb_data,

        rr1 => prf_r_1, rr4 => prf_r_4,
        rr2 => prf_r_2, rr3 => prf_r_3, rr5 => prf_r_5, rr6 => prf_r_6,
        v2 => prf_v_2,v3 =>  prf_v_3, v5 => prf_v_5, v6 => prf_v_6
        -- for args: r2, r3, r5, r6 we return the addr of rename reg
        -- for dests: r1, r4 we return the new rename reg assigned to them
);

rs_ls_block: ls_rs_stage port map(
    clk => clk, stall =>stall, reset =>rst,
    pc_in_1 => ls_rs_pc_in_1, pc_in_2 => ls_rs_pc_in_2,
    opcode_1 => ls_rs_opcode_1, opcode_2 => ls_rs_opcode_2,
    imm6_1 => ls_rs_imm6_1, imm6_2 => ls_rs_imm6_2,
    imm9_1 =>ls_rs_imm9_1, imm9_2 =>ls_rs_imm9_2,
    r_a_1 =>ls_rs_r_a_1, r_b_1 =>ls_rs_r_b_1, r_a_2 =>ls_rs_r_a_2, r_b_2 =>ls_rs_r_b_2,
    v_a_1 =>ls_rs_v_a_1, v_a_2 => ls_rs_v_a_2, v_b_1 => ls_rs_v_b_1, v_b_2 => ls_rs_v_b_2,
    prf_data_bus =>prf_addr_bus_1,
    prf_addr_bus =>prf_data_bus_1,

    mem_read_data_bus => mem_data_read_bus,
    mem_addr_bus => mem_addr_bus,
    mem_write_en => mem_write_en,
    mem_write_data_bus => mem_data_write_bus,

    prf_wb_addr =>ls_rs_wb_addr,
    prf_wb_en =>ls_rs_wb_en,
    prf_wb_data =>ls_rs_wb_data
    );
rs_block: rs_stage port map (
        clk=> clk, reset=> rst,
        pc_in_1 => rs_pc_in_1, pc_in_2 => rs_pc_in_2,
        opcode_1 => rs_opcode_1, opcode_2 => rs_opcode_2,
        alu_op_1=> rs_alu_op_1, alu_op_2 => rs_alu_op_2,
        imm6_1 => rs_imm6_1, imm6_2 => rs_imm6_2,
        imm9_1 => rs_imm9_1, imm9_2 => rs_imm9_2, 
        r_1 => rs_r_1, r_2=> rs_r_2, r_3 => rs_r_3, r_4 => rs_r_4, r_5 => rs_r_5, r_6 => rs_r_6,
        v_2 => rs_v_2, v_3 => rs_v_3, v_5 => rs_v_5, v_6 => rs_v_6,
        prf_data_bus=> prf_data_bus,
        prf_addr_bus=> prf_addr_bus,
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
    clk => clk, stall => stall, reset => rst, 
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
            clk => clk, stall => stall, reset  => rst, exec => unstall,
            enable1 => disp_rob_r1 , enable2 => disp_rob_r4 ,
            PC1 => disp_rob_PC1, PC4 => disp_rob_PC4,
            PC_BP1 => pco1, PC_BP2 => pco2,
            r1 => disp_rob_rr1 , r4 => disp_rob_rr4 ,
            rr1 => disp_rob_enable1, rr4 => disp_rob_enable2 ,
            write_en => rob_write_en,
            write_rreg => rob_write_rreg,
            write_destreg => rob_write_destreg,
            addr_bus => addr_bus,
            busy_bus => busy_bus);

end architecture;