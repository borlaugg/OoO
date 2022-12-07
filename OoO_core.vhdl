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
        port(clk, stall,rst: in std_logic;
            mem_data_in_1, mem_data_in_2, pc_in: in std_logic_vector(15 downto 0);
            instr_1, instr_2, mem_addr_1, mem_addr_2, pc_out, pc_out_1, pc_out_2: out std_logic_vector(15 downto 0)
            );
    end component;

    component ID_STAGE is
        port(clk, stall: in std_logic;
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
    
        write_en: in std_logic_vector(0 to rob_size*2-1);
        write_reg: in addr_array(0 to rob_size*2-1);
        write_data: in prf_data_array(0 to rob_size*2-1);
    
        rr1,rr4,
        rr2,rr3,rr5,rr6:  out std_logic_vector(15 downto 0);
        v2,v3,v5,v6: out std_logic
        -- for args: r2, r3, r5, r6 we return the addr of rename reg
        -- for dests: r1, r4 we return the new rename reg assigned to them
        );
    end component;

    component rs_stage is
        generic(
            rs_size: integer := 16
            -- there are 8 architectural registers
        ); 
        port(
            clk, stall, reset: in std_logic;  --system ip
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
            alu_dest, alu_value : out std_logic_vector(15 downto 0);
            ls_dest,ls_value : out std_logic_vector(15 downto 0);
            br_value, br_pc, alu_pc, ls_pc : out std_logic_vector(15 downto 0);
            alu_mode: out std_logic_vector(5 downto 0);
            br_mode: out std_logic_vector(3 downto 0);
            ls_mode: out std_logic_vector(3 downto 0);
            alu_c, alu_z: out std_logic;
            br_c, br_z: out std_logic;
            ls_c, ls_z: out std_logic;
            br_eq : out std_logic);
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

    signal pc : std_logic_vector(15 downto 0) := (others=>'0');
    signal stall : std_logic := '0';

begin

ins_mem : InstructionMemory port map(clk => clk, addr_1 => ,addr_2 =>, data_out_1 => ,data_out_2 => );

if_block: IF_STAGE port map(clk => clk, stall => stall, rst => rst,
            mem_data_in_1 => , mem_data_in_2 =>, pc_in =>,
            instr_1 =>, instr_2 =>, mem_addr_1 =>, mem_addr_2 =>, pc_out =>, pc_out_1 =>, pc_out_2 =>);

id_block: ID_STAGE port map(clk => clk, stall=> stall,
        instr_in_1 =>, instr_in_2 =>,  pc_in_1 =>, pc_in_2 =>,
        r_1 =>, r_2 =>, r_3 =>, r_4 =>, r_5 =>, r_6 =>,
        opcode_1 =>, opcode_2 =>,
        alu_op_1 =>, alu_op_2 =>,
        imm6_1 =>, imm6_2 =>,
        imm9_1 =>, imm9_2 => ,
        pc_out_2 =>, pc_out_1 =>);
        
rs_block: rs_stage port map (
        clk=>, stall=>, reset=>,
        pc_in_1=>, pc_in_2=>,
        opcode_1=>, opcode_2=>,
        alu_op_1=>, alu_op_2=>,
        imm6_1=>, imm6_2=>,
        imm9_1=>, imm9_2=>, 
        r_1=>, r_2=>, r_3=>, r_4=>, r_5=>, r_6=>,
        v_2=>, v_3=>, v_5=>, v_6=>,
        prf_data_bus=>,
        prf_addr_bus=>,
        alu1_dest =>, ls1_dest =>,
        alu1_oper1 =>, alu1_oper2 =>, ls1_oper =>,
        alu1_imm6 =>,
        alu1_pc =>,
        ls1_imm6 =>,
        ls1_imm9 =>,
        ls1_pc =>,
        br1_mode =>,
        br1_oper1 =>, br1_oper2 =>,
        br1_imm6 =>,
        br1_imm9 =>,
        br1_pc =>,
        alu1_mode =>,
        ls1_mode => ,
        branch_mode =>
);

prf_block : rename_registers port map(gr1 => ,gr4 => ,
        gr2 => ,gr3 => ,gr5 => ,gr6 => ,
        reset => , clk => , stall => ,

        -- gr1-6 are the registers from the two lines of code above(in that order)
        prf_addr_bus => ,
        prf_data_bus => , -- (busy) (16 BIT DATA)

        write_en => ,
        write_reg => ,
        write_data => ,

        rr1 => ,rr4 => ,
        rr2 => ,rr3 => ,rr5 => ,rr6 =>,
        v2 => ,v3 => ,v5 => ,v6 =>
        -- for args: r2, r3, r5, r6 we return the addr of rename reg
        -- for dests: r1, r4 we return the new rename reg assigned to them
);

exec_block : exec_unit port map(
    clk =>, stall => , reset => , 
    alu1_dest =>, ls1_dest =>,
    alu1_oper1 =>, alu1_oper2 =>, ls1_oper =>,
    alu1_imm6 =>,
    ls1_imm6 =>,
    ls1_imm9 =>,
    br1_mode =>,
    br1_oper1 =>, br1_oper2 =>, br1_pc_in =>, ls1_pc_in =>, alu1_pc_in =>,
    br1_imm6 =>,
    br1_imm9 =>,
    alu1_mode => ,
    ls1_mode => ,
    branch_mode =>,
    alu_dest =>, alu_value =>,
    ls_dest =>,ls_value =>,
    br_value =>, br_pc =>, alu_pc =>, ls_pc =>,
    alu_mode =>,
    br_mode =>,
    ls_mode =>,
    alu_c =>, alu_z =>,
    br_c =>, br_z =>,
    ls_c =>, ls_z =>,
    br_eq =>);
        
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