
--r_a, r_b, r_c: in std_logic_vector(3 downto 0);
--	 opcode: in std_logic_vector(4 downto 0);

library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

entity REG_VALIDATOR is
port(
	r_a, r_b, r_c: in std_logic_vector(2 downto 0); -- 1111 don't care
	opcode: in std_logic_vector(3 downto 0);
	or_a, or_b, or_c: out std_logic_vector(3 downto 0)
	);
end REG_VALIDATOR;

architecture behav of REG_VALIDATOR is
begin
	process(r_a, r_b, r_c, opcode)
	begin
		if (opcode = "0000" or opcode = "0111" or opcode = "0101" or opcode = "1000" or opcode = "1010" or opcode = "0011" or opcode = "1100" or opcode = "1101" or opcode = "1001" or opcode = "1011") then
			or_c(3) <= '1';
		end if;
		if (opcode = "0011" or opcode = "1100" or opcode = "1101" or opcode = "1001" or opcode = "1011") then
			or_b(3) <= '1';
		end if;
		or_a(3) <= '0';
		or_a(2 downto 0) <= r_a;
		or_b(2 downto 0) <= r_b;
		or_c(2 downto 0) <= r_c;
	end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

entity ID_STAGE is
port(clk, stall: in std_logic;
	instr_in_1, instr_in_2, pc_in_1, pc_in_2: in std_logic_vector(15 downto 0);
	r_1, r_2, r_3, r_4, r_5, r_6: out std_logic_vector(3 downto 0); -- 1111 don't care
	opcode_1, opcode_2: out std_logic_vector(3 downto 0);
	alu_op_1, alu_op_2: out std_logic_vector(1 downto 0);
	imm6_1, imm6_2 : out std_logic_vector(5 downto 0);
    imm9_1, imm9_2 : out std_logic_vector(8 downto 0);
	pc_out_2, pc_out_1  : out std_logic_vector(15 downto 0)
	);
end ID_STAGE;


architecture behav of ID_STAGE is
component REG_VALIDATOR is
port(
	r_a, r_b, r_c: in std_logic_vector(2 downto 0); -- 1111 don't care
	opcode: in std_logic_vector(3 downto 0);
	or_a, or_b, or_c: out std_logic_vector(3 downto 0)
	);
end component;
signal tr_1, tr_2, tr_3, tr_4, tr_5, tr_6: std_logic_vector(2 downto 0);
signal topcode_1, topcode_2: std_logic_vector(3 downto 0);

begin
	opcode_1 <= topcode_1;
	opcode_2 <= topcode_2;
	alu_op_1 <= instr_in_1(1 downto 0);
	alu_op_2 <= instr_in_2(1 downto 0);
	pc_out_1 <= pc_in_1;
	pc_out_2 <= pc_in_2;
	rv_1: REG_VALIDATOR port map(r_a => tr_1, r_b => tr_2, r_c => tr_3,
		opcode => topcode_1, or_a => r_1, or_b => r_2, or_c => r_3);
	rv_2: REG_VALIDATOR port map(r_a => tr_4, r_b => tr_5, r_c => tr_6,
		opcode => topcode_2, or_a => r_4, or_b => r_5, or_c => r_6);
	process(clk)
	variable pc : std_logic_vector(15 downto 0) := (others => '0');
	variable op_1, op_2 : std_logic_vector(3 downto 0);
	begin
		if falling_edge(clk) and stall = '0' then
			if (instr_in_1(15 downto 12) = "0000" or instr_in_1(15 downto 12) = "0111" or instr_in_1(15 downto 12) = "0101" or instr_in_1(15 downto 12) = "1000") then
				imm6_1 <= instr_in_1(5 downto 0);
				imm9_1 <= "111111111";
			elsif (instr_in_1(15 downto 12) = "1011" or instr_in_1(15 downto 12) = "1001" or instr_in_1(15 downto 12) = "1101" or instr_in_1(15 downto 12) = "1100" or instr_in_1(15 downto 12) = "1111") then
				imm6_1 <= "111111";
				imm9_1 <= instr_in_1(8 downto 0);
			else 
				imm6_1 <= "111111";
				imm9_1 <= "111111111";
			end if;
			if (instr_in_2(15 downto 12) = "0000" or instr_in_2(15 downto 12) = "0111" or instr_in_2(15 downto 12) = "0101" or instr_in_2(15 downto 12) = "1000") then
				imm6_2 <= instr_in_2(5 downto 0);
				imm9_2 <= "111111111";
			elsif (instr_in_2(15 downto 12) = "1011" or instr_in_2(15 downto 12) = "1001" or instr_in_2(15 downto 12) = "1101" or instr_in_2(15 downto 12) = "1100" or instr_in_2(15 downto 12) = "1111") then
				imm6_2 <= "111111";
				imm9_2 <= instr_in_2(8 downto 0);
			else 
				imm6_2 <= "111111";
				imm9_2 <= "111111111";
			end if;
			tr_1(2 downto 0) <= instr_in_1(6 downto 4);
			tr_2(2 downto 0) <= instr_in_1(9 downto 7);
			tr_3(2 downto 0) <= instr_in_1(12 downto 10);

			tr_4(2 downto 0) <= instr_in_2(6 downto 4);
			tr_5(2 downto 0) <= instr_in_2(9 downto 7);
			tr_6(2 downto 0) <= instr_in_2(12 downto 10);

			topcode_1 <= instr_in_1(3 downto 0);
			topcode_2 <= instr_in_2(3 downto 0);
		end if;
	end process;
end architecture;