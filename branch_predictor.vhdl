library ieee;
use ieee.std_logic_1164.all;

entity bpt is    
    port (rst, clk, b_obs, unstall: in std_logic;
          opcode: in  std_logic_vector(3 downto 0);
          pc_in: in std_logic_vector(15 downto 0);
          b_pred: out std_logic;
          pc_out: out std_logic_vector(15 downto 0)
        );
end entity;

architecture beh of bpt is
type state is (reset, t_1, t_0, nt_1, nt_0);
signal curr_state, nxt_state : state := reset;
signal pred: std_logic;
begin
    process (clk, rst) 
    begin
        if (rst = '0') then
            curr_state <= reset;
        elsif (clk' event and clk = '1') then   
            curr_state <= nxt_state;
        end if;
    end process;
    
    process(opcode, b_obs, curr_state, unstall)
    begin
        if (opcode = "1001" or opcode = "1010" or opcode = "1011") then
            pred <= '1';
        elsif (opcode = "1000") then
            case curr_state is
                when reset=> pred <= '0';
                          if (b_obs = '1') then
                            nxt_state <= nt_1;
                          else 
                            nxt_state <= nt_0;
                          end if;
                when nt_0=> pred <= '0';
                          if (b_obs = '1') then
                            nxt_state <= nt_1;
                          else 
                            nxt_state <= nt_0;
                          end if;
                when nt_1=> pred <= '0';
                          if (b_obs = '1') then
                            nxt_state <= t_0;
                          else 
                            nxt_state <= nt_0;
                          end if;
                when t_0=> pred <= '1';
                          if (b_obs = '1') then
                            nxt_state <= t_1;
                          else 
                            nxt_state <= nt_1;
                          end if;
                when t_1=> pred <= '1';
                          if (b_obs = '1') then
                            nxt_state <= t_1;
                          else 
                            nxt_state <= t_0;
                          end if;
            end case;
        else 
            pred <= '0';
        end if;

        if (pred = '1' and unstall = '1') then
          pred <= '0';
        end if;
    end process;

    b_pred <= pred; --b_pred = stall
    pc_out <= pc_in;

end architecture;