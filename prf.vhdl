library ieee ;
use ieee.std_logic_1164.all ;

entity rename_registers is
    port(gr1,gr4,
    gr2,gr3,gr5,gr6: in integer,
    reset: in std_logic,

    rr1,rr4,
    rr2,rr3,rr5,rr6: out integer
    );
end entity;

architecture renaming of rename_registers is

    constant n : integer := 65536 --size of PRF
    type rat_vector is array(0 to 5) of integer;
    signal rat: rat_vector := (0,1,2,3,4,5);
    variable tags : std_logic_vector() --

    signal next_value : integer := 6

begin

    rr2 <= rat(gr2);
    rr3 <= rat(gr3);
    rr1 <= big;
    rat(gr1) <= big;
    big <= big+1;

    rr5 <= rat(gr5);
    rr6 <= rat(gr6);
    rr4 <= big;
    rat(gr4) <= big;
    big <= big+1;

end renaming;