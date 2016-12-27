library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;

entity adder32 is
	port(
	A:in STD_LOGIC_VECTOR(31 DOWNTO 0);
	B:in STD_LOGIC_VECTOR(31 DOWNTO 0);
	S:out STD_LOGIC_VECTOR(31 DOWNTO 0);
	EN: in std_logic
);
end adder32;

architecture bhv of adder32 is
begin
	
	process
	begin
		if EN='1' then
			s<=A+B;
		else
			s<="00000000000000000000000000000000";
		end if;
	end process;
	
	
	--s<=A+B;
End bhv;