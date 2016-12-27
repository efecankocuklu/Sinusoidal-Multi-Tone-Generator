library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;

entity REG32 is
	port(
		LOAD	:in STD_LOGIC;--This is clock input the same with top module CLK and connects to C9.
		DIN	:in STD_LOGIC_VECTOR(31 DOWNTO 0);
		DOUT	:out STD_LOGIC_VECTOR(31 DOWNTO 0)
	);
end REG32;

architecture bhv of REG32 is

begin 
	process(LOAD,DIN)
	begin
		if LOAD'event and LOAD='1' then
		DOUT<=DIN;
		end if;
	end process;
End bhv;