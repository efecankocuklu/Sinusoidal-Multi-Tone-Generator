----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Efecan Kocuklu, Arda Kalender, Gizem Koca
-- 
-- Create Date:    10:41:51 12/10/2016 
-- Design Name: Tone Generator
-- Module Name:    TopModule - Behavioral 
-- Project Name: 
-- Target Devices: Spartan 3E Starter Board
-- Tool versions: 
-- Description: 
--
-- Dependencies: dacstuff2.vhd, ToneGen.vhd, ps2_keyboard.vhd, DACconnections.ucf, BTNconnections.ucf
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.STD_LOGIC_arith.ALL;
use IEEE.NUMERIC_STD.ALL;
--use IEEE.std_logic_signed.all;
--library UNISIM;
--use UNISIM.VComponents.all;

entity TopModule is
		GENERIC(
		debounce_counter_size : INTEGER := 8;
		debounce_counter_size2 : INTEGER := 19); --set such that (2^size)/clk_freq = 5us (size = 8 for 50MHz)
		Port(
		DAC_CLR     : out std_logic; -- P8
		FPGA_INIT_B : out std_logic; -- T3
		AMP_CS      : out std_logic; -- N7
		AD_CONV     : out std_logic; -- P11
		SF_CE0      : out std_logic; -- D16
		SPI_SS_B    : out std_logic; -- U3
		CLK			: in 	std_logic; -- C9(50MHz Clock)
		CS 			: out STD_LOGIC; -- N8
		MOSI 			: out STD_LOGIC; -- T4
		KEYBOARD_EN	: in	STD_LOGIC; -- L13(SW0)
		BTN_DO		: in	STD_LOGIC; -- B4 (J1<0> IO1)
		BTN_RE		: in	STD_LOGIC; -- U4(BTN_NORTH)
		BTN_MI		: in	STD_LOGIC; -- D18(BTN_WEST)
		BTN_FA		: in	STD_LOGIC; -- K17(BTN_SOUTH)
		BTN_SOL		: in	STD_LOGIC; -- SW3
		BTN_LA		: in	STD_LOGIC; -- SW2
		BTN_SI		: in	STD_LOGIC; -- SW1
		SCK 			: out STD_LOGIC; -- U16
		ps2_clk		: IN  STD_LOGIC; --clock signal from PS/2 keyboard (G14)
		ps2_data		: IN  STD_LOGIC; --data signal from PS/2 keyboard	(G13)
		OCTAVE_INC	: IN	STD_LOGIC; --octave increment button
		OCTAVE_DEC	: IN	STD_LOGIC; --octave decrement button
		LED			: OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
		);
end TopModule;

architecture Behavioral of TopModule is

	signal EN 			: STD_LOGIC;  -- DAC ENABLE
	signal Din			: STD_LOGIC_VECTOR(11 downto 0); -- Analog Value for DAC
	signal BTNinputs	: STD_LOGIC_VECTOR(6 downto 0); --BTN inputs Array
	signal NOTES		: STD_LOGIC_VECTOR(6 downto 0);
	signal ps2_code_new: STD_LOGIC;
	signal flag,flag2	: STD_LOGIC;
	signal ps2_code	: STD_LOGIC_VECTOR(7 DOWNTO 0);--PS2 KEYBOARD CODE FOR EXAMPLE 'A' = 1C
	signal Temp13b,Temp13b2,Temp13b3		: STD_LOGIC_VECTOR(12 downto 0);
	signal Temp12b,Temp12b2,Temp12b3		: STD_LOGIC_VECTOR(11 downto 0);
	signal Temp14b		: STD_LOGIC_VECTOR(13 downto 0);
	signal Addr 		: STD_LOGIC_VECTOR(3 downto 0);  -- Shows that Which DAC pin is selected
	signal OCTAVE		: integer range 0 to 3:=0;
	signal OCTAVE_INC_DEB,OCTAVE_DEC_DEB: STD_LOGIC;
	signal DO,RE,MI,FA,SOL,LA,SI		: integer;
	signal BTN_DO_DEB,BTN_RE_DEB,BTN_MI_DEB,BTN_FA_DEB,BTN_SOL_DEB,BTN_LA_DEB,BTN_SI_DEB: STD_LOGIC;--DEBOUNCED INPUTS
	signal DOout,REout,MIout,FAout,SOLout,LAout,SIout: STD_LOGIC_VECTOR(11 downto 0);-- 12 bit tone outs.
	
	component dacstuff2 is
		port( clk 	: in  STD_LOGIC; -- Max 50 MHz
				CS 	: out STD_LOGIC; -- N8
				MOSI 	: out STD_LOGIC; -- T4
				SCK 	: out STD_LOGIC; -- U16
				Din 	: in  STD_LOGIC_VECTOR(11 downto 0); -- Deðer
				Addr 	: in  STD_LOGIC_VECTOR(3 downto 0);  -- Hangi DAC
				EN 	: in  STD_LOGIC  -- EN=1 enable falan filan
		);
	end component;
	
	component ToneGen is
		port( CLK			: in 	std_logic; -- C9(50MHz Clock)
				TONEout12b	: out	std_logic_vector (11 downto 0);--12 bit tone outputs.
				NOTEin		: in 	integer; --NOTEin calculated from this formula: k=fin*2^32/50000000
				EN				: in	std_logic
		);
	end component;
	
	component ps2_keyboard is
		port(
				clk			: IN  STD_LOGIC;                     --system clock
				ps2_clk		: IN  STD_LOGIC;                     --clock signal from PS/2 keyboard
				ps2_data		: IN  STD_LOGIC;                     --data signal from PS/2 keyboard
				ps2_code_new: OUT STD_LOGIC;                     --flag that new PS/2 code is available on ps2_code bus
				ps2_code		: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)	 --code received from PS/2
		);
	end component;
	
	COMPONENT debounce IS
		GENERIC(
				counter_size : INTEGER); --debounce period (in seconds) = 2^counter_size/(clk freq in Hz)
		PORT(
				clk    : IN  STD_LOGIC;  --input clock
				button : IN  STD_LOGIC;  --input signal to be debounced
				result : OUT STD_LOGIC); --debounced signal
	END COMPONENT;
	
begin
LED<=BTNinputs;
----------------------------------------
	--DAC_CLR     <= '1'; -- P8				|
	FPGA_INIT_B <= '1'; -- T3				|
	AMP_CS      <= '1'; -- N7				|
	AD_CONV     <= '0'; -- P11				|
	SF_CE0      <= '1'; -- D16				|
	SPI_SS_B    <= '1'; -- U3				|
----------------------------------------

--INPUT DEBOUNCERS----------
	DO_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_DO, result => BTN_DO_DEB);
	RE_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_RE, result => BTN_RE_DEB);
	MI_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_MI, result => BTN_MI_DEB);
	FA_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_FA, result => BTN_FA_DEB);
	SOL_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_SOL, result => BTN_SOL_DEB);
	LA_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_LA, result => BTN_LA_DEB);
	SI_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size)
		PORT MAP(clk => clk, button => BTN_SI, result => BTN_SI_DEB);
	OCTAVE_INC_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size2)
		PORT MAP(clk => clk, button => OCTAVE_INC, result => OCTAVE_INC_DEB);
	OCTAVE_DEC_DEBOUNCER: debounce
		GENERIC MAP(counter_size => debounce_counter_size2)
		PORT MAP(clk => clk, button => OCTAVE_DEC, result => OCTAVE_DEC_DEB);
--INPUT DEBOUNCERS----------

--DAC COMPONENT-------------
	DAC: dacstuff2 port map (
		clk 	=> CLK	,
		CS 	=>	CS		,
		MOSI 	=>	MOSI	,
		SCK 	=>	SCK	,
		Din 	=>	Din	,
		Addr 	=>	Addr	,
		EN 	=>	EN
	);
--DAC COMPONENT-------------

--KEYBOARD COMPONENT--------
	KEYBOARD: ps2_keyboard port map (
		clk				=>	CLK			,
		ps2_clk			=>	ps2_clk		,
		ps2_data			=>	ps2_data		,
		ps2_code_new	=>	ps2_code_new,
		ps2_code			=>	ps2_code
	);
--KEYBOARD COMPONENT--------

--NOTE GENERATORS--------------
	DOGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	DOout	,
		NOTEin		=> DO		,
		EN				=>	EN
	);
	REGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	REout	,
		NOTEin		=> RE		,
		EN				=>	EN
	);
	MIGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	MIout	,
		NOTEin		=> MI		,
		EN				=>	EN
	);
	FAGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	FAout	,
		NOTEin		=> FA		,
		EN				=>	EN
	);
	SOLGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	SOLout,
		NOTEin		=> SOL	,
		EN				=>	EN
	);
	LAGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	LAout	,
		NOTEin		=> LA		,
		EN				=>	EN
	);
	SIGEN: ToneGen port map (
		CLK			=>	CLK	,
		TONEout12b	=>	SIout	,
		NOTEin		=> SI		,
		EN				=>	EN
	);
--NOTE GENERATORS--------------

--MAIN CODE--------------------
--KEYBOARD ENABLER-------------
	process (NOTES, ps2_code)
	begin
		if KEYBOARD_EN='1' THEN
			case ps2_code is
				when "00011100" => NOTES <= "1000000";
				when "00011011" => NOTES <= "0100000";
				when "00100011" => NOTES <= "0010000";
				when "00101011" => NOTES <= "0001000";
				when "00110100" => NOTES <= "0000100";
				when "00110011" => NOTES <= "0000010";
				when "00111011" => NOTES <= "0000001";
				when others => NOTES <= "0000000";
			end case;
			BTNinputs	<= NOTES;
		elsif KEYBOARD_EN='0' THEN
			BTNinputs	<= (BTN_DO_DEB , BTN_RE_DEB , BTN_MI_DEB , BTN_FA_DEB , BTN_SOL_DEB , BTN_LA_DEB , BTN_SI_DEB);
		end if;
	end process;
--KEYBOARD ENABLER-------------
--OCTAVE DECIDER---------------
	process (CLK, OCTAVE_INC_DEB, OCTAVE_DEC_DEB)
	begin
		if rising_edge(CLK) then
			if OCTAVE_INC_DEB='1' AND OCTAVE/=3 AND flag/='1' then
				flag		<=	'1';
				OCTAVE	<=	OCTAVE + 1;
			elsif OCTAVE_INC_DEB='0' THEN
				flag		<= '0';
			end if;
			
			if OCTAVE_DEC_DEB='1' AND OCTAVE/=0 AND flag2/='1' then
				flag2		<=	'1';
				OCTAVE	<=	OCTAVE - 1;
			elsif OCTAVE_DEC_DEB='0' THEN
				flag2		<= '0';
			end if;
		end if;
	end process;
	process (OCTAVE, DO,RE,MI,FA,SOL,LA,SI)
	begin
		if OCTAVE=0 THEN
			DO		<=	22471; RE<=25229; MI<=28312; FA<=29996; SOL<=33673; LA<=37795; SI<=42426;--1st octave
		elsif OCTAVE=1 THEN
			DO		<=	44942; RE<=50458; MI<=56624; FA<=59992; SOL<=67346; LA<=75590; SI<=84852;--2nd octave
		elsif OCTAVE=2	THEN
			DO		<=	89884; RE<=100916; MI<=113248; FA<=119984; SOL<=134692; LA<=151180; SI<=169704;--3rd octave
		else
			DO		<=	179768; RE<=201832; MI<=226496; FA<=239968; SOL<=269384; LA<=302360; SI<=339408;--4th octave
		end if;
	end process;
--OCTAVE DECIDER---------------
	
	--Din	<= "000000000000";--12bit test data.
	--EN 	<=	'1'	;
	--BTNinputs	<= (BTN_DO , BTN_RE , BTN_MI , BTN_FA , BTN_SOL , BTN_LA , BTN_SI);
	--BTNinputs	<= ps2_code(6 downto 0);
	Addr	<=	"0011"; 			--Address for D out on DAC
	--DO		<=	22471; RE<=25229; MI<=28312; FA<=29996; SOL<=33673; LA<=37795; SI<=42426;
	--Din	<= ;
	
	DECIDER: process (CLK, BTNinputs, Temp13b, Din, DOout, REout, MIout, FAout, SOLout, LAout, SIout) is
	begin
		if rising_edge(CLK) then
			---ONE BUTTON PROBABILITIES---
			if BTNinputs="1000000" then
				EN 		<=	'1';
				Din		<=	DOout;
			elsif BTNinputs="0100000" then
				EN 		<=	'1';
				Din		<=	REout;
			elsif BTNinputs="0010000" then
				EN 		<=	'1';
				Din		<=	MIout;
			elsif BTNinputs="0001000" then
				EN 		<=	'1';
				Din		<=	FAout;
			elsif BTNinputs="0000100" then
				EN 		<=	'1';
				Din		<=	SOLout;
			elsif BTNinputs="0000010" then
				EN 		<=	'1';
				Din		<=	LAout;
			elsif BTNinputs="0000001" then
				EN 		<=	'1';
				Din		<=	SIout;
			---ONE BUTTON PROBABILITIES---
			
			---TWO PAIR BUTTON PROBABILITIES---
			elsif BTNinputs="1100000" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(DOout+REout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1010000" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(DOout+MIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1001000" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(DOout+FAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1000100" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(DOout+SOLout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1000010" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(DOout+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1000001" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(DOout+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0110000" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(REout+MIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0101000" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(REout+FAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0100100" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(REout+SOLout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0100010" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(REout+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0100001" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(REout+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0011000" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(MIout+FAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0010100" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(MIout+SOLout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0010010" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(MIout+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0010001" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(MIout+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0001100" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(FAout+SOLout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0001010" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(FAout+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0001001" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(FAout+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0000110" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(SOLout+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0000101" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(SOLout+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0000011" then
				EN 		<=	'1';
				Temp13b	<= std_logic_vector(LAout+SIout);
				Din		<=	Temp13b(12 downto 1);
			---TWO PAIR BUTTON PROBABILITIES---
			
			---THREE PAIR BUTTON PROBABILITIES---
			elsif BTNinputs="0000111" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(SOLout+LAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0001011" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(FAout+LAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0001110" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(FAout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0001101" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(FAout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0010011" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(MIout+LAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0010101" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(MIout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0011001" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(MIout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0010110" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(MIout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0011010" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(MIout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0011100" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(MIout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0100011" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+LAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0100101" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0101001" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0100110" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0101010" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0101100" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0110001" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0110010" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0110100" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="0111000" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(REout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+FAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1000011" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+LAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1000101" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1001001" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1001010" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1001100" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+FAout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1000110" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+SOLout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1010001" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1010010" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1010100" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1011000" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+MIout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+FAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1100001" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+REout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1100010" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+REout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1100100" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+REout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1101000" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+REout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+FAout);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1110000" then
				EN			<= '1';
				Temp13b	<= std_logic_vector(DOout+REout);
				Temp12b	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+MIout);
				Din		<=	Temp13b2(12 downto 1);
			---THREE PAIR BUTTON PROBABILITIES---
			
			---FOUR PAIR BUTTON PROBABILITIES---
			elsif BTNinputs="0001111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(FAout+SOLout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0010111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(MIout+SOLout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0011011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(MIout+FAout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0011101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(MIout+FAout+SOLout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0011110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(MIout+FAout+SOLout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0100111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+SOLout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0101011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+FAout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0101101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+FAout+SOLout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0101110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+FAout+SOLout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0110011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0110101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+SOLout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0110110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+SOLout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0111100" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+SOLout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0111010" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="0111001" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1000111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+SOLout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1001011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+FAout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1001101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+FAout+SOLout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1001110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+FAout+SOLout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1010011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1010101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+SOLout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1010110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+SOLout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1011100" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+FAout+SOLout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1011010" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+FAout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1011001" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+FAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1100011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+LAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1100101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+SOLout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1100110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+SOLout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1101100" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+FAout+SOLout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1101010" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+FAout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1101001" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+FAout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1110001" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SIout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1110010" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+LAout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1110100" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SOLout);
				Din		<=	Temp14b(13 downto 2);
			elsif BTNinputs="1111000" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+FAout);
				Din		<=	Temp14b(13 downto 2);
			---FOUR PAIR BUTTON PROBABILITIES---
			
			---FIVE PAIR BUTTON PROBABILITIES---
			elsif BTNinputs="0011111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(MIout+FAout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0101111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+FAout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0110111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0111011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0111101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="0111110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1001111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+FAout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1010111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1011011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+FAout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1011101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1011110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+FAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1100111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1101011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+FAout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1101101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1101110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+SOLout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+FAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1110011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1110110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+LAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1111100" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+FAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SOLout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1111010" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+FAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+LAout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1110101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			elsif BTNinputs="1111001" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+FAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(Temp12b+SIout);
				Din		<=	Temp13b(12 downto 1);
			---FIVE PAIR BUTTON PROBABILITIES---
			
			---SIX PAIR BUTTON PROBABILITIES---
			elsif BTNinputs="0111111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(REout+MIout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+SIout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1011111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+MIout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+SIout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1101111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+FAout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+SIout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1110111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+SIout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1111011" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+FAout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+SIout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1111101" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(FAout+SIout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			elsif BTNinputs="1111110" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+FAout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Din		<=	Temp13b2(12 downto 1);
			---SIX PAIR BUTTON PROBABILITIES---
			
			---SEVEN PAIR BUTTON PROBABILITY---
			elsif BTNinputs="1111111" then
				EN			<= '1';
				Temp14b	<= std_logic_vector(DOout+REout+MIout+SOLout);
				Temp12b	<=	Temp14b(13 downto 2);
				Temp13b	<=	std_logic_vector(LAout+FAout);
				Temp12b2	<=	Temp13b(12 downto 1);
				Temp13b2	<=	std_logic_vector(Temp12b+Temp12b2);
				Temp12b3	<=	Temp13b2(12 downto 1);
				Temp13b3	<= std_logic_vector(Temp12b3+SIout);
				Din		<=	Temp13b3(12 downto 1);
				
			---SEVEN PAIR BUTTON PROBABILITY---
			else
				EN			<=	'0';
			end if;
		end if;
		if EN='1' THEN
			DAC_CLR     <= '1';
		else
			DAC_CLR     <= '0';--NO buttons => DAC is cleared.
		end if;
	end process;
	
	
--MAIN CODE--------------------
end Behavioral;

