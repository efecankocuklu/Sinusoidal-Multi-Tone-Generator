----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Efecan Kocuklu, Arda Kalender, Gizem Koca
-- 
-- Create Date:    10:41:51 12/10/2016 
-- Design Name: Tone Generator
-- Module Name:    ToneGen - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: adder32.vhd, REG32.vhd
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.NUMERIC_STD.ALL;
--use IEEE.std_logic_signed.all;
--library UNISIM;
--use UNISIM.VComponents.all;

entity ToneGen is 
	Port(
		CLK			: in 	std_logic; -- C9(50MHz Clock)
		TONEout12b	: out	std_logic_vector (11 downto 0);
		NOTEin		: in 	integer;
		EN				: in	std_logic
	);
	
end ToneGen;

architecture Behavioral of ToneGen is

	--signal Addr 	: STD_LOGIC_VECTOR(3 downto 0);  -- Which DAC pin
	--signal Din		: STD_LOGIC_VECTOR(11 downto 0); -- Analog Value
	--signal EN 		: STD_LOGIC;  -- enable falan filan
	--signal AddrSLUT: integer range 0 to 1023;
	signal cntr		: STD_LOGIC_VECTOR(11 downto 0);
	signal NOTE		: integer;
	signal DOUTtoA	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	signal StoDIN	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	signal K			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	signal Addr10b	: STD_LOGIC_VECTOR(9 DOWNTO 0);
	signal flag		: STD_LOGIC;
	
	type memory_type is array (0 to 1023) of integer range 0 to 2047; 
	--ROM for storing the sine values generated by "http://www.daycounter.com/Calculators/Sine-Generator-Calculator.phtml" using values 1023 of range max 511
	signal sinestart : memory_type :=(	16,16,16,16,16,16,16,16,
													16,16,16,17,17,17,17,17,
													17,17,17,17,17,17,18,18,
													18,18,18,18,18,18,18,18,
													38,38,38,38,38,39,39,39,
													39,39,40,40,40,40,40,40,
													41,41,41,41,41,42,42,42,
													42,42,42,43,43,43,43,43,
													66,66,66,66,67,67,67,68,
													68,68,68,69,69,69,69,70,
													70,70,70,71,71,71,71,72,
													72,72,72,73,73,73,73,74,
													99,99,99,100,100,100,101,101,
													101,102,102,102,103,103,103,104,
													104,104,104,105,105,105,106,106,
													106,106,107,107,107,108,108,108,
													136,136,136,137,137,137,138,138,
													138,139,139,139,140,140,140,141,
													141,141,142,142,142,143,143,143,
													143,144,144,144,145,145,145,145,
													175,175,176,176,176,177,177,177,
													177,178,178,178,179,179,179,179,
													180,180,180,181,181,181,181,182,
													182,182,182,183,183,183,183,184,
													215,215,215,215,216,216,216,216,
													217,217,217,217,217,218,218,218,
													218,218,219,219,219,219,219,220,
													220,220,220,220,220,220,221,221,
													253,253,253,253,253,253,253,254,
													254,254,254,254,254,254,254,254,
													254,254,255,255,255,255,255,255,
													255,255,255,255,255,255,255,255,
													287,287,287,287,287,287,287,287,
													287,287,287,287,287,287,286,286,
													286,286,286,286,286,286,286,286,
													285,285,285,285,285,285,285,284,
													316,316,315,315,315,315,315,314,
													314,314,314,313,313,313,313,312,
													312,312,311,311,311,311,310,310,
													310,309,309,309,308,308,307,307,
													338,337,337,336,336,335,335,334,
													334,334,333,333,332,332,331,331,
													330,330,329,329,328,327,327,326,
													326,325,325,324,324,323,322,322,
													351,350,349,349,348,347,346,346,
													345,344,344,343,342,341,341,340,
													339,339,338,337,336,335,335,334,
													333,332,332,331,330,329,328,327,
													354,353,352,351,350,349,348,347,
													346,346,345,344,343,342,341,340,
													339,338,337,336,335,334,333,332,
													331,330,329,328,327,326,324,323,
													347,346,345,344,343,341,340,339,
													338,337,336,334,333,332,331,330,
													328,327,326,325,323,322,321,320,
													319,317,316,315,314,312,311,310,
													331,329,328,326,325,324,322,321,
													320,318,317,315,314,313,311,310,
													308,307,306,304,303,301,300,298,
													297,296,294,293,291,290,288,287,
													305,303,302,300,298,297,295,294,
													292,291,289,288,286,284,283,281,
													280,278,277,275,274,272,270,269,
													267,266,264,263,261,259,258,256,
													271,269,267,266,264,262,261,259,
													257,256,254,252,251,249,247,246,
													244,242,241,239,237,236,234,232,
													231,229,228,226,224,223,221,219,
													230,229,227,225,224,222,220,218,
													217,215,213,212,210,208,207,205,
													203,201,200,198,196,195,193,191,
													190,188,186,185,183,181,180,178,
													186,185,183,181,180,178,176,174,
													173,171,169,168,166,164,163,161,
													159,158,156,155,153,151,150,148,
													147,145,143,142,140,139,137,136,
													141,139,138,136,135,133,131,130,
													128,127,125,124,122,120,119,117,
													116,114,113,111,110,108,107,105,
													104,103,101,100,98,97,95,94,
													97,96,94,93,92,90,89,87,
													86,85,83,82,81,79,78,77,
													75,74,73,71,70,69,68,66,
													65,64,63,62,60,59,58,57,
													58,57,56,55,54,53,52,50,
													49,48,47,46,45,44,43,42,
													41,40,39,38,37,36,35,34,
													33,32,31,30,30,29,28,27,
													27,27,26,25,24,23,22,22,
													21,20,19,19,18,17,17,16,
													15,15,14,13,13,12,12,11,
													11,10,10,9,9,8,8,7,
													7,7,6,6,5,5,5,4,
													4,4,3,3,3,2,2,2,
													2,1,1,1,1,1,1,0,
													0,0,0,0,0,0,0,0,
													0,0,0,0,0,0,0,0,
													1,1,1,1,1,1,2,2,
													2,2,3,3,3,4,4,4,
													5,5,5,6,6,7,7,8,
													8,9,9,10,11,11,12,12,
													13,14,14,15,16,16,17,18,
													18,19,20,21,22,22,23,24,
													25,26,27,28,29,30,30,31,
													34,35,36,37,38,39,40,41,
													42,44,45,46,47,48,49,51,
													52,53,55,56,57,58,60,61,
													63,64,65,67,68,70,71,72,
													77,78,80,81,83,85,86,88,
													89,91,93,94,96,98,100,101,
													103,105,107,108,110,112,114,116,
													118,119,121,123,125,127,129,131,
													138,140,142,144,146,148,150,152,
													154,156,158,161,163,165,167,169,
													171,174,176,178,180,183,185,187,
													189,192,194,196,199,201,203,206,
													215,218,220,223,225,228,230,233,
													235,238,240,243,246,248,251,253,
													256,258,261,264,266,269,272,274,
													277,280,282,285,288,290,293,296,
													309,311,314,317,320,323,326,328,
													331,334,337,340,343,346,349,352,
													354,357,360,363,366,369,372,375,
													378,381,384,387,390,393,396,399,
													415,418,421,424,427,430,433,436,
													439,443,446,449,452,455,458,461,
													464,468,471,474,477,480,483,486,
													490,493,496,499,502,505,508,512
	);
	signal sine : memory_type :=(	512,515,518,521,524,527,530,533,
											537,540,543,546,549,552,555,559,
											562,565,568,571,574,577,580,584,
											587,590,593,596,599,602,605,608,
											611,614,618,621,624,627,630,633,
											636,639,642,645,648,651,654,657,
											660,663,666,669,672,675,678,681,
											684,687,690,693,696,699,702,705,
											707,710,713,716,719,722,725,728,
											730,733,736,739,742,744,747,750,
											753,756,758,761,764,767,769,772,
											775,777,780,783,785,788,791,793,
											796,799,801,804,806,809,811,814,
											816,819,821,824,826,829,831,834,
											836,839,841,843,846,848,851,853,
											855,858,860,862,864,867,869,871,
											873,876,878,880,882,884,887,889,
											891,893,895,897,899,901,903,905,
											907,909,911,913,915,917,919,921,
											923,924,926,928,930,932,934,935,
											937,939,941,942,944,946,947,949,
											950,952,954,955,957,958,960,961,
											963,964,966,967,969,970,971,973,
											974,975,977,978,979,981,982,983,
											984,985,987,988,989,990,991,992,
											993,994,995,996,997,998,999,1000,
											1001,1002,1003,1004,1005,1005,1006,1007,
											1008,1009,1009,1010,1011,1011,1012,1013,
											1013,1014,1014,1015,1016,1016,1017,1017,
											1018,1018,1018,1019,1019,1020,1020,1020,
											1021,1021,1021,1021,1022,1022,1022,1022,
											1022,1023,1023,1023,1023,1023,1023,1023,
											1023,1023,1023,1023,1023,1023,1023,1022,
											1022,1022,1022,1022,1022,1021,1021,1021,
											1020,1020,1020,1019,1019,1019,1018,1018,
											1017,1017,1016,1016,1015,1015,1014,1014,
											1013,1012,1012,1011,1010,1010,1009,1008,
											1007,1007,1006,1005,1004,1003,1003,1002,
											1001,1000,999,998,997,996,995,994,
											993,992,991,990,988,987,986,985,
											984,982,981,980,979,977,976,975,
											973,972,971,969,968,966,965,964,
											962,961,959,958,956,954,953,951,
											950,948,946,945,943,941,940,938,
											936,934,933,931,929,927,925,924,
											922,920,918,916,914,912,910,908,
											906,904,902,900,898,896,894,892,
											890,888,885,883,881,879,877,875,
											872,870,868,866,863,861,859,856,
											854,852,849,847,845,842,840,837,
											835,833,830,828,825,823,820,818,
											815,813,810,808,805,802,800,797,
											795,792,789,787,784,781,779,776,
											773,771,768,765,762,760,757,754,
											751,749,746,743,740,737,735,732,
											729,726,723,720,718,715,712,709,
											706,703,700,697,694,691,688,685,
											683,680,677,674,671,668,665,662,
											659,656,653,650,647,644,640,637,
											634,631,628,625,622,619,616,613,
											610,607,604,601,597,594,591,588,
											585,582,579,576,573,569,566,563,
											560,557,554,551,548,544,541,538,
											535,532,529,526,522,519,516,513,
											510,507,504,501,497,494,491,488,
											485,482,479,475,472,469,466,463,
											460,457,454,450,447,444,441,438,
											435,432,429,426,422,419,416,413,
											410,407,404,401,398,395,392,389,
											386,383,379,376,373,370,367,364,
											361,358,355,352,349,346,343,340,
											338,335,332,329,326,323,320,317,
											314,311,308,305,303,300,297,294,
											291,288,286,283,280,277,274,272,
											269,266,263,261,258,255,252,250,
											247,244,242,239,236,234,231,228,
											226,223,221,218,215,213,210,208,
											205,203,200,198,195,193,190,188,
											186,183,181,178,176,174,171,169,
											167,164,162,160,157,155,153,151,
											148,146,144,142,140,138,135,133,
											131,129,127,125,123,121,119,117,
											115,113,111,109,107,105,103,101,
											99,98,96,94,92,90,89,87,
											85,83,82,80,78,77,75,73,
											72,70,69,67,65,64,62,61,
											59,58,57,55,54,52,51,50,
											48,47,46,44,43,42,41,39,
											38,37,36,35,33,32,31,30,
											29,28,27,26,25,24,23,22,
											21,20,20,19,18,17,16,16,
											15,14,13,13,12,11,11,10,
											9,9,8,8,7,7,6,6,
											5,5,4,4,4,3,3,3,
											2,2,2,1,1,1,1,1,
											1,0,0,0,0,0,0,0,
											0,0,0,0,0,0,0,1,
											1,1,1,1,2,2,2,2,
											3,3,3,4,4,5,5,5,
											6,6,7,7,8,9,9,10,
											10,11,12,12,13,14,14,15,
											16,17,18,18,19,20,21,22,
											23,24,25,26,27,28,29,30,
											31,32,33,34,35,36,38,39,
											40,41,42,44,45,46,48,49,
											50,52,53,54,56,57,59,60,
											62,63,65,66,68,69,71,73,
											74,76,77,79,81,82,84,86,
											88,89,91,93,95,97,99,100,
											102,104,106,108,110,112,114,116,
											118,120,122,124,126,128,130,132,
											134,136,139,141,143,145,147,150,
											152,154,156,159,161,163,165,168,
											170,172,175,177,180,182,184,187,
											189,192,194,197,199,202,204,207,
											209,212,214,217,219,222,224,227,
											230,232,235,238,240,243,246,248,
											251,254,256,259,262,265,267,270,
											273,276,279,281,284,287,290,293,
											295,298,301,304,307,310,313,316,
											318,321,324,327,330,333,336,339,
											342,345,348,351,354,357,360,363,
											366,369,372,375,378,381,384,387,
											390,393,396,399,402,405,409,412,
											415,418,421,424,427,430,433,436,
											439,443,446,449,452,455,458,461,
											464,468,471,474,477,480,483,486,
											490,493,496,499,502,505,508,512
	);
	
	component adder32 is
		port( A:in STD_LOGIC_VECTOR(31 DOWNTO 0);
				B:in STD_LOGIC_VECTOR(31 DOWNTO 0);
				S:out STD_LOGIC_VECTOR(31 DOWNTO 0);
				EN: in std_logic
		);
	end component;
	component REG32 is
		port( LOAD	:in STD_LOGIC;--This is clock input the same with top module CLK and connects to C9.
				DIN	:in STD_LOGIC_VECTOR(31 DOWNTO 0);
				DOUT	:out STD_LOGIC_VECTOR(31 DOWNTO 0)
		);
	end component;
	
begin

--ADDER COMPONENT-----------
	ADDER: adder32 port map (
		A =>	DOUTtoA	,
		B =>	K			,
		S =>	StoDIN	,
		EN=>	EN
	);
--ADDER COMPONENT-----------

--REG32 COMPONENT-----------
	REG: REG32 port map (
		LOAD	=>	CLK		,
		DIN	=> StoDIN	,
		DOUT	=>	DOUTtoA	
	);
--REG32 COMPONENT-----------

--MAIN VALUE GENERATOR CODE------------------------

	--K		<= std_logic_vector(to_unsigned(NOTE, K'length));--NOTE value integer to std_logic_vector. example:18898=220Hz sine
	--THE FORMULA IS fsin=K*(50000000/2^32)
	Addr10b(9 downto 0) <= DOUTtoA(31 downto 22);
	--TONEout12b	<= std_logic_vector(to_signed(sine(to_integer(unsigned(Addr10b))), Addr10b'length));
	K				<= std_logic_vector(to_unsigned(NOTEin, K'length));--integer to std_logic_vector.37796=440Hz sine
	

	
	process is
	begin
		--if rising_edge(CLK) then
			if flag/='1' AND EN='1' then
				TONEout12b	<= std_logic_vector(to_signed(sinestart(to_integer(unsigned(Addr10b))), Addr10b'length));
			elsif EN='1' AND flag='1' then
				TONEout12b	<= std_logic_vector(to_signed(sine(to_integer(unsigned(Addr10b))), Addr10b'length));
			end if;
			
			if Addr10b="1111111111" AND flag/='1' AND EN='1' then
				flag		<=	'1';
			elsif	EN='0' THEN
				flag		<=	'0';
			end if;
		--end if;
	end process;
--MAIN VALUE GENERATOR CODE------------------------

end Behavioral;
