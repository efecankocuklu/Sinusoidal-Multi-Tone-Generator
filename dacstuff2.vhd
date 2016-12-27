----------------------------------------------------------------------------------
-- DAC handler 
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity dacstuff2 is
  Port ( clk : in  STD_LOGIC; -- Max 50 MHz
          CS : out STD_LOGIC; -- N8
        MOSI : out STD_LOGIC; -- T4
         SCK : out STD_LOGIC; -- U16
         Din : in  STD_LOGIC_VECTOR(11 downto 0); -- Deðer
        Addr : in  STD_LOGIC_VECTOR(3 downto 0);  -- Hangi DAC
          EN : in  STD_LOGIC  -- EN=1 enable falan filan
  );
end dacstuff2;

architecture Behavioral of dacstuff2 is
  signal cntr : integer range 0 to 32;
  signal SR : STD_LOGIC_VECTOR(31 downto 0);
begin
  -- Top-Level dosyada bunlarý da koy ve pinlere baðla --
  --DAC_CLR     <= '1'; -- P8
  --FPGA_INIT_B <= '1'; -- T3
  --AMP_CS      <= '1'; -- N7
  --AD_CONV     <= '0'; -- P11
  --SF_CE0      <= '1'; -- D16
  --SPI_SS_B    <= '1'; -- U3
  -------------------------------------------------------
  
  MOSI <= SR(31);
  SCK <= clk;
  
  -- SPI'dan gönderilecek 32 bitlik veriyi SR'a yüklüyor
  -- Sonra da tek tek gönderiyor. 
  process(clk) is begin
    if(falling_edge(clk)) then
      if(EN='1') then
        if(cntr=0) then
          SR <= "00000000" & "0011" & Addr & Din & "0000";
          CS <= '0';
        else
          SR(31 downto 1) <= SR(30 downto 0);
        end if;
        if(cntr=32) then 
          cntr <= 0;
          CS <= '1';
        else
          cntr <= cntr+1;
        end if;
      else
        CS <= '1';
      end if;
    end if;
  end process;

end Behavioral;
