LIBRARY ieee;
USE ieee.std_logic_1164.all;

--ENTITY mazemovement IS
--	GENERIC(
--		signal	pixels_y :	INTEGER := 300;    --row that first color will persist until
--		signal	pixels_x	:	INTEGER := 400);
--	PORT(
--		disp_ena		:	IN		STD_LOGIC;	--display enable ('1' = display time, '0' = blanking time)
--		row			:	IN		INTEGER;		--row pixel coordinate
--		column		:	IN		INTEGER;		--column pixel coordinate
--		pbL			:	IN		STD_LOGIC;
--		pbR			:	IN		STD_LOGIC;
--		pbU			:	IN		STD_LOGIC;
--		pbD			:	IN		STD_LOGIC;
--		red			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
--		green			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
--		blue			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0')); --blue magnitude output to DAC
--END mazemovement;
--
--ARCHITECTURE behavior OF mazemovement IS
--SIGNAL pos_x : INTEGER := 0;
--SIGNAL pos_y : INTEGER := 0;
--BEGIN
--	PROCESS(disp_ena, row, column)
--	BEGIN
--
--		IF(disp_ena = '1') THEN		--display time
--			IF(row < pixels_y AND column < pixels_x) THEN
--				red <= (OTHERS => '1');
--				green	<= (OTHERS => '1');
--				blue <= (OTHERS => '0');
--			ELSE
--				red <= (OTHERS => '0');
--				green	<= (OTHERS => '0');
--				blue <= (OTHERS => '1');
--			END IF;
--			if(pbL='1' and pbR='0' and pbU = '0' and pbD = '0') then   
--				pos_x <= pos_x - 100; --top left corner
--		
---- right button pushed
--			elsif(pbL='0' and pbR ='1' and pbU = '0' and pbD = '0') then   
--				pos_x <= pos_x + 100; --top left corner
--		
---- top button pushed	
--			elsif(pbL='0' and pbR ='0' and pbU = '1' and pbD = '0') then   
--				pos_y <= pos_y + 100;
--		
---- bottom button pushed	
--			elsif(pbL='0' and pbR ='0' and pbU = '0' and pbD = '1') then   
--				pos_y <= pos_y - 100;
--			END IF;
--		ELSE								--blanking time
--			red <= (OTHERS => '0');
--			green <= (OTHERS => '0');
--			blue <= (OTHERS => '0');
--		END IF;
--	
--	END PROCESS;
--END behavior;

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY mazemovement IS
  PORT(
    disp_ena :  IN   STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
    row      :  IN   INTEGER;    --row pixel coordinate
    column   :  IN   INTEGER;    --column pixel coordinate
	 pbL			:	IN		STD_LOGIC;
	 pbR			:	IN		STD_LOGIC;
	 pbU			:	IN		STD_LOGIC;
	 pbD			:	IN		STD_LOGIC;
	 clk			:	IN		STD_LOGIC;
	 LED1			:	OUT		STD_LOGIC;
    red      :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
    green    :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
    blue     :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0')); --blue magnitude output to DAC
END mazemovement;

ARCHITECTURE behavior OF mazemovement IS

SIGNAL    framel :  INTEGER := 120;   --row that first color will persist until
SIGNAL	 framer:	INTEGER := 1800;
SIGNAL    framet :  INTEGER := 120;  --column that first color will persist until
SIGNAL	 frameb : INTEGER := 960;
SIGNAL	 chx : INTEGER := 20;
SIGNAL	 chy	: INTEGER := 140;
SIGNAL	 lmr		: STD_LOGIC;
SIGNAL	 lml		: STD_LOGIC;
SIGNAL	 lmu		: STD_LOGIC;
SIGNAL	 lmd		: STD_LOGIC;


BEGIN
	PROCESS(clk)
	BEGIN

	IF (falling_edge(clk)) THEN
		IF (pbR = '0' AND lmr = '1') THEN 
			
			chx <= chx + 120;
	
		END IF;
			lmr <= pbR;
		IF (pbL = '0' AND lml = '1') THEN
			
			chx <= chx - 120;
			
		END IF;
		lml <= pbL;
		IF (pbU = '0' AND lmu = '1') THEN
			
			chy <= chy - 120;
			
		END IF;
		lmu <= pbU;
		IF (pbD = '0' AND lmd = '1') THEN
			
			chy <= chy + 120;
			
		END IF;
		lmd <= pbD;
	END IF;
	END PROCESS;
  PROCESS(disp_ena, row, column)
  BEGIN
    IF(disp_ena = '1') THEN        --display time
		IF(column < framet OR column > frameb OR (column > framet*1 AND column < framet*2 AND ((row > framel*3 AND row < framel*4) OR 
		(row > framel*8 AND row < framel*9) OR (row > framel*15 AND row < framel*16))) OR (column > framet*2 AND column < framet*3 AND 
		((row < framel*2 ) OR (row > framel*3 AND row < framel*7) OR (row > framel*11 AND row < framel*16))) OR (column > framet*3 AND 
		column < framet*4 AND ((row < framel*1 ) OR (row > framel*6 AND row < framel*7) OR (row > framel*8 AND row < framel*10) OR 
		(row > framel*11 AND row < framel*12) OR (row > framel*15 AND row < framel*16))) OR (column > framet*4 AND 
		column < framet*5 AND ((row < framel*1 ) OR (row > framel*2 AND row < framel*5) OR (row > framel*6 AND row < framel*7) OR 
		(row > framel*8 AND row < framel*9) OR (row > framel*11 AND row < framel*12) OR (row > framel*13 AND row < framel*14) 
		OR (row > framel*15 AND row < framel*16))) OR (column > framet*5 AND column < framet*6 AND ((row < framel*1 ) OR 
		(row > framel*2 AND row < framel*3) OR (row > framel*13 AND row < framel*14) OR (row > framel*15 AND row < framel*16)))
		OR (column > framet*6 AND column < framet*7 AND ((row < framel*1 ) OR (row > framel*2 AND row < framel*3) OR 
		(row > framel*5 AND row < framel*14) OR (row > framel*15 AND row < framel*16))) OR (column > framet*7 AND column < framet*8 AND ((row < framel*1 ) OR (row > framel*2 AND row < framel*3) OR (row > framel*12 AND row < framel*13))))THEN
			red <= (OTHERS => '0');
        green  <= (OTHERS => '0');
        blue <= ("10110110");
      ELSE
        red <= ("10010110");
        green  <= ("10001100");
        blue <= (OTHERS => '0');
		  IF(column > (chy) AND column < (chy+80) AND row > chx AND row < (chx+80)) THEN
			red <= (OTHERS => '1');
        green  <= (OTHERS => '1');
        blue <= (OTHERS => '1');
		  	END IF;
      END IF;
		IF  (column > framet*7 AND column < framet*8 AND ((row > framel*15 AND row <framel*16))) THEN
			red <= (OTHERS => '0');
        green  <= (OTHERS => '1');
        blue <= (OTHERS => '0');
		  END IF;
    ELSE                           --blanking time
      red <= (OTHERS => '0');
      green <= (OTHERS => '0');
      blue <= (OTHERS => '0');
    END IF; 
--	IF (chy = (7*120 + 20) AND chx = (15*120 + 20)) THEN
--		LED1 <= '1';
--	END IF;
  END PROCESS;
END behavior;
