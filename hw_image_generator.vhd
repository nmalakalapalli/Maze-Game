--------------------------------------------------------------------------------
--
--   FileName:         hw_image_generator.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 64-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 05/10/2013 Scott Larson
--     Initial Public Release
--    
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY hw_image_generator IS
	PORT(
		disp_ena		:	IN		STD_LOGIC;	--display enable ('1' = display time, '0' = blanking time)
		row			:	IN		INTEGER;		--row pixel coordinate
		column		:	IN		INTEGER;		--column pixel coordinate
		pbL			:	IN		STD_LOGIC;
		pbR			:	IN		STD_LOGIC;
		pbU			:	IN		STD_LOGIC;
		pbD			:	IN		STD_LOGIC;
		red			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
		green			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
		blue			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0')); --blue magnitude output to DAC
END hw_image_generator;

ARCHITECTURE behavior OF hw_image_generator IS

signal	pixels_y :	INTEGER := 300;    --row that first color will persist until
signal	pixels_x	:	INTEGER := 400;   --column that first color will persist until

BEGIN
	PROCESS(disp_ena, row, column)
	BEGIN

		IF(disp_ena = '1') THEN		--display time
			IF(row < pixels_y AND column < pixels_x) THEN
				red <= (OTHERS => '1');
				green	<= (OTHERS => '1');
				blue <= (OTHERS => '0');
			ELSE
				red <= (OTHERS => '0');
				green	<= (OTHERS => '0');
				blue <= (OTHERS => '1');
			END IF;
			if(pbL='1' and pbR='0' and pbU = '0' and pbD = '0') then   
				pixels_x <= pixels_x - 100; --top left corner
		
-- right button pushed
			elsif(pbL='0' and pbR ='1' and pbU = '0' and pbD = '0') then   
				pixels_x <= pixels_x + 100; --top left corner
		
-- top button pushed	
			elsif(pbL='0' and pbR ='0' and pbU = '1' and pbD = '0') then   
				pixels_y <= pixels_y + 100;
		
-- bottom button pushed	
			elsif(pbL='0' and pbR ='0' and pbU = '0' and pbD = '1') then   
				pixels_y <= pixels_y - 100;
			END IF;
		ELSE								--blanking time
			red <= (OTHERS => '0');
			green <= (OTHERS => '0');
			blue <= (OTHERS => '0');
		END IF;
	
	END PROCESS;
END behavior;