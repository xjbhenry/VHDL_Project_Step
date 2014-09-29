-------------------------------------------------------------------------------
--PROJECT Step 3 - Component Instantiation and the Generate Statement
--
--NAME:
--
--YOU CAN USE THE ENTITIES AND ARCHITECUTRES FROM STEP 2
--For the generic_function, carry_chain, and 1_bit_ALU
--They are in the library and if you completed the last assignment
--correctly they can again be used here.
-------------------------------------------------------------------------------
--  Enter your Entity for an 8 Bit ALU here
-------------------------------------------------------------------------------
ENTITY eightBitALU IS
  PORT (aIn, bIn:        IN BIT_VECTOR(7 DOWNTO 0);
        zOut:            OUT BIT_VECTOR(7 DOWNTO 0);
        pIn, kIn, rIn:   IN BIT_VECTOR(3 DOWNTO 0);
        cIn:             IN BIT;
        cOut:            OUT BIT);
END ENTITY;
-------------------------------------------------------------------------------
--  Enter your Structural Architecture for a 8 Bit ALU here
--    using 8 component instantiations
-------------------------------------------------------------------------------
ARCHITECTURE behavioral OF eightBitALU IS
COMPONENT oneBitALU
  PORT (aIn, bIn, cIn: IN BIT;
        P, K, R: IN BIT_VECTOR(3 DOWNTO 0);
        zOut, cOut:    OUT BIT);
END COMPONENT;

FOR ALL: oneBitALU USE ENTITY WORK.oneBitALU(behavioral);
SIGNAL cInt: BIT_VECTOR(6 DOWNTO 0);

BEGIN
ALU0: oneBitALU PORT MAP (aIn(0), bIn(0), cIn, pIn, kIn, rIn, zOut(0), cInt(0));
ALU1: oneBitALU PORT MAP (aIn(1), bIn(1), cInt(0), pIn, kIn, rIn, zOut(1), cInt(1));
ALU2: oneBitALU PORT MAP (aIn(2), bIn(2), cInt(1), pIn, kIn, rIn, zOut(2), cInt(2));
ALU3: oneBitALU PORT MAP (aIn(3), bIn(3), cInt(2), pIn, kIn, rIn, zOut(3), cInt(3));
ALU4: oneBitALU PORT MAP (aIn(4), bIn(4), cInt(3), pIn, kIn, rIn, zOut(4), cInt(4));
ALU5: oneBitALU PORT MAP (aIn(5), bIn(5), cInt(4), pIn, kIn, rIn, zOut(5), cInt(5));
ALU6: oneBitALU PORT MAP (aIn(6), bIn(6), cInt(5), pIn, kIn, rIn, zOut(6), cInt(6));
ALU7: oneBitALU PORT MAP (aIn(7), bIn(7), cInt(6), pIn, kIn, rIn, zOut(7), cOut);
END behavioral;
-------------------------------------------------------------------------------
--  Enter your Structural Architecture for a 8 Bit ALU here
--    using component instantiation for the lsb and msb and 
--    a generate statement for the remaining slices
-------------------------------------------------------------------------------
ARCHITECTURE good OF eightBitALU IS
COMPONENT oneBitALU
  PORT (aIn, bIn, cIn: IN BIT;
        P, K, R: IN BIT_VECTOR(3 DOWNTO 0);
        zOut, cOut:    OUT BIT);
END COMPONENT;

FOR ALL: oneBitALU USE ENTITY WORK.oneBitALU(behavioral);
SIGNAL cInt: BIT_VECTOR(6 DOWNTO 0);

BEGIN
ALU0: oneBitALU PORT MAP (aIn(0), bIn(0), cIn, pIn, kIn, rIn, zOut(0), cInt(0));
ALU0to6: FOR i IN 1 TO 6 GENERATE
  ALU: oneBitALU PORT MAP (aIn(i), bIn(i), cInt(i-1), pIn, kIn, rIn, zOut(i), cInt(i));
END GENERATE; 
ALU7: oneBitALU PORT MAP (aIn(7), bIn(7), cInt(6), pIn, kIn, rIn, zOut(7), cOut);
END good;

-------------------------------------------------------------------------------
--  Enter your structural Architecture for a 8 Bit ALU here
--    using a generate statement with conditions that handle
--    the lsb, msb, and intermediate slices
-------------------------------------------------------------------------------
ARCHITECTURE structural OF eightBitALU IS
COMPONENT oneBitALU
  PORT (aIn, bIn, cIn: IN BIT;
        P, K, R: IN BIT_VECTOR(3 DOWNTO 0);
        zOut, cOut:    OUT BIT);
END COMPONENT;

FOR ALL: oneBitALU USE ENTITY WORK.oneBitALU(behavioral);
SIGNAL cInt: BIT_VECTOR(6 DOWNTO 0);

BEGIN
ALU_all: FOR i IN 0 TO 7 GENERATE
  lsb: IF i = 0 GENERATE
    ALU0: oneBitALU PORT MAP (aIn(i), bIn(i), cIn, pIn, kIn, rIn, zOut(i), cInt(i));
  END GENERATE;
  mid: IF i > 0 AND i < 7 GENERATE
    ALU1to6: oneBitALU PORT MAP (aIn(i), bIn(i), cInt(i-1), pIn, kIn, rIn, zOut(i), cInt(i));
  END GENERATE;
  msb: IF i = 7 GENERATE
    ALU7: oneBitALU PORT MAP (aIn(i), bIn(i), cInt(i-1), pIn, kIn, rIn, zOut(i), cOut);
  END GENERATE;
END GENERATE;
END structural;
-------------------------------------------------------------------------------
--  The Test bench for testing of the 8 bit ALU 
-------------------------------------------------------------------------------
ENTITY p3 IS
END p3;

-------------------------------------------------------------------------------
--  The Test Bench Architecture
-------------------------------------------------------------------------------
ARCHITECTURE test OF p3 IS

  TYPE operations IS (op_A,op_B,op_notA,op_notB,op_AxorB,op_AorB,op_AandB,
                    op_AnandB,op_AxnorB,op_0,op_1,op_incA,op_incB,op_decA,
                    op_decB,op_negA,op_negB,op_AplusB,op_AplusBwC,
                    op_AminB,op_AminBwC,op_BminA);
  TYPE oper_type  IS ARRAY (0 to 21) of operations;
  CONSTANT      oper_tbl     : oper_type := (op_A,op_B,op_notA,op_notB,
                   op_AxorB,op_AorB,op_AandB,op_AnandB,op_AxnorB,op_0,
                   op_1,op_incA,op_incB,op_decA,op_decB,op_negA,op_negB,
                   op_AplusB,op_AplusBwC,op_AminB,op_AminBwC,op_BminA);
  SIGNAL        oper : operations;

  TYPE  val_tbl_type IS ARRAY (0 to 21) of bit_vector (3 downto 0);
  CONSTANT pval_tbl : val_tbl_type :=
              ("1100","1010","0011","0101","0110","1110","1000","0111",
               "1001","0000","1111","1100","1010","0011","0101","0011",
               "0101","0110","0110","1001","1001","1001");
  CONSTANT kval_tbl : val_tbl_type :=
              ("1111","1111","1111","1111","1111","1111","1111","1111",
               "1111","1111","1111","0011","0101","1100","1010","1100",
               "1010","0001","0001","0100","0100","0010");
  CONSTANT rval_tbl : val_tbl_type :=
              ("1100","1100","1100","1100","1100","1100","1100","1100",
               "1100","1100","1100","0110","0110","1001","1001","0110",
               "0110","0110","0110","1001","1001","1001");

  SIGNAL        Pval,Kval,Rval   : bit_vector (3 downto 0);
  SIGNAL        A,B,Za,Zb,Zc     : bit_vector (7 downto 0);
  SIGNAL        Cin,Coa,Cob,Coc  : bit;

  TYPE z_check_array IS ARRAY (1 TO 168) OF bit_vector (7 downto 0);
  CONSTANT z_check : z_check_array := (
"00000000","00000000","11111111","11111111","01010101","11110000","10110010",
"00000000","11111111","00000000","11111111","10101010","00001111","00101011",
"11111111","11111111","00000000","00000000","10101010","00001111","01001101",
"11111111","00000000","11111111","00000000","01010101","11110000","11010100",
"00000000","11111111","11111111","00000000","11111111","11111111","10011001",
"00000000","11111111","11111111","11111111","11111111","11111111","10111011",
"00000000","00000000","00000000","11111111","00000000","00000000","00100010",
"11111111","11111111","11111111","00000000","11111111","11111111","11011101",
"11111111","00000000","00000000","11111111","00000000","00000000","01100110",
"00000000","00000000","00000000","00000000","00000000","00000000","00000000",
"11111111","11111111","11111111","11111111","11111111","11111111","11111111",
"00000001","00000001","00000000","00000000","01010110","11110001","10110011",
"00000001","00000000","00000001","00000000","10101011","00010000","00101100",
"11111111","11111111","11111110","11111110","01010100","11101111","10110001",
"11111111","11111110","11111111","11111110","10101001","00001110","00101010",
"00000000","00000000","00000001","00000001","10101011","00010000","01001110",
"00000000","00000001","00000000","00000001","01010110","11110001","11010101",
"00000000","11111111","11111111","11111110","11111111","11111111","11011101",
"00000000","11111111","11111111","11111110","11111111","11111111","11011101",
"00000001","00000000","00000000","11111111","00000000","00000000","11011110",
"00000000","00000001","11111111","00000000","10101011","11100001","10000111",
"00000000","00000001","11111111","00000000","10101011","11100001","10000111",
"11111111","00000000","11111110","11111111","10101010","11100000","10000110",
"00000000","11111111","00000001","00000000","01010101","00011111","01111001");
  TYPE cout_check_array IS ARRAY (1 to 168) OF bit;
  CONSTANT cout_check : cout_check_array := (
   '0','0','0','0','0','0','0', '0','0','0','0','0','0','0',
   '0','0','0','0','0','0','0', '0','0','0','0','0','0','0',
   '0','0','0','0','0','0','0', '0','0','0','0','0','0','0',
   '0','0','0','0','0','0','0', '0','0','0','0','0','0','0',
   '0','0','0','0','0','0','0', '0','0','0','0','0','0','0',
   '0','0','0','0','0','0','0', '0','0','1','1','0','0','0',
   '0','1','0','1','0','0','0', '1','1','0','0','0','0','0',
   '1','0','1','0','0','0','0', '1','1','0','0','0','0','0',
   '1','0','1','0','0','0','0', '0','0','0','1','0','0','0',
   '0','0','0','1','0','0','0', '0','1','1','1','1','1','0',
   '0','1','0','0','1','0','0', '0','1','0','0','1','0','0',
   '1','1','0','1','1','0','0', '0','0','1','0','0','1','1');
SIGNAL z_err,cout_err : BIT;
SIGNAL z_exp : BIT_VECTOR (7 downto 0);
SIGNAL co_exp : BIT;
   
  --  Enter your name in the (  )
  TYPE mname IS (Jiabei_XU);
  SIGNAL nm : mname := mname'VAL(0);

  --  Enter the COMPONENT declaration and configurations for your
  --  8 bit ALUs here 
  --  Note that you need only 1 COMPONENT declaration but 3
  --  FOR  xxx : ... configurations statements where xxx will be
  --  the label you use for the given configuration instantiation.

  COMPONENT eightBitALU
    PORT (aIn, bIn:        IN BIT_VECTOR(7 DOWNTO 0);
          zOut:            OUT BIT_VECTOR(7 DOWNTO 0);
          pIn, kIn, rIn:   IN BIT_VECTOR(3 DOWNTO 0);
          cIn:             IN BIT;
          cOut:            OUT BIT);
  END COMPONENT;

  FOR eightBitALU0: eightBitALU USE ENTITY WORK.eightBitALU(behavioral);
  FOR eightBitALU1: eightBitALU USE ENTITY WORK.eightBitALU(good);
  FOR eightBitALU2: eightBitALU USE ENTITY WORK.eightBitALU(structural);

BEGIN  --  test 

  --  Enter the instantiations for your 8 bit ALU Entity/Architecture
  --  Note that the inputs are per the diagram in lecture (slide 8)
  --     except that only A, B, and Cin are driven by the applytest process
  --     Note that A and B are now Bit vectors!!!
  --  Also note that the P, K, and R controls are in Pval, Kval, and
  --     Rval which are bit_vectors (3 downto 0)
  --  The carry input is Cin
  --  Note that the three instantiations all have the same inputs.
  --  The outputs are different.
  --  The data and carry out of the alu slice is 
  --     Za and Coa for the 8 component instantiation case
  --     Zb and Cob for the first generate stmt case
  --     Zc and Coc for the second generate stmt case. 
  --  Note that Za/Zb/Zc output signals are bit vectors.
  --     and the Coa, Cob, Coc are a single bit.
  eightBitALU0: eightBitALU PORT MAP (A, B, Za, Pval, Kval, Rval, Cin, Coa);
  eightBitALU1: eightBitALU PORT MAP (A, B, Zb, Pval, Kval, Rval, Cin, Cob);
  eightBitALU2: eightBitALU PORT MAP (A, B, Zc, Pval, Kval, Rval, Cin, Coc);

  applytests : PROCESS
  BEGIN  --  PROCESS applytests 
    outter: FOR i IN 0 TO 21 LOOP
       oper <= oper_tbl(i);
	   Pval <= pval_tbl(i);
	   Kval <= kval_tbl(i);
	   Rval <= rval_tbl(i);

       IF (i >= 11 and i <= 16) THEN Cin <= '1';
                                ELSE Cin <= '0';
       END IF;
       
	   A <= "00000000"; B <= "00000000";
	   WAIT FOR 100 ns;
	   A <= "00000000"; B <= "11111111";
	   WAIT FOR 100 ns;
	   A <= "11111111"; B <= "00000000";
	   WAIT FOR 100 ns;
	   A <= "11111111"; B <= "11111111";
	   WAIT FOR 100 ns;
	   A <= "01010101"; B <= "10101010";
	   WAIT FOR 100 ns;
	   A <= "11110000"; B <= "00001111";
	   WAIT FOR 100 ns;
	   A <= "10110010"; B <= "00101011";
	   WAIT FOR 100 ns;

       IF (i = 18 or i = 20) THEN
           Cin <= '1';
	       A <= "00000000"; B <= "00000000";
	       WAIT FOR 100 ns;
	       A <= "00000000"; B <= "11111111";
	       WAIT FOR 100 ns;
	       A <= "11111111"; B <= "00000000";
	       WAIT FOR 100 ns;
	       A <= "11111111"; B <= "11111111";
	       WAIT FOR 100 ns;
	       A <= "01010101"; B <= "10101010";
	       WAIT FOR 100 ns;
	       A <= "11110000"; B <= "00001111";
	       WAIT FOR 100 ns;
	       A <= "10110010"; B <= "00101011";
	       WAIT FOR 100 ns;
       END IF;

	END LOOP outter;
	WAIT;
  END PROCESS applytests;


check : PROCESS
BEGIN
  z_err <= '0';
  cout_err <= '0';
  WAIT FOR 99 ns;
  FOR i IN 1 to 168 LOOP
    IF (Za /= z_check(i)) THEN z_err <= '1', '0' AFTER 1 ns;
    END IF;
    IF (Coa /= cout_check(i)) THEN cout_err <= '1', '0' AFTER 1 ns;
    END IF;
    WAIT FOR 1 ns;
    z_exp <= z_check(i);
    co_exp <= cout_check(i);
    WAIT FOR 99 ns;
  END LOOP;
  WAIT;
END PROCESS check;

nm <= mname'VAL(0);

END test;
