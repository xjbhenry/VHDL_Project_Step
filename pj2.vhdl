--=============================================================================
--  EE762 - Project Assignment #2
--
--  NAME: Jiabei Xu
-- 
--=============================================================================
--  Generic Function Unit Entity
-------------------------------------------------------------------------------
--  As you have already compiled your generic unit into the library
--  from step 1 you do not need to redeclare it here.
--  NOTE:  If for some reason you need to re-create your VHDL library
--         you will need to recompile pr_step 1.
-------------------------------------------------------------------------------
--  Enter your Entity for Carry Chain Unit here
-------------------------------------------------------------------------------
ENTITY carryChain IS 
  PORT (pIn, kIn, cIn: IN BIT;
        --pOut, cOut, cOutPlus1: OUT BIT
        cOut: OUT BIT);
END carryChain;

-------------------------------------------------------------------------------
--  Enter your Architecture for Carry Chain Unit here
-------------------------------------------------------------------------------
ARCHITECTURE behavioral OF carryChain IS
BEGIN
--pOut <= pIn;
--cOut <= cIn;
cOut <= (pIn AND cIn) OR ((NOT pIn) AND (NOT kIn)) AFTER 5 ns;
END behavioral;

-------------------------------------------------------------------------------
--  Enter your Entity for a 1 Bit ALU here
-------------------------------------------------------------------------------
ENTITY oneBitALU IS
  PORT (aIn, bIn, cIn: IN BIT;
        P, K, R: IN BIT_VECTOR(3 DOWNTO 0);
        zOut, cOut: OUT BIT);
END oneBitALU;

-------------------------------------------------------------------------------
--  Enter your structural Architecture for a 1 Bit ALU here
-------------------------------------------------------------------------------
ARCHITECTURE behavioral of oneBitALU IS
COMPONENT generic_function
  PORT (A,B,G3,G2,G1,G0: IN BIT;
        R: OUT BIT);
END COMPONENT;

COMPONENT carryChain
  PORT (pIn, kIn, cIn: IN BIT;
        cOut: OUT BIT);
END COMPONENT;

FOR ALL: generic_function USE ENTITY WORK.generic_function(behavioral);
FOR ALL: carryChain USE ENTITY WORK.carryChain(behavioral);

--PInt: P Internal
--KInt: K Internal
SIGNAL PInt, KInt: BIT;

BEGIN
PBlock: generic_function PORT MAP (aIn, bIn, P(3), P(2), P(1), P(0), PInt);
KBlock: generic_function PORT MAP (aIn, bIn, K(3), K(2), K(1), K(0), KInt);
CChain: carryChain PORT MAP (PInt, KInt, cIn, cOut);
RBlock: generic_function PORT MAP (PInt, cIn, R(3), R(2), R(1), R(0), zOut);
END behavioral;


-------------------------------------------------------------------------------
--  The Test bench for testing of the 1 bit ALU 
-------------------------------------------------------------------------------
ENTITY ps2 IS
END ps2;

-------------------------------------------------------------------------------
--  The Test Bench Architecture
-------------------------------------------------------------------------------
ARCHITECTURE test OF ps2 IS

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

  SIGNAL        Pval,Kval,Rval      : bit_vector (3 downto 0);
  SIGNAL        A,B,Cin,Zout,Cout    : bit;

  --  Enter the COMPONENT declaration and configuration for your
  --    1 bit ALU here 
  COMPONENT alu_1bit 
    PORT (aIn, bIn, cIn: IN BIT;
        P, K, R: IN BIT_VECTOR(3 DOWNTO 0);
        zOut, cOut: OUT BIT);
  END COMPONENT;
  FOR ALL: alu_1bit USE ENTITY WORK.oneBitALU(behavioral);

BEGIN  --  test 

  --  Enter the instantiation for your 1 bit ALU
  --  Note that the inputs are per the diagram in lecture (slide 8)
  --     except that only A and B are driven by the applytest process
  --  Also note that the P, K, and R controls are in Pval, Kval, and
  --     Rval which are bit_vectors (3 downto 0)
  --     They can be used a bit_vectors or the individual bits used,
  --     P(3),P(2), ...
  --  The carry input is Cin, the carry output Cout
  --  The data output of the alu slice is Zout
  --
  --  Instantiate Here
oneBitALU: alu_1bit PORT MAP (A, B, Cin, Pval, Kval, Rval, zOut, cOut);
	

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
       
       A <= '0'; B <= '0';
       WAIT FOR 25 ns;
       A <= '0'; B <= '1';
       WAIT FOR 25 ns;
       A <= '1'; B <= '0';
       WAIT FOR 25 ns;
       A <= '1'; B <= '1';
       WAIT FOR 25 ns;

       IF (i = 18 or i = 20) THEN
           Cin <= '1';
           A <= '0'; B <= '0';
           WAIT FOR 25 ns;
           A <= '0'; B <= '1';
           WAIT FOR 25 ns;
           A <= '1'; B <= '0';
           WAIT FOR 25 ns;
           A <= '1'; B <= '1';
           WAIT FOR 25 ns;
       END IF;

    END LOOP outter;
    WAIT;
  END PROCESS applytests;

END test;


