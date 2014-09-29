-------------------------------------------------------------------------------
-- Project Step 6 - Packages 
--
-- NAME: Jiabei XU
--
-------------------------------------------------------------------------------
--  Package Declarative Part Here
-------------------------------------------------------------------------------
PACKAGE alu_op IS
 TYPE operations IS (op_A,op_B,op_notA,op_notB,op_AxorB,op_AorB,op_AandB,
                     op_AnandB,op_AxnorB,op_0,op_1,op_incA,op_incB,op_decA,
                     op_decB,op_negA,op_negB,op_AplusB,op_AplusBwC,
                     op_AminB,op_AminBwC,op_BminA);
 PROCEDURE BINADD (a, b: IN BIT_VECTOR;
                   c_in: IN BIT;
                   z_out: OUT BIT_VECTOR;
                   c_out: OUT BIT);
 PROCEDURE BINSUB (a, b: IN BIT_VECTOR;
                   b_in: IN BIT;
                   z_out: OUT BIT_VECTOR;
                   b_out: OUT BIT);
 PROCEDURE COMP_2 (num: IN BIT_VECTOR;
                   z_out: OUT BIT_VECTOR;
                   c_out: OUT BIT);
END alu_op;
-------------------------------------------------------------------------------
--  Package Body Here
-------------------------------------------------------------------------------
PACKAGE BODY alu_op IS
 --addition
  PROCEDURE BINADD (a, b: IN BIT_VECTOR;
                    c_in: IN BIT;
                    z_out: OUT BIT_VECTOR;
                    c_out: OUT BIT) IS
  VARIABLE carry: BIT;
  BEGIN
  carry := c_in;
  FOR i IN a'REVERSE_RANGE LOOP
    z_out(i) := a(i) XOR b(i) XOR carry;
    carry := (a(i) AND b(i)) OR (a(i) AND carry) OR (b(i) AND carry);
  END LOOP;
  c_out := carry; 
  END BINADD;
  --subraction  
  PROCEDURE BINSUB (a, b: IN BIT_VECTOR;
                    b_in: IN BIT;
                    z_out: OUT BIT_VECTOR;
                    b_out: OUT BIT) IS
  VARIABLE borrow: BIT;
  BEGIN
  borrow := b_in;
  FOR i IN a'REVERSE_RANGE LOOP
    z_out(i) := a(i) XOR b(i) XOR borrow;
    borrow := (NOT a(i) AND borrow) OR (b(i) AND borrow) OR (NOT a(i) AND b(i));
  END LOOP;
  b_out := borrow;
  END BINSUB;
 --2's complement
  PROCEDURE COMP_2 (num: IN BIT_VECTOR;
                    z_out: OUT BIT_VECTOR;
                    c_out: OUT BIT) IS
  VARIABLE notNum, Zero: BIT_VECTOR (num'RANGE);
  BEGIN
    notNum := NOT num;
    BINADD(notNum, ZERO, '1', z_out, c_out);
  END COMP_2;
END alu_op;

-------------------------------------------------------------------------------
--  Enter the revised Entity for the 8 Bit ALU here
-------------------------------------------------------------------------------
USE WORK.alu_op.ALL;
ENTITY eightBitALU IS
  PORT (aIn, bIn:        IN BIT_VECTOR(7 DOWNTO 0);
        zOut:            OUT BIT_VECTOR(7 DOWNTO 0);
        aluOp:           IN operations;
        cIn:             IN BIT;
        cOut:            OUT BIT);
END ENTITY;
-------------------------------------------------------------------------------
-- Enter your Architecture using the modified CASE stmt for the 8 Bit ALU here
-------------------------------------------------------------------------------
ARCHITECTURE process_v2 OF eightBitALU IS
BEGIN
  PROCESS(aIn, bIn, aluOp, cIn)
    VARIABLE cOutTemp: BIT;
    VARIABLE zOutTemp: BIT_VECTOR(7 DOWNTO 0);
    BEGIN
    CASE aluOp IS
      WHEN op_A => zOut <= aIn; cOut <= '0'; --opA
      WHEN op_b => zOut <= bIn; cOut <= '0'; --opB
      WHEN op_notA => zOut <= NOT aIn; cOut <= '0'; --opNotA
      WHEN op_notB => zOut <= NOT bIn; cOut <= '0'; --opNotB
      WHEN op_AxorB => zOut <= aIn XOR bIn; cOut <= '0'; --opAxorB
      WHEN op_AorB => zOut <= aIn OR bIn; cOut <= '0'; --opAorB
      WHEN op_AandB => zOut <= aIn AND bIn; cOut <= '0'; --opAandB
      WHEN op_AnandB => zOut <= aIn NAND bIn; cOut <= '0'; --opAnandB
      WHEN op_AxnorB => zOut <= aIn XNOR bIn; cOut <= '0'; --opAxnorB
      WHEN op_0 => zOut <= "00000000"; cOut <= '0'; --op0
      WHEN op_1 => zOut <= "11111111"; cOut <= '0'; --op1
      WHEN op_incA => BINADD(aIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opIncA
      WHEN op_incB => BINADD(bIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opIncB
      WHEN op_decA => BINSUB(aIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opDecA
      WHEN op_decB => BINSUB(bIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opDecB
      WHEN op_negA => COMP_2(aIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opNegA
      WHEN op_negB => COMP_2(bIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opNegB
      WHEN op_AplusB | op_AplusBwC => BINADD(aIn, bIn, cIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opA+BwC
      WHEN op_AminB | op_AminBwC => BINSUB(aIn, bIn, cIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opA-BwC
      WHEN op_BminA => BINSUB(bIn, aIn, cIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opB-A
      WHEN OTHERS => NULL;
    END CASE;
  END PROCESS;
END process_v2;
-------------------------------------------------------------------------------
--  The Test bench for testing of the 8 bit ALU 
-------------------------------------------------------------------------------
USE WORK.alu_op.ALL;
ENTITY p6 IS
END p6;

-------------------------------------------------------------------------------
--  The Test Bench Architecture
-------------------------------------------------------------------------------
ARCHITECTURE test OF p6 IS

  --TYPE operations IS (op_A,op_B,op_notA,op_notB,op_AxorB,op_AorB,op_AandB,
  --                  op_AnandB,op_AxnorB,op_0,op_1,op_incA,op_incB,op_decA,
  --                  op_decB,op_negA,op_negB,op_AplusB,op_AplusBwC,
  --                  op_AminB,op_AminBwC,op_BminA);
  TYPE oper_type  IS ARRAY (0 to 21) of operations;
  CONSTANT      oper_tbl     : oper_type := (op_A,op_B,op_notA,op_notB,
                   op_AxorB,op_AorB,op_AandB,op_AnandB,op_AxnorB,op_0,
                   op_1,op_incA,op_incB,op_decA,op_decB,op_negA,op_negB,
                   op_AplusB,op_AplusBwC,op_AminB,op_AminBwC,op_BminA);
  SIGNAL        oper : operations;

  TYPE result_table IS ARRAY(op_A to op_BminA,1 to 7) of bit_vector(7 downto 0);
  TYPE result_array is ARRAY(1 to 7) of bit_vector(7 downto 0);
  CONSTANT res_tbl : result_table :=
(("00000000","00000000","11111111","11111111","01010101","11110000","10110010"),-- op_A
("00000000","11111111","00000000","11111111","10101010","00001111","00101011"),-- op_B
("11111111","11111111","00000000","00000000","10101010","00001111","01001101"),-- not_A
("11111111","00000000","11111111","00000000","01010101","11110000","11010100"),-- not_B
("00000000","11111111","11111111","00000000","11111111","11111111","10011001"),-- AxorB
("00000000","11111111","11111111","11111111","11111111","11111111","10111011"),-- A or B
("00000000","00000000","00000000","11111111","00000000","00000000","00100010"),-- AandB
("11111111","11111111","11111111","00000000","11111111","11111111","11011101"),-- AnandB
("11111111","00000000","00000000","11111111","00000000","00000000","01100110"),-- AxnorB
("00000000","00000000","00000000","00000000","00000000","00000000","00000000"),-- op_0
("11111111","11111111","11111111","11111111","11111111","11111111","11111111"),-- op_1
("00000001","00000001","00000000","00000000","01010110","11110001","10110011"),-- inc A
("00000001","00000000","00000001","00000000","10101011","00010000","00101100"),-- inc B
("11111111","11111111","11111110","11111110","01010100","11101111","10110001"),-- dec A
("11111111","11111110","11111111","11111110","10101001","00001110","00101010"),-- dec B
("00000000","00000000","00000001","00000001","10101011","00010000","01001110"),-- neg_A
("00000000","00000001","00000000","00000001","01010110","11110001","11010101"),-- neg_B
("00000000","11111111","11111111","11111110","11111111","11111111","11011101"),-- AplusB
("00000000","11111111","11111111","11111110","11111111","11111111","11011101"),--A+B+0
("00000000","00000001","11111111","00000000","10101011","11100001","10000111"),-- A-B
("00000000","00000001","11111111","00000000","10101011","11100001","10000111"),-- A-B-0
("00000000","11111111","00000001","00000000","01010101","00011111","01111001"));-- B-A
  CONSTANT AplBpl1_tbl : result_array :=
("00000001","00000000","00000000","11111111","00000000","00000000","11011110");-- A+B+1
  CONSTANT AmBm1_tbl : result_array :=
("11111111","00000000","11111110","11111111","10101010","11100000","10000110");-- A-B-1
  TYPE carry_result_table IS ARRAY(op_A to op_BminA,1 to 7) of bit;
  TYPE carry_result_array IS ARRAY (1 to 7) of bit;
  CONSTANT carry_res_tbl : carry_result_table :=
                    (('0','0','0','0','0','0','0'), -- op_A
                     ('0','0','0','0','0','0','0'), -- op_B
                     ('0','0','0','0','0','0','0'), -- notA
                     ('0','0','0','0','0','0','0'), -- notB
                     ('0','0','0','0','0','0','0'), -- AxorB
                     ('0','0','0','0','0','0','0'), -- AorB
                     ('0','0','0','0','0','0','0'), -- AandB
                     ('0','0','0','0','0','0','0'), -- AnandB
                     ('0','0','0','0','0','0','0'), -- AnxorB
                     ('0','0','0','0','0','0','0'), -- op_0
                     ('0','0','0','0','0','0','0'), -- op_1
                     ('0','0','1','1','0','0','0'), -- incA
                     ('0','1','0','1','0','0','0'), -- incB
                     ('1','1','0','0','0','0','0'), -- decA
                     ('1','0','1','0','0','0','0'), -- decB
                     ('1','1','0','0','0','0','0'), -- negA
                     ('1','0','1','0','0','0','0'), -- negB
                     ('0','0','0','1','0','0','0'), -- A+B
                     ('0','0','0','1','0','0','0'), -- A+B+0
                     ('0','1','0','0','1','0','0'), -- A-B
                     ('0','1','0','0','1','0','0'), -- A-B-0
                     ('0','0','1','0','0','1','1')); -- B-A
  CONSTANT carry_AplBpl1_tbl : carry_result_array :=
                     ('0','1','1','1','1','1','0'); -- A+B+1
  CONSTANT carry_AmBm1_tbl : carry_result_array :=
                     ('1','1','0','1','1','0','0'); -- A-B-1

  SIGNAL        A,B,Zout    : bit_vector (7 downto 0);
  SIGNAL        Cin,Cout    : bit;
  SIGNAL        Zexp        : bit_vector (7 downto 0);
  SIGNAL        Error,Cexp  : bit;
  SIGNAL        Zerr,Cerr   : bit;
  SIGNAL        TestVecNo   : integer;

  -- Enter your name in the (  )
  TYPE mname IS (Jiabei_Xu);
  SIGNAL nm : mname := mname'VAL(0);


  --  Enter the COMPONENT declaration and configuration for your
  --    8 bit ALU here 
  COMPONENT eightBitALU
    PORT (aIn, bIn:        IN BIT_VECTOR(7 DOWNTO 0);
          zOut:            OUT BIT_VECTOR(7 DOWNTO 0);
          aluOp:           IN operations;
          cIn:             IN BIT;
          cOut:            OUT BIT);
  END COMPONENT;
  FOR eightBitALU5: eightBitALU USE ENTITY WORK.eightBitALU(process_v2);


BEGIN  --  test 

  --  Enter the instantiation for your 8 bit ALU
  --  Note that the inputs are per the diagram in lecture (slide 8)
  --     except that only A and B are driven by the applytest process
  --     Also note that A and B are Bit vectors!!!
  --  Also note that the P, K, and R signals are gone.  You must now
  --     use the signal Oper to control the operation of the ALU.
  --  The carry input is Cin, the carry output Cout
  --  The data output of the alu slice is Zout which is also now a bit
  --     vector.
  eightBitALU5: eightBitALU PORT MAP (A, B, Zout, Oper, Cin, Cout);


  applytests : PROCESS

    PROCEDURE wait_n_check IS
      variable cor_res : bit_vector (7 downto 0);
      variable cor_cout : bit;
    BEGIN
      WAIT for 90 ns;
      IF (oper = op_AplusBwC and Cin = '1') THEN
        cor_res := AplBpl1_tbl(TestVecNo);
        cor_cout := carry_AplBpl1_tbl(TestVecNo);
      ELSIF (oper = op_AminBwC and Cin = '1') THEN
        cor_res := AmBm1_tbl(TestVecNo);
        cor_cout := carry_AmBm1_tbl(TestVecNo);
      ELSE
        cor_res := res_tbl(oper,TestVecNo);
        cor_cout := carry_res_tbl(oper,TestVecNo);
      END IF;
      IF (cor_res /= Zout OR cor_cout /= Cout) THEN
           Error <= '1', '0' after 10 ns;
      END IF;
      IF (cor_res /= Zout) THEN Zerr <= '1','0' AFTER 10 ns; END IF;
      IF (cor_cout /= Cout) THEN Cerr <= '1','0' AFTER 10 ns; END IF;
      Zexp <= cor_res; Cexp <= cor_cout;
      WAIT for 10 ns;
    END wait_n_check;

  BEGIN  --  PROCESS applytests 
    outter: FOR i IN 0 TO 21 LOOP
    Oper <= oper_tbl(i);

    IF (i >= 11 and i <= 16) THEN Cin <= '1';
                             ELSE Cin <= '0';
    END IF;
  
    A <= "00000000"; B <= "00000000"; TestVecNo <= 1;
    wait_n_check;
    A <= "00000000"; B <= "11111111"; TestVecNo <= 2;
    wait_n_check;
    A <= "11111111"; B <= "00000000"; TestVecNo <= 3;
    wait_n_check;
    A <= "11111111"; B <= "11111111"; TestVecNo <= 4;
    wait_n_check;
    A <= "01010101"; B <= "10101010"; TestVecNo <= 5;
    wait_n_check;
    A <= "11110000"; B <= "00001111"; TestVecNo <= 6;
    wait_n_check;
    A <= "10110010"; B <= "00101011"; TestVecNo <= 7;
    wait_n_check;

    IF (i = 18 or i = 20) THEN
      Cin <= '1';
      A <= "00000000"; B <= "00000000"; TestVecNo <= 1;
      wait_n_check;
      A <= "00000000"; B <= "11111111"; TestVecNo <= 2;
      wait_n_check;
      A <= "11111111"; B <= "00000000"; TestVecNo <= 3;
      wait_n_check;
      A <= "11111111"; B <= "11111111"; TestVecNo <= 4;
      wait_n_check;
      A <= "01010101"; B <= "10101010"; TestVecNo <= 5;
      wait_n_check;
      A <= "11110000"; B <= "00001111"; TestVecNo <= 6;
      wait_n_check;
      A <= "10110010"; B <= "00101011"; TestVecNo <= 7;
      wait_n_check;
    END IF;

    END LOOP outter;
    WAIT;
  END PROCESS applytests;

  nm<= mname'VAL(0);

END test;
