------------------------------------------------------------------------------
-- Project Step 5 - Use of Procedures Declared in Process
--
-- NAME: Jiabei Xu
--
-------------------------------------------------------------------------------
-- YOU WILL USE YOUR 8 BIT ALU ENTITIY FROM STEP 3
-------------------------------------------------------------------------------
-- Enter your Behavioral Architecture for a 8 Bit ALU here
-- Remember that it will have only one process and the procedures
--   are to be declared within the process.
-------------------------------------------------------------------------------
ARCHITECTURE process_new OF eightBitALU IS
BEGIN
  PROCESS (aIn, bIn, cIn, pIn, kIn, rIn)
    PROCEDURE BINADD (a, b: IN BIT_VECTOR;
                      c_in: IN BIT;
                      z_out: OUT BIT_VECTOR;
                      c_out: OUT BIT) IS
    VARIABLE carry: BIT;
    BEGIN
    carry := c_in;
    FOR i IN a'REVERSE_RANGE LOOP
      z_out(i) := a(i) XOR b(i) XOR carry;
      --carry := (b(i) AND (a(i) XOR carry)) OR (a(i) AND carry);
      carry := (a(i) AND b(i)) OR (a(i) AND carry) OR (b(i) AND carry);
    END LOOP;
    c_out := carry; 
    END BINADD;
    
    PROCEDURE BINSUB (a, b: IN BIT_VECTOR;
                      b_in: IN BIT;
                      z_out: OUT BIT_VECTOR;
                      b_out: OUT BIT) IS
    VARIABLE borrow: BIT;

    BEGIN
    borrow := b_in;
    FOR i IN a'REVERSE_RANGE LOOP
      z_out(i) := a(i) XOR b(i) XOR borrow;
      --borrow := ((NOT a(i)) AND borrow) AND b(i);
      borrow := (NOT a(i) AND borrow) OR (b(i) AND borrow) OR (NOT a(i) AND b(i));
    END LOOP;
    b_out := borrow;
    END BINSUB;

    PROCEDURE COMP_2 (num: IN BIT_VECTOR;
                      z_out: OUT BIT_VECTOR;
                      c_out: OUT BIT) IS
    VARIABLE notNum, Zero: BIT_VECTOR (num'RANGE);
    BEGIN
      notNum := NOT num;
      BINADD(notNum, ZERO, '1', z_out, c_out);
    END COMP_2;
    
    VARIABLE zOutTemp: BIT_VECTOR(7 DOWNTO 0);
    VARIABLE cOutTemp: BIT;
    VARIABLE opcode: BIT_VECTOR(11 DOWNTO 0);

  BEGIN
  opcode := pIn&kIn&rIn;

  CASE opcode IS
    WHEN "110011111100" => zOut <= aIn; cOut <= '0'; --opA
    WHEN "101011111100" => zOut <= bIn; cOut <= '0'; --opB
    WHEN "001111111100" => zOut <= NOT aIn; cOut <= '0'; --opNotA
    WHEN "010111111100" => zOut <= NOT bIn; cOut <= '0'; --opNotB
    WHEN "011011111100" => zOut <= aIn XOR bIn; cOut <= '0'; --opAxorB
    WHEN "111011111100" => zOut <= aIn OR bIn; cOut <= '0'; --opAorB
    WHEN "100011111100" => zOut <= aIn AND bIn; cOut <= '0'; --opAandB
    WHEN "011111111100" => zOut <= aIn NAND bIn; cOut <= '0'; --opAnandB
    WHEN "100111111100" => zOut <= aIn XNOR bIn; cOut <= '0'; --opAxnorB
    WHEN "000011111100" => zOut <= "00000000"; cOut <= '0'; --op0
    WHEN "111111111100" => zOut <= "11111111"; cOut <= '0'; --op1
    WHEN "110000110110" => BINADD(aIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opIncA
    WHEN "101001010110" => BINADD(bIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opIncB
    WHEN "001111001001" => BINSUB(aIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opDecA
    WHEN "010110101001" => BINSUB(bIn, "00000000", '1', zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opDecB
    WHEN "001111000110" => COMP_2(aIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opNegA
    WHEN "010110100110" => COMP_2(bIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opNegB
    WHEN "011000010110" => BINADD(aIn, bIn, cIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opA+BwC
    WHEN "100101001001" => BINSUB(aIn, bIn, cIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opA-BwC
    WHEN "100100101001" => BINSUB(bIn, aIn, cIn, zOutTemp, cOutTemp); zOut <= zOutTemp; cOut <= cOutTemp; --opB-A
    WHEN OTHERS => NULL;
  END CASE;
END PROCESS;
END process_new;

-------------------------------------------------------------------------------
--  The Test bench for testing of the 8 bit ALU 
-------------------------------------------------------------------------------
ENTITY p5 IS
END p5;

-------------------------------------------------------------------------------
--  The Test Bench Architecture
-------------------------------------------------------------------------------
ARCHITECTURE test OF p5 IS

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
                 

  SIGNAL        Pval,Kval,Rval      : bit_vector (3 downto 0);
  SIGNAL        A,B,Zout    : bit_vector (7 downto 0);
  SIGNAL        Cin,Cout    : bit;
  SIGNAL        Zexp        : bit_vector (7 downto 0);
  SIGNAL        Error,Cexp  : bit;
  SIGNAL        TestVecNo   : integer;

  -- Enter your name in the (  )
  TYPE mname IS (Jiabei_Xu);
  SIGNAL nm : mname := mname'VAL(0);

  --  Enter the COMPONENT declaration and configuration for your
  --    8 bit ALU here 
  COMPONENT eightBitALU
  PORT (aIn, bIn:        IN BIT_VECTOR(7 DOWNTO 0);
        zOut:            OUT BIT_VECTOR(7 DOWNTO 0);
        pIn, kIn, rIn:   IN BIT_VECTOR(3 DOWNTO 0);
        cIn:             IN BIT;
        cOut:            OUT BIT);
  END COMPONENT;
  FOR eightBitALU5: eightBitALU USE ENTITY WORK.eightBitALU(process_new);

BEGIN  --  test 

  --  Enter the instantiation for your 8 bit ALU
  --  NOTE:
  --    A, B, and Zout are bit_vector (7 downto 0)
  --    The P, K, and R controls are in Pval, Kval, and
  --       Rval which are bit_vectors (3 downto 0)
  --    The carry input is Cin, the carry output Cout
  eightBitALU5: eightBitALU PORT MAP (A, B, Zout, Pval, Kval, Rval, Cin, Cout);


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
      IF (cor_res /= Zout or cor_cout /= Cout) THEN
           Error <= '1', '0' after 10 ns;
      END IF;
      Zexp <= cor_res; Cexp <= cor_cout;
      WAIT for 10 ns;
    END wait_n_check;

  BEGIN  --  PROCESS applytests 
    outter: FOR i IN 0 TO 21 LOOP
    oper <= oper_tbl(i);
    Pval <= pval_tbl(i);
    Kval <= kval_tbl(i);
    Rval <= rval_tbl(i);

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

  nm<=mname'VAL(0);
END test;
