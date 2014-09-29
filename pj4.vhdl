-------------------------------------------------------------------------------
-- Project Step 4 - Behavioral Description of bit slice ALU
--
-- NAME: Jiabei Xu
--
-------------------------------------------------------------------------------
--YOU WILL USE YOUR 8 BIT ALU ENTITIY FROM STEP 3 
--     NO NEW ENTITY IS NEEDED
--This is an alternative architecture for that entity.
--Make sure your configuration is correct!!
-------------------------------------------------------------------------------
--  Enter your Behavioral Architecture for a 8 Bit ALU here
-------------------------------------------------------------------------------
ARCHITECTURE logic OF eightBitALU IS
BEGIN
  PROCESS (aIn, bIn, cIn, pIn, kIn, rIn)
  VARIABLE cInt: BIT_VECTOR(8 DOWNTO 0);
  VARIABLE pInt, kInt: BIT_VECTOR(7 DOWNTO 0);
  VARIABLE pkSel, rSel: BIT_VECTOR(1 DOWNTO 0); 
  BEGIN
    FOR i IN 0 TO 7 LOOP
      pkSel(1) := aIn(i);
      pkSel(0) := bIn(i);
      CASE pkSel IS
        WHEN "00" => pInt(i) := pIn(0);
                     kInt(i) := kIn(0);
        WHEN "01" => pInt(i) := pIn(1);
                     kInt(i) := kIn(1);
        WHEN "10" => pInt(i) := pIn(2);
                     kInt(i) := kIn(2);
        WHEN "11" => pInt(i) := pIn(3);
                     kInt(i) := kIn(3);
      END CASE;
      --cInt(i + 1) := (pInt(i) AND cInt(i)) OR (aIn(i) AND bIn(i));
      cInt(0) := cIn;
      cInt(i + 1) := (pInt(i) AND cInt(i)) OR ((NOT pInt(i)) AND NOT (kInt(i)));
      cOut <= cInt(8);
      rSel(1) := pInt(i);
      rSel(0) := cInt(i);
      CASE rSel IS
        WHEN "00" => zOut(i) <= rIn(0);
        WHEN "01" => zOut(i) <= rIn(1);
        WHEN "10" => zOut(i) <= rIn(2);
        WHEN "11" => zOut(i) <= rIn(3);
      END CASE;
    END LOOP;
  END PROCESS;
END logic;  

-------------------------------------------------------------------------------
--  The Test bench for testing of the 8 bit ALU 
-------------------------------------------------------------------------------
ENTITY p4 IS
END p4;

-------------------------------------------------------------------------------
--  The Test Bench Architecture
-------------------------------------------------------------------------------
ARCHITECTURE test OF p4 IS

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
  --  BE SURE TO SPECIFY THE ARCHITECTURE YOU JUST WROTE!!!
  COMPONENT eightBitALU
    PORT (aIn, bIn:        IN BIT_VECTOR(7 DOWNTO 0);
          zOut:            OUT BIT_VECTOR(7 DOWNTO 0);
          pIn, kIn, rIn:   IN BIT_VECTOR(3 DOWNTO 0);
          cIn:             IN BIT;
          cOut:            OUT BIT);
  END COMPONENT;
  FOR eightBitALU4: eightBitALU USE ENTITY WORK.eightBitALU(logic);

BEGIN  --  test 

  --  Enter the instantiation for your 8 bit ALU
  --  NOTE:
  --    A, B, and Zout are bit_vector (7 downto 0)
  --    The P, K, and R controls are in Pval, Kval, and
  --       Rval which are bit_vectors (3 downto 0)
  --    The carry input is Cin, the carry output Cout
  eightBitALU4: eightBitALU PORT MAP (A, B, Zout, Pval, Kval, Rval, Cin, Cout);

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

  nm <= mname'VAL(0);

END test;
