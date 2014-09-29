-------------------------------------------------------------------------------
--  EE762 - Project Assignment #1
--
--  NAME: Jiabei XU
--
--=============================================================================
--  Generic Function Unit Entity
-------------------------------------------------------------------------------
ENTITY generic_function IS
  PORT (A,B,G3,G2,G1,G0 : IN BIT;
        R  : OUT  BIT);
END generic_function;

-------------------------------------------------------------------------------
--  Enter your architecture for the generic function unit here
-------------------------------------------------------------------------------
ARCHITECTURE behavioral of generic_function IS

  SIGNAL G: BIT_VECTOR(3 DOWNTO 0);

  BEGIN

  G(3) <= G3;
  G(2) <= G2;
  G(1) <= G1;
  G(0) <= G0;

  WITH G SELECT
  R <= '0'           WHEN "0000",
       A NOR B       WHEN "0001",
       NOT A AND B   WHEN "0010",
       NOT A         WHEN "0011",
       A AND NOT B   WHEN "0100",
       NOT B         WHEN "0101",
       A XOR B       WHEN "0110",
       A NAND B      WHEN "0111",
       A AND B       WHEN "1000",
       A XNOR B      WHEN "1001",
       B             WHEN "1010",
       NOT A OR B    WHEN "1011",
       A             WHEN "1100",
       A OR NOT B    WHEN "1101",
       A OR B        WHEN "1110",
       '1'           WHEN "1111";

  END behavioral;

-------------------------------------------------------------------------------
--  The Test Bench Entity for testing of the generic function block
-------------------------------------------------------------------------------
ENTITY p1tb IS
END p1tb;

-------------------------------------------------------------------------------
--  The Test Bench Architecture - (pr_step1_testbench)
-------------------------------------------------------------------------------
ARCHITECTURE test OF p1tb IS

  TYPE operations IS (opZERO,opNOR,opAbarB,opNOTA,opABbar,opNOTB,opXOR,opNAND,
                    opAND,opXNOR,opB,opABARorB,opA,opAorBbar,opOR,opONE);
  TYPE oper_type  IS ARRAY (0 to 15) of operations;
  CONSTANT      oper_tbl     : oper_type := (opZERO,opNOR,opAbarB,opNOTA,
		   opABbar,opNOTB,opXOR,opNAND,opAND,opXNOR,opB,opABARorB,opA,
		   opAorBbar,opOR,opONE);
  SIGNAL        oper : operations;

  TYPE  gval_tbl_type IS ARRAY (0 to 15) of bit_vector (3 downto 0);
  CONSTANT gval_tbl : gval_tbl_type :=
              ("0000","0001","0010","0011","0100","0101","0110","0111",
               "1000","1001","1010","1011","1100","1101","1110","1111");
  SIGNAL        gval    : bit_vector (3 downto 0);

  SIGNAL        A,B,R            : bit;

  --  component declaration and configuration **** you must enter
  --        the architecture name in the FOR ALL statment for ARCH_NAME

  COMPONENT generic_function
    PORT (A,B,G3,G2,G1,G0 : IN BIT;
          R  : OUT  BIT);
  END COMPONENT;
  FOR ALL : generic_function USE ENTITY WORK.generic_function(behavioral); 

BEGIN  --  test 

  g0 : generic_function PORT MAP(A,B,gval(3),gval(2),gval(1),gval(0),R);

  applytests : PROCESS
  BEGIN  --  PROCESS applytests 
    outter: FOR i IN 0 TO 15 LOOP
       oper <= oper_tbl(i);
	   gval <= gval_tbl(i);
	   A <= '0'; B <= '0';
	   WAIT FOR 25 ns;
	   A <= '0'; B <= '1';
	   WAIT FOR 25 ns;
	   A <= '1'; B <= '0';
	   WAIT FOR 25 ns;
	   A <= '1'; B <= '1';
	   WAIT FOR 25 ns;
	END LOOP outter;
	WAIT;
  END PROCESS applytests;

END test;


