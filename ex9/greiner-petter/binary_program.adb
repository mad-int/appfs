with Ada.Text_IO; use Ada.Text_IO;
-- use lib
with Binary_Program; use Binary_Program;

-- define package body of binary program
package body Binary_Program is
   -- like a static function, this function returns a constraint type
   -- by given string.
   function Get_Constraint_Type ( Var : String )
				 return Constraint_Type
   is
   begin
      if Var = "<=" then
	 return LEQ;
      elsif Var = ">=" then
	 return GEQ;
      elsif Var = "=" then
	 return EQ;
      elsif Var = "<" then
	 return LESS;
      elsif Var = ">" then
	 return GREATER;
      else return UNKNOWN;
      end if;
   end Get_Constraint_Type;

   -- Solve procedure
   -- This procedure solves a given binary program defined by
   --          A     x       =          b.
   --       matrix * x <const_types> sol_vec
   --
   -- A solution can represented by an 32-bit Integer. To iterate
   -- through each possible solution just increase this integer.
   -- If this bit pattern feasable print the solution to the console.
   procedure Solve
     ( matrix      : in Matrix_Arr_Arr;
       const_types : in Const_Type_Arr;
       sol_vec     : in Floating_Arr
      )
   is
   begin
      for value in 0 .. Int_32'Value("5") loop

	 if is_Feasable ( matrix, const_types, sol_vec ) then
	    Put_Line ( "Solution: " & Get_String_From_Value(value, 4) );
	 end if;


      end loop;
   end Solve;

   function is_Feasable
     ( matrix : Matrix_Arr_Arr;
       const  : Const_Type_Arr;
       sols   : Floating_Arr
      ) return Boolean
   is
   begin
      return TRUE;
   end is_Feasable;

   function Get_String_From_Value ( value : Int_32; length : Positive ) return String
   is
      pow : Int_32 := Int_32'Last;
      str : String ( 1 .. length ) := "";
   begin
      Put_Line ( "RAW: " & str );
      for I in str'Range loop
	 str ( I ) := '0';
	 if (pow and value) > 0 then
	    str ( I ) := '1';
	 end if;
	 pow := Shift_Left( pow, 1 );
      end loop;
      return str;
   end Get_String_From_Value;

end Binary_Program;
