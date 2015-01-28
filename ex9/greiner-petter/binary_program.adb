with Ada.Text_IO; use Ada.Text_IO;
with Binary_Program; use Binary_Program;

-- define package body of binary program
package body Binary_Program is
   -- like a static function, this function returns a constraint type
   -- by given string.
   function Get_Constraint_Type ( Var : String )
				 return Constraint_Type
   is
   begin
      -- use if - elseif - else to find right constraint type
      if    Var =" <= " then
	 return LEQ;
      elsif Var =" >= " then
	 return GEQ;
      elsif Var = " = " then
	 return EQ;
      elsif Var = " < " then
	 return LESS;
      elsif Var = " > " then
	 return GREATER;
      else return UNKNOWN; -- if the given type is unknown
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
      max    : Int_32;       -- maximum integer value
      binary : Bool_Arr_Ptr; -- binary of integer
   begin
      -- get maximum value
      if matrix'Length (2) = 32 then
	 max := Int_32'Last;
      else
	 max := Int_32'Val( (2**(matrix'Length(2) + 1) ) - 1 );
      end if;

      -- inform user
      Put_Line ("Searching until: " & Int_32'Image (max));

      -- test for each value
      for value in 0 .. max loop
	 -- get binary array of value
	 binary := get_Binary_Array (value, matrix'Length (2));
	 -- if feasible, print the solution
	 if is_Feasable ( matrix, const_types, sol_vec, binary ) then
	    print_Binary_String ( binary );
	 end if;
      end loop;
   end Solve;

   -- Return 'a <const> b', with <const> could be
   -- <=, >=, =, <, >
   function is_Valid
     ( const : Constraint_Type;
       a     : Float;
       b     : Float
      ) return Boolean
   is
   begin
      case const is
      when GEQ     => return a >= b;
      when LEQ     => return a <= b;
      when EQ      => return a  = b;
      when LESS    => return a <  b;
      when GREATER => return a >  b;
      when UNKNOWN =>
	 Put_Line ("Unknown constraint type");
	 return False;
      end case;
   end is_Valid;

   -- Return true if the given binary is a feasible solution of given BIP, false otherwise
   function is_Feasable
     ( matrix : Matrix_Arr_Arr;
       const  : Const_Type_Arr;
       sols   : Floating_Arr;
       binary : Bool_Arr_Ptr
      ) return Boolean
   is
      sum : Float; -- sum of constraint
   begin
      -- for each constraint
      for I in matrix'Range (1) loop
	 -- calculate the sum for variables
	 sum := 0.0;
	 for J in matrix'Range(2) loop
	    if binary ( Legal_Int'Val (J) ) then
	       sum := sum + matrix (I, J);
	    end if;
	 end loop;
	 -- if this sum violates the constraint type, return false
	 if not is_Valid (const (I), sum, sols (I)) then
	    return false;
	 end if;
      end loop;
      -- otherwise this binary is a feasible solution
      return True;
   end is_Feasable;

   -- Calculates a binary array of given value with given width
   function get_Binary_Array
     ( value : Int_32;
       width : Legal_Int
      ) return Bool_Arr_Ptr
   is
      -- create a new binary array
      arr_ptr : constant Bool_Arr_Ptr := new Bool_Arr ( 1 .. width );
      tmp     : Int_32 := value; -- dont change input value
   begin
      for I in arr_ptr'Range loop
	 -- remainder, works like: A = (A / B) * B + (A rem B)
	 if not tmp rem 2 = 0 then
	    arr_ptr (I) := True; -- set the bit
	 end if;
	 tmp := tmp / 2;
      end loop;
      return arr_ptr;
   end get_Binary_Array;

   -- Print the given array to console
   procedure print_Binary_String ( pointer : Bool_Arr_Ptr ) is
      type str_ptr is access String;
      output : str_ptr := new String ( 1 .. pointer'Length );
   begin
      -- create a new string object
      for I in output'Range loop
	 if pointer (Legal_Int'Val(I)) then
	    output (I) := '1';
	 else output (I) := '0';
	 end if;
      end loop;
      -- print the new object
      Put_Line( output ( output'Range ) );
   end print_Binary_String;
end Binary_Program;
