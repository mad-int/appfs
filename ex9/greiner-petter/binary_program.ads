with Interfaces; use Interfaces;

-- This package library discribes a binary program.
package Binary_Program is
   -- unsigned integer
   type Int_32 is new Unsigned_32;

   -- enumeration of Constraint types
   type Constraint_Type is
     (GEQ,     -- greater or equal >=
      LEQ,     -- less or equal    <=
      EQ,      -- equal            ==
      LESS,    -- less             <
      GREATER, -- greater          >
      UNKNOWN  -- undefined value
     );

   -- define own integer from 1 to max 32
   type Legal_Int is new Integer range 0 .. 32;

   -- constraints and matrix arrays
   type Const_Type_Arr is array (Legal_Int range <>) of Constraint_Type with pack;
   type Floating_Arr   is array (Legal_Int range <>) of Float with pack;
   type Matrix_Arr_Arr is array (Legal_Int range <>, Legal_Int range <>) of Float with pack;

   -- returns the
   function Get_Constraint_Type ( Var : String ) return Constraint_Type;

   --
   procedure Solve ( matrix      : in Matrix_Arr_Arr;
		     const_types : in Const_Type_Arr;
		     sol_vec     : in Floating_Arr
		    );

private

   function is_Feasable ( matrix : Matrix_Arr_Arr;
			  const  : Const_Type_Arr;
			  sols   : Floating_Arr
			 ) return Boolean;

   function Get_String_From_Value ( value : Int_32; length : Positive ) return String;

end Binary_Program;
