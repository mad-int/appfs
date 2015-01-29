with Interfaces; use Interfaces;

-- This package library discribes a binary program.
package Binary_Program is
   -- unsigned integer
   type Int_32 is new Unsigned_32;

   -- define own integer from 1 to max 32
   type Legal_Int is new Integer range 0 .. 32;

   -- enumeration of Constraint types
   type Constraint_Type is
     (GEQ,     -- greater or equal >=
      LEQ,     -- less or equal    <=
      EQ,      -- equal            ==
      LESS,    -- less             <
      GREATER, -- greater          >
      UNKNOWN  -- undefined value
     );

   -- constraints and matrix arrays
   type Const_Type_Arr is array (Legal_Int range <>) of Constraint_Type with pack;
   type Floating_Arr   is array (Legal_Int range <>) of Float with pack;
   type Matrix_Arr_Arr is array (Legal_Int range <>, Legal_Int range <>) of Float with pack;

   -- returns the constraint type of a string
   function Get_Constraint_Type ( Var : in String ) return Constraint_Type;

   -- solve a BIP and print all solutions to console
   --         A     * x        =             b
   --    < matrix > * x < const_types > < sol_vec >
   procedure Solve ( matrix      : in Matrix_Arr_Arr;
		     const_types : in Const_Type_Arr;
		     sol_vec     : in Floating_Arr
		    );

-- add some private methods
private
   -- Use a boolean array for binary integers
   type Bool_Arr is array (Legal_Int range <>) of Boolean with pack;
   type Bool_Arr_Ptr is access Bool_Arr;

   -- Returns whether 'a <const> b' is true or not.
   -- For instance const type could be '<'. So returns
   --     a < b
   function is_Valid (
			  const : in Constraint_Type;
			  a     : in Float;
			  b     : in Float
		     ) return Boolean;

   -- Returns whether a given solution (boolean array) is a
   -- feasible solution of the whole BIP
   function is_Feasable ( matrix : in Matrix_Arr_Arr;
			  const  : in Const_Type_Arr;
			  sols   : in Floating_Arr;
			  binary : in Bool_Arr_Ptr
			 ) return Boolean;

   -- Returns the binary array of value with given width.
   -- For instance:
   --     get_Binary_Array( 1, 4 ) returns
   --     [0,0,0,1]
   function get_Binary_Array (
			       value : in Int_32;
			       width : in Legal_Int
			     ) return Bool_Arr_Ptr;

   -- Print a boolean array to console
   procedure print_Binary_String ( pointer : in Bool_Arr_Ptr );
end Binary_Program;
