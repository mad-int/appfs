with Vector;
use Vector;
with Linear_Constraints;
use Linear_Constraints;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Binary_Integer_Program is

   WRONG_CONSTRAINT_DIMENSION_EXCEPTION : exception;
   WRONG_MATRIX_DIMENSION_EXCEPTION : exception;
   UNKNOWN_CONSTRAINT_TYPE : exception;
   WRONG_NUMBER_OF_ROWS_EXCEPTION : exception;
   PARSING_EXCEPTION : exception;
   OVER_UNDERFLOW_EXCEPTION : exception;

   type BipConstraintArray is array (Integer range <>) of Constraint_ptr;
   type BipConstraintArray_ptr is access BipConstraintArray;

   type BIP is tagged record
      numOfRows : Integer;
      numOfRowsAdded : Integer;
      numOfColumns : Integer;
      bca : BipConstraintArray_ptr;
   end record;
   type BIP_ptr is access BIP;

   -- Reads a BIP from a file --
   procedure Read_BIP(bipInstance_ptr : out BIP_ptr; fileName : in String);

   -- Prints the BIP --
   procedure Print_BIP(bipInstance : in BIP_ptr);

   -- Add a constraint to the BIP --
   procedure Add_Constraint_to_BIP(bipInstance : in BIP_ptr ; bipCon : in Constraint_ptr);

   -- Method to enumerate all feasible BIP solutions --
   function Determine_Number_of_Feasible_Solutions_for_BIP(bipInstance : in BIP_ptr) return Long_Integer;

   -- Method to check feasibility of a binary vector --
   function Check_Feasibility_Of_Solution(bipInstance : in BIP_ptr; solVec : in BinaryVector_ptr) return Boolean;


private
   -- Creates a linear constraint from a line --
   function Create_Linear_Constraint(line : in Unbounded_String; numOfCols : in Integer) return Constraint_ptr;

   -- Create an empty instance of a BIP --
   procedure Create_Empty_BIP(bipInstance : out BIP_ptr ; numOfRows : in Positive ; numOfColumns : in Positive);

   -- Check the program for consistency --
   function Read_Matrix_Dimension(line : in Unbounded_String) return Positive;

   -- Check the program for consistency --
   procedure Check_Consistency(bipInstance : in BIP_ptr);

   -- Method to recursively create and evaluate all binary solutions --
   procedure Enumerate_Solutions_of_BIP_Recursively(bipInstance : in BIP_ptr; solVec : in out BinaryVector_ptr ; bitChangeIndex : in Integer; numberOfSolutions : in out Long_Integer);

   -- Function checking feasibility of a solution for a single constraint --
   function Check_Feasibility_For_Single_Constraint(cons : in Constraint_ptr ; solVec : in BinaryVector_ptr) return Boolean;

end Binary_Integer_Program;

