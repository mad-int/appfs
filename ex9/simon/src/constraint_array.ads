with numb; use numb;
with Ada.Unchecked_Deallocation; -- for definition of free

package Constraint_Array is

   -- should feasible binary solutions be printed?
   printSolutions : constant Boolean := False;

   -- types
   type Coeff_Vector is array (Positive range <>) of Number;

   type Constr_Type is (LESS_EQUAL , EQUAL , GREATER_EQUAL);

   type Constraint (dimension : Positive) is record
      coeffs : Coeff_Vector (1 .. Dimension);
      rhs    : Number;
      cType  : Constr_Type;
   end record;

   type Constraint_Ptr is access Constraint;

   type Constraint_Array is array (Positive range <>) of Constraint_Ptr;

   type Constr_Array (dimension, nConstrs : Positive) is record
      cA : Constraint_Array (1 .. nConstrs);
   end record;

   type Constr_Array_Ptr is access Constr_Array;

   type Lhs_Array is array (Positive range <>) of Number;

   type Constr_Status is (NORMAL, INFEASIBLE, REDUNDANT);

   -- error caused by overflow of the number type
   Overflow_Error : exception;

   -- Free's

   procedure Free is new Ada.Unchecked_Deallocation (Object => Constraint, Name => Constraint_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (Object => Constr_Array, Name => Constr_Array_Ptr);

   -- procedures

   function Create_Constr_Array (dimension, nConstrs : Positive) return Constr_Array_Ptr;

   -- adding constraint after checking for redundancy and infeasibility
   function Add_Constraint_To_Array (c : Constraint_Ptr; cap : Constr_Array_Ptr; index : Positive) return Constr_Status
     with Pre => c.Dimension = cap.Dimension;

   -- resize cap, such that all null-constraints will be deleted
   procedure Resize_Constr_Array (cap : in out Constr_Array_Ptr; newNConstraints : in out Integer)
     with  Pre => 0 < newNConstraints,
     Post => 0 = newNConstraints; -- newNConstraints will be decreased by 1 for each not-null constraint, so it should be zero in the end

   -- ensures that all not null constraints in the array are also free'd
   procedure Free_Constr_Array (cap : in out Constr_Array_Ptr);

   -- print to stdout that the model in unconstrained
   procedure Print_Unconstrained_Solution (dimension : Positive);

   -- print to stdout that the model has no feasible assignment
   procedure Print_No_Feasible_Assignment;

   -- test each possible binary variable assignment for being feasible for the constraint array
   procedure Enumerate (cap : Constr_Array_Ptr);

   -- build constraint array for testing
   function Build_Dummy_Constr_Array  return Constr_Array_Ptr;

   -- print the given constraint array to stdout
   procedure Print_Constr_Array (cap : Constr_Array_Ptr);

private
   -- length of string to print for numbers
   numberStrLength : Positive := 3;

end Constraint_Array;