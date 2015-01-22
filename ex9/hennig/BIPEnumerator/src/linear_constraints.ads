with Vector;
use Vector;

package Linear_Constraints is

   type ConstraintType is (LEQ,GEQ,EQ);

   type Constraint is
      record
         ineqType : ConstraintType;
         coefficients : Vector_ptr;
         rhs : Integer;
      end record;

   type Constraint_ptr is access Constraint;

   function Create_Linear_Constraint(ineqType : in ConstraintType; coefficients : in Vector_ptr; rhs : in Integer) return Constraint_ptr;

end Linear_Constraints;