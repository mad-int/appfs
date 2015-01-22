package body Linear_Constraints is
   function Create_Linear_Constraint(ineqType : in ConstraintType; coefficients : in Vector_ptr; rhs : in Integer) return Constraint_ptr
   is
      cons : Constraint_ptr;
   begin
      cons := new Constraint;
      cons.ineqType := ineqType;
      cons.coefficients := coefficients;
      cons.rhs := rhs;
      return cons;
   end Create_Linear_Constraint;
end Linear_Constraints;