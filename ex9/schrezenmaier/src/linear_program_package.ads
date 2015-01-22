-- linear_program_package.ads

with Gray_Code_Package; use Gray_Code_Package;

package Linear_Program_Package is
   
   type Linear_Program (<>) is tagged private;

   function New_LP (Vars, Constrs : Natural) return Linear_Program;

   procedure Print (Self : Linear_Program);

   function Read_LP_From_File (File_Name : String) return Linear_Program;
   
   function Count_Feasible_Binary (Self : Linear_Program) return Int_64;

private

   type Vector is array (Natural range <>) of Float;

   type Matrix is array (Natural range <>, Integer range <>) of Float;
   
   type Linear_Program (Var, Constr : Positive) is tagged record
      Vars            : Positive                   := Var;
      Constrs         : Positive                   := Constr;
      A               : Matrix (1..Constr, 1..Var) := (others => (others => 0.0));
      Rhs             : Vector (1..Constr)         := (others => 0.0);
      Last_Inequality : Natural                    := 0;
      First_Equality  : Natural                    := Constr + 1;
   end record;

   type Comp_Sign is (GEQ, LEQ, EQ);

end Linear_Program_Package;
