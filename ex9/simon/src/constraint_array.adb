with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces; -- for Unsigned_32
with Ada.Calendar; use Ada.Calendar; -- for timing
with Ada.Integer_Text_IO;
package body Constraint_Array is

   function Create_Constr_Array (dimension, nConstrs : Positive) return Constr_Array_Ptr is
      cA : Constraint_Array (1 .. nConstrs);
   begin
      return new Constr_Array'(dimension => dimension, nConstrs => nConstrs, cA => cA);
   end Create_Constr_Array;


   -- calculates maxActivity and checks for maximum overflow
   function Calculate_Max_Activity_For_Constraint (c : Constraint_Ptr) return Number is
      subtype constrained_Number is Number range Number'Range; -- boundary checks for Number, used to check floating point overflow
      sum : constrained_Number := number_Zero;
   begin
      for i in c.coeffs'Range loop
         declare
            coeff : Number := c.coeffs (i);
         begin
            if Number_Greater_Than_Number (coeff, number_Zero) then
               sum := sum + coeff;
            end if;
         end;
      end loop;
      return sum;
   end Calculate_Max_Activity_For_Constraint;

   -- calculates minActivity and check for minimum overflow
   function Calculate_Min_Activity_For_Constraint (c : Constraint_Ptr) return Number is
      subtype constrained_Number is Number range Number'Range; -- boundary checks for Number, used to check floating point overflow
      sum : constrained_Number := number_Zero;
   begin
      for i in c.coeffs'Range loop
         declare
            coeff : Number := c.coeffs (i);
         begin
            if Number_Less_Than_Number (coeff, number_Zero) then
               sum := sum + coeff;
            end if;
         end;
      end loop;
      return sum;
   end Calculate_Min_Activity_For_Constraint;

   function Add_Constraint_To_Array (c : Constraint_Ptr; cap : Constr_Array_Ptr; index : Positive) return Constr_Status is
      minAct, maxAct : Number;
      rhs            : Number := c.rhs;
   begin

      begin
         minAct := Calculate_Min_Activity_For_Constraint (c);
      exception
         when Constraint_Error =>
            raise Overflow_Error with "min overflow";
      end;
      begin
         maxAct := Calculate_Max_Activity_For_Constraint (c);
      exception
         when Constraint_Error =>
            raise Overflow_Error with "max overflow";
      end;

      case c.cType is

         when LESS_EQUAL =>
            if not Number_Greater_Than_Number (maxAct, rhs) then
               return REDUNDANT;
            end if;
            if Number_Greater_Than_Number (minAct , rhs) then
               return INFEASIBLE;
            end if;

         when EQUAL =>
            if Number_Equals_Number (minAct, maxAct) and then Number_Equals_Number (minAct, rhs) then
               return REDUNDANT;
            end if;
            if Number_Less_Than_Number (maxAct, rhs) or else Number_Greater_Than_Number (minAct, rhs) then
               return INFEASIBLE;
            end if;

         when GREATER_EQUAL =>
            if not Number_Less_Than_Number (minAct, rhs) then
               return REDUNDANT;
            end if;
            if Number_Less_Than_Number (maxAct , rhs) then
               return INFEASIBLE;
            end if;
      end case;

      -- add constraint only if it is not infeasible nor redundant
      cap.cA (index) := c;
      return NORMAL;
   end Add_Constraint_To_Array;

   procedure Resize_Constr_Array (cap : in out Constr_Array_Ptr; newNConstraints : in out Integer) is
      newCap          : Constr_Array_Ptr := create_Constr_Array (Dimension => cap.Dimension, nConstrs => newNConstraints);
      newCapIndex     : Positive := 1;
   begin

      for i in cap.cA'Range loop
         if not (null = cap.cA (i)) then
            newNConstraints := newNConstraints - 1 ;
            newCap.cA (newCapIndex) := cap.cA (i);
            newCapIndex := 1 + newCapIndex;
         end if;
      end loop;
      -- only free the pointer, do not use Free_Constr_Array, since it also frees the constraints!
      Free (cap);
      cap := newCap;
   end Resize_Constr_Array;

   procedure Free_Constr_Array (cap : in out Constr_Array_Ptr) is
   begin
      for i in cap.cA'Range loop
         if not (null = cap.cA (i)) then
            Free (cap.cA (i));
         end if;
      end loop;
      Free (cap);
   end Free_Constr_Array;

   -- convert binary uncoded as Unsigned_32 into a 0/1-string which has to be red from left to right
   function Parse_Binary_To_String (binary : Unsigned_32; dimension : Integer) return String is
      output : String (1 .. dimension);
      mask   : Unsigned_32 := 1;
   begin
      for i in 1 .. dimension loop
         if (binary and mask) = 0 then
            output (i) := '0';
         else
            output (i) := '1';
         end if;
         mask := Shift_Left (mask, 1);
      end loop;
      return outPut;
   end Parse_Binary_To_String;

   -- standard text for printing a feasible binary
   procedure Print_Feasible_Binary (binary : Unsigned_32; dimension : Positive )is
   begin
      Put_line ("Binary assignment " & Parse_Binary_To_String (binary => binary, dimension => dimension) & " is valid!");
   end Print_Feasible_Binary;

   -- standard text for printing the number of feasible solutions
   procedure Print_Number_Of_Feasible_Binaries (nFeasSol : Unsigned_64) is
   begin
      Put_Line (Unsigned_64'Image (nFeasSol) & " feasible solutions found!");
   end Print_Number_Of_Feasible_Binaries;

   procedure Print_Unconstrained_Solution (dimension : Positive) is
      nFeasSol : Unsigned_64 := (Shift_Left (1, dimension));
   begin
      Put_line ("All binary solutions are feasible!");

      if printSolutions then
         for count in 0 .. nFeasSol - 1 loop
            -- count is always an Unsigned_32
            Print_Feasible_Binary (Unsigned_32 (count), dimension);
         end loop;

         Print_Number_Of_Feasible_Binaries (nFeasSol);
      end if;

   end Print_Unconstrained_Solution;

   procedure Print_No_Feasible_Assignment is
   begin
      Put_line ("No feasible solution found!");
   end Print_No_Feasible_Assignment;

   -- check for the given lhs values if the constraints are fulfilled
   function Check_All_Constraints (cap : Constr_Array_Ptr; lhss : Lhs_Array) return Boolean is
   begin
      for i in 1 .. cap.nConstrs loop
         declare
            c   : Constraint_Ptr := cap.cA (i);
            lhs : Number := lhss (i);
            rhs : Number := c.rhs;
         begin
            case c.cType is
               when LESS_EQUAL =>
                  if Number_Greater_Than_Number (x => lhs, y => rhs) then
                     return False;
                  end if;
               when EQUAL =>
                  if not Number_Equals_Number (x => lhs, y => rhs) then
                     return False;
                  end if;
               when GREATER_EQUAL =>
                  if Number_Less_Than_Number (x => lhs, y => rhs) then
                     return False;
                  end if;
            end case;
         end;
      end loop;

      return True;
   end Check_All_Constraints;

   -- generate new testBinary using gray codes and update the lhs vector analogously
   procedure Update (cap : in Constr_Array_Ptr; count : in Unsigned_32; lhss : in out Lhs_Array; testBinary : in out Unsigned_32) is
      mask : Unsigned_32 := 1;
      i    : Natural range 1 .. cap.dimension := 1;
      add  : Boolean;
   begin
      -- update only if another iteration is following, otherwise the mask may overflow if dimension = 32
      if (Shift_Right (Unsigned_32'Last, 32 - cap.dimension)) /= (count) then

         -- look for dimension 'i' of coefficient to change in the test binary. it is equal to the dimension of the first zero coefficient in the binary representation of count
         while ((mask and count) /= 0) loop
            mask := Shift_Left (mask, 1);
            i := i + 1;
         end loop;

         -- update lhss
         add := (0 = (mask and testBinary));

         for j in 1 .. cap.nConstrs loop
            if add then
               lhss (j) := lhss (j) + cap.cA (j).coeffs (i);
            else
               lhss (j) := lhss (j) - cap.cA (j).coeffs (i);
            end if;
         end loop;

         -- update testBinary
         testBinary := testBinary xor mask;
      end if;
   end Update;

   procedure Enumerate (cap : Constr_Array_Ptr) is
      testBinary  : Unsigned_32 := 0; -- the binary assignment to check for feasibility
      nFeasSols   : Unsigned_64 := 0;
      lhss        : Lhs_Array (1 .. cap.nConstrs) := (others => number_zero); -- array of the lhs's initialized to zero
      Start       : Time := Clock;
      Finish      : Time;
   begin
      for count in 0 .. (Shift_Right (Unsigned_32'Last, 32 - cap.dimension)) loop
         if (Check_All_Constraints (cap, lhss)) then
            nFeasSols := nFeasSols + 1;
            if printSolutions then
               Print_Feasible_Binary (binary => testBinary, dimension => cap.dimension);
            end if;
         end if;
         update (cap, count, lhss, testBinary);
      end loop;
      Finish := Clock;

      if nFeasSols = 0 then
         Print_No_Feasible_Assignment;
      else
         Print_Number_Of_Feasible_Binaries (nFeasSol => nFeasSols);
      end if;

      Put_Line ("Time for enumerating all binaries, checking for feasibility and printing the feasible ones is: " & Duration'Image (Finish - Start) & " seconds");
   end Enumerate;

   function Build_Dummy_Constr_Array return Constr_Array_Ptr is

      dimension            : Integer := 5;
      nConstraints         : Integer := 2;
      c1, c2               : Constraint_Ptr;
      Coeff1, Coeff2       : Coeff_Vector (1 .. Dimension);
      cap                  : Constr_Array_Ptr;
   begin

      cap := create_Constr_Array (Dimension => dimension, nConstrs => nConstraints);
      coeff1 := (Int_To_Numb  (1), Int_To_Numb (2), Int_To_Numb (3), Int_To_Numb (4), Int_To_Numb (-5));
      coeff2 := (Int_To_Numb (-4), Int_To_Numb (5), Int_To_Numb (6), Int_To_Numb (7), Int_To_Numb  (8));
      c1 := new Constraint'(coeffs    => coeff1,
                            cType     => LESS_EQUAL,
                            rhs       => Int_To_Numb (6),
                            dimension => dimension);
      c2 := new Constraint'(coeffs    => coeff2,
                            cType     => EQUAL,
                            rhs       => Int_To_Numb (10),
                            dimension => dimension);

      cap.cA (1) := c1;
      cap.cA (2) := c2;

      return cap;
   end Build_Dummy_Constr_Array;

   -- print single constraint to stdout
   procedure Print_Constraint (c : access constant Constraint) is
   begin
      for index in c.coeffs'range loop
         Put (Parse_Number_To_String_With_Length (c.coeffs (index), numberStrLength) & "x"); Ada.Integer_Text_IO.Put (index, 0);
         if index /= c.coeffs'last then
            Put (" + ");
         end if;
      end loop;
      case c.cType is
         when LESS_EQUAL   => Put (" <= ");
         when EQUAL        => Put (" == ");
         when GREATER_EQUAL  => Put (" >= ");
      end case;
      Put (Parse_Number_To_String_With_Length (c.Rhs, numberStrLength));
      New_Line (1);
   end Print_Constraint;

   procedure Print_Constr_Array (cap : Constr_Array_Ptr) is
   begin
      for index in 1 .. cap.nConstrs loop
         Put ("c");
         Ada.Integer_Text_IO.Put (index, 3);
         Put (": ");
         print_Constraint (cap.cA (index));
      end loop;
   end Print_Constr_Array;

end Constraint_Array;

