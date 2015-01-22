with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings;
use Ada.Strings;
with Ada.IO_Exceptions;

package body Binary_Integer_Program is

   -- Create an empty instance of a BIP --
   procedure Create_Empty_BIP(bipInstance : out BIP_ptr ; numOfRows : in Positive ; numOfColumns : in Positive)
   is
   begin
      bipInstance := new Bip;
      bipInstance.numOfRows := numOfRows;
      bipInstance.numOfRowsAdded := 0;
      bipInstance.numOfColumns := numOfColumns;
      bipInstance.bca := new BipConstraintArray(1..numOfRows);
   end Create_Empty_BIP;



   -- Add a constraint to the BIP --
   procedure Add_Constraint_to_BIP(bipInstance : in BIP_ptr ; bipCon : in Constraint_ptr)
   is
   begin
      if bipCon.coefficients'Length /= bipInstance.numOfColumns then
         raise WRONG_CONSTRAINT_DIMENSION_EXCEPTION;
      end if;
      if bipInstance.numOfRowsAdded = bipInstance.numOfRows then
         raise WRONG_NUMBER_OF_ROWS_EXCEPTION;
      end if;

      bipInstance.numOfRowsAdded := bipInstance.numOfRowsAdded + 1;
      bipInstance.bca(bipInstance.numOfRowsAdded) := bipCon;
   exception
      when WRONG_CONSTRAINT_DIMENSION_EXCEPTION =>
         Put_Line("Tried to add a constraint with a wrong dimension. Skip it.");
   end Add_Constraint_to_BIP;



   -- Method to enumerate all feasible BIP solutions --
   function Determine_Number_of_Feasible_Solutions_for_BIP(bipInstance : in BIP_ptr) return Long_Integer
   is
      solVector : BinaryVector_ptr;
      numberOfSolutions : Long_Integer := 0;
   begin
      solVector := new BinaryVector(1..bipInstance.numOfColumns);
      Enumerate_Solutions_of_BIP_Recursively(bipInstance , solVector , 0 , numberOfSolutions);
      return numberOfSolutions;
   end Determine_Number_of_Feasible_Solutions_for_BIP;



   --Method to recursively create and evaluate all binary solutions--
   procedure Enumerate_Solutions_of_BIP_Recursively(bipInstance : in BIP_ptr ; solVec : in out BinaryVector_ptr; bitChangeIndex : in Integer; numberOfSolutions : in out Long_Integer)
   is
      currentIndex : Integer;
   begin
      if bitChangeIndex = solVec'Length then
         if Check_Feasibility_Of_Solution( bipInstance , solVec) then
            numberOfSolutions := numberOfSolutions + 1;
         end if;
         return;
      end if;

      currentIndex := bitChangeIndex + 1;
      solVec(currentIndex) := 0;
      Enumerate_Solutions_of_BIP_Recursively(bipInstance , solVec , currentIndex,numberOfSolutions);
      solVec(currentIndex) := 1;
      Enumerate_Solutions_of_BIP_Recursively(bipInstance , solVec , currentIndex,numberOfSolutions);

   end Enumerate_Solutions_of_BIP_Recursively;



   --Method to check feasibility of a binary vector--
   function Check_Feasibility_Of_Solution(bipInstance : in BIP_ptr; solVec : in BinaryVector_ptr) return Boolean
   is
   begin
      for I in 1 .. bipInstance.numOfRows loop
         if not Check_Feasibility_For_Single_Constraint(bipInstance.bca(I),solVec) then
            return False;
         end if;
      end loop;
      return True;
   end Check_Feasibility_Of_Solution;



   -- Function checking feasibility of a solution for a single constraint --
   function Check_Feasibility_For_Single_Constraint(cons : in Constraint_ptr ; solVec : in BinaryVector_ptr) return Boolean
   is
      lhsValue : Integer := 0;
      indexForEx : Integer;
   begin
      for I in cons.coefficients'Range loop
         if solVec(I) = 1 then
            indexForEx := I;
            lhsValue := cons.coefficients(I) + lhsValue;
         end if;
      end loop;

      case cons.ineqType is
      when LEQ =>
         if lhsValue - cons.rhs <= 0 then
            return TRUE;
         else
            return FALSE;
         end if;
      when GEQ =>
         if lhsValue - cons.rhs >= 0 then
            return TRUE;
         else
            return FALSE;
         end if;
      when EQ =>
         if lhsValue - cons.rhs = 0 then
            return TRUE;
         else
            return FALSE;
         end if;
      end case;

   exception
      when CONSTRAINT_ERROR =>
         Put_Line("An over- or underflow was detected adding/substracting " & Integer'Image(cons.coefficients(indexForEx)) & ", " & Integer'Image(lhsValue) & ", or " & Integer'Image(cons.rhs));
         raise OVER_UNDERFLOW_EXCEPTION;
   end Check_Feasibility_For_Single_Constraint;




   -- Method reading a BIP from a file --
   procedure Read_BIP(bipInstance_ptr : out BIP_ptr; fileName : String)
   is
      data_File : File_Type;
      line : Unbounded_String;
      numOfCols : Integer;
      numOfRows : Integer;
      cons : Constraint_ptr;
   begin
      Open(data_File,In_File,fileName);

      -- Reads the number of columns --
      Set_Unbounded_String(line, Get_Line (data_File));
      numOfCols := Read_Matrix_Dimension(line);

      -- Reads the number of rows --
      Set_Unbounded_String(line, Get_Line (data_File));
      numOfRows := Read_Matrix_Dimension(line);

      -- Create an empty BIP --
      Create_Empty_BIP(bipInstance_ptr,numOfRows,numOfCols);

      -- Read and add the constraints --
      while not End_Of_File(data_File) loop
         begin
            Set_Unbounded_String(line, Get_Line (data_File));
            cons := Create_Linear_Constraint(line,numOfCols);
            Add_Constraint_to_BIP(bipInstance_ptr,cons);
         end;
      end loop;
      Check_Consistency(bipInstance_ptr);
   exception
      when UNKNOWN_CONSTRAINT_TYPE =>
         Put_Line("Unknown constraint type! Terminate procedure!");
         bipInstance_ptr := null;
         return;
      when WRONG_MATRIX_DIMENSION_EXCEPTION =>
         Put_Line("Since the dimension of the matrix of the BIP was invalid the procedure terminates.");
         bipInstance_ptr := null;
         return;
      when WRONG_NUMBER_OF_ROWS_EXCEPTION =>
         Put_Line("Since the number of added rows is invalid the procedure terminates.");
         bipInstance_ptr := null;
      when ADA.IO_EXCEPTIONS.END_ERROR =>
         Put_Line("No matrix dimensions have been specified (Maybe the file is empty?!).");
         bipInstance_ptr := null;
         return;
      when PARSING_EXCEPTION =>
         Put_Line("Was not able to parse a numerical value (Maybe wrong number of coefficients, no type, or no RHS?!).");
         bipInstance_ptr := null;
         return;
      when OVER_UNDERFLOW_EXCEPTION =>
         Put_Line("An over- or underflow occured during the calculations.");
         bipInstance_ptr := null;
         return;
   end Read_BIP;


   procedure Check_Consistency(bipInstance : in BIP_ptr)
   is
   begin
      if  bipInstance.numOfRows /= bipInstance.numOfRowsAdded then
         raise WRONG_NUMBER_OF_ROWS_EXCEPTION;
      end if;
   end Check_Consistency;


   procedure Print_BIP(bipInstance : in BIP_ptr) is
   begin
      for I in 1 .. bipInstance.numOfRows loop
         for J in 1 .. bipInstance.numOfColumns loop
            Put(Integer'Image(bipInstance.bca(I).coefficients(J)));
         end loop;
         case bipInstance.bca(I).ineqType is
            when LEQ =>
               Put(" <= ");
            when GEQ =>
               Put(" >= ");
            when EQ =>
               Put(" == ");
         end case;
         Put_Line(Integer'Image(bipInstance.bca(I).rhs));
      end loop;
   end;

   -- Reads the dimension of the BIP --
   function Read_Matrix_Dimension(line : in Unbounded_String) return Positive
   is
      commentIndex : Integer;
   begin
      commentIndex := Index(line,"#");

      if commentIndex /= 0 then
         return Integer'Value(Slice(line,1,commentIndex-1));
      else
         return Integer'Value(To_String(line));
      end if;

   exception
      when CONSTRAINT_ERROR =>
         if commentIndex /= 0 then
            Put_Line("Invalid Matrix Dimension: " & Slice(line,1,commentIndex-1));
         else
            Put_Line("Invalid Matrix Dimension: " & To_String(line));
         end if;
         raise WRONG_MATRIX_DIMENSION_EXCEPTION;
   end Read_Matrix_Dimension;

   function Create_Linear_Constraint(line : in Unbounded_String ; numOfCols : in Integer) return Constraint_ptr
   is
      typeString : Unbounded_String;
      consType : ConstraintType;
      coeffs : Vector_ptr;
      rhs : Integer;
      indexFirst : Integer := 1;
      indexLast : Integer := 1;
   begin
      coeffs := new Vector.Vector(1..numOfCols);

      for I in 1 .. numOfCols loop
         indexFirst := Index_Non_Blank(line,indexLast);
         indexLast := Index(line," ",indexFirst);
         coeffs(I) := Integer'Value(Slice(line,indexFirst,indexLast));
      end loop;

      -- Read the type of the constraint --
      indexFirst := Index_Non_Blank(line,indexLast);
      indexLast := Index(line," ",indexFirst);
      Set_Unbounded_String(typeString,Slice(line,indexFirst,indexLast-1));

      if typeString = "<=" then
         consType := LEQ;
      elsif typeString = ">=" then
         consType := GEQ;
      elsif typeString = "==" then
         consType := EQ;
      else
         raise UNKNOWN_CONSTRAINT_TYPE;
      end if;

      indexFirst := Index_Non_Blank(line,indexLast);
      indexLast :=  Index_Non_Blank(line, Backward);
      rhs := Integer'Value(Slice(line,indexFirst,indexLast));
      return Create_Linear_Constraint(consType,coeffs,rhs);
   exception
      when CONSTRAINT_ERROR =>
         if indexFirst /= 0 and indexLast > indexFirst then
            Put_Line("Could not parse numerical value: " & Slice(line,indexFirst,indexLast));
         else
            Put_Line("Problems parsing line: " & To_String(line));
         end if;
         raise PARSING_EXCEPTION;
   end Create_Linear_Constraint;


end Binary_Integer_Program;

