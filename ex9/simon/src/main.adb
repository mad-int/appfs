with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
-- local
-- use all type: make standard operations like "=" on "Exit_Status" visible in here
with Read_Bip; use all type Read_Bip.Exit_Status;
with Constraint_Array; use Constraint_Array;

-- TODOs
--    * implement redundancy checks

procedure Main is
   result            : Read_Bip.Exit_Status;
   cap               : Constr_Array_Ptr := null;

   procedure Usage is
   begin
      Put ("Usage: " & Command_Name & " inputFile");
      Set_Exit_Status (Failure);
   end Usage;

begin
   -- check for exactly one command line argument
   if Argument_Count /= 1 then
      Usage;
      return;
   end if;

   result := Read_Bip.Read_Bip_From_File (Argument (1), cap => cap);
   case result is
      when READING_ERROR =>
         -- data should already be free'd
         Set_Exit_Status (Failure);
         return;
      when UNCONSTRAINED =>
         Print_Unconstrained_Solution (cap.dimension);
      when  INFEASIBLE =>
         Print_No_Feasible_Assignment;
      when NORMAL =>
         -- Put_Line ("BIP from file:");
         -- Print_Constr_Array (cap);
         Put_Line ("Reading successful! Starting enumeration now.");
         Enumerate (cap);
   end case ;
   Free_Constr_Array (cap => cap);
   -- normal termination
   Set_Exit_Status (Success);
end main;