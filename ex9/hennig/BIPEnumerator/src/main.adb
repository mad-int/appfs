with Ada.Command_Line;
with Ada.Text_IO;
with Binary_Integer_Program;
use Ada.Command_Line;
use Ada.Text_IO;
use Binary_Integer_Program;

procedure main is
   bipInstance : BIP_ptr;
   numberOfSolutions : Long_Integer;
begin

   if not (Argument_Count > 0) then
      Ada.Text_IO.Put_Line("MISSING INPUT PARAMETER: Please choose the file to read in.");
      return;
   end if;

   Read_BIP(bipInstance,Argument(1));

   if bipInstance = null then
      Put_Line("Program terminating normally.");
      return;
   end if;

   -- Print_BIP(bipInstance);

   numberOfSolutions := Determine_Number_of_Feasible_Solutions_for_BIP(bipInstance);

   Put("BIP " & Argument(1) & " has " & Long_Integer'Image(numberOfSolutions) & " solutions!");

exception
   when OVER_UNDERFLOW_EXCEPTION =>
      Put_Line("Program terminating normally.");
      return;
end main;