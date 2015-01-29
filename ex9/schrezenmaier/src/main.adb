-- main.adb

with Linear_Program_Package; use Linear_Program_Package;
with Gray_Code_Package; use Gray_code_Package;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with GNAT.OS_Lib;

procedure Main is
begin
   if Argument_Count /= 1 then
      Put_Line (Standard_Error, "usage: " & Command_Name & " input_filename");
      Gnat.OS_Lib.OS_Exit (Integer (Failure));
   end if;

   declare
      LP        : Linear_Program := Read_LP_From_File (Argument (1));
      Solutions : Int_64         := LP.Count_Feasible_Binary;
   begin
      Put_Line ("found " & Int_64'Image (Solutions) & " solutions");
   end;
end Main;
