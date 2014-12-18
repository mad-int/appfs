with Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;
use Ada.Command_Line;

procedure main is
   --Create IO for Integer numbers --
   package Integer_IO is new Ada.Sequential_IO(Integer);
   use Integer_IO;

   --The array indexing if a certain integer number is contained--
   type IntegerIndexArray is array (Integer range 0..Integer'Last) of Boolean with default_component_value => False;
   type IntegerIndexArray_ptr is access IntegerIndexArray;

   data_File : File_Type;
   value : Integer;
   indicatorArray : IntegerIndexArray_ptr;

begin

   if not (Argument_Count > 0) then
      Ada.Text_IO.Put_Line("MISSING INPUT PARAMETER: Please choose the file to read in.");
      return;
   end if;

   indicatorArray := new IntegerIndexArray;
   Open(data_File, In_File, Argument(1));

   while not End_Of_File(data_File) loop
      Read(data_File,value);
      if value >= 0 then
         indicatorArray(value) := True;
      end if;
   end loop;

   for number in 0 .. Integer'Last loop
      if indicatorArray(number) then
         Ada.Text_IO.Put_Line(Integer'Image(number));
      end if;
   end loop;

end main;




