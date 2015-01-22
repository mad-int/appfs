-- with ada packages
with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
-- use packages
use Ada;
use Interfaces;

-- procedure ex1 read a binary file containing integers
-- and lists all positive values in order on console
procedure ex1 is
   -- define special sequential input/output with 32 bit integers
   package Integer_IO is new Sequential_IO (Integer_32);
   use Integer_IO; -- and use this new package

   -- define a boolean array containing all positive values from 0 to Max_Integer
   type Bool_Array is array (0 .. Integer_32'Last) of Boolean with pack;
   -- pointer variable to access boolean array
   type Bool_Array_Ptr is access Bool_Array;

   -- global variables
   data_file    : File_Type;
   value        : Integer_32;
   has_number   : constant Bool_Array_Ptr := new Bool_Array;
   numbers_read : Integer := 0;
   
-- begin procedure
begin
   -- test usage
   if Command_Line.Argument_Count < 1 then
      raise Constraint_Error with "Not enough arguments";
   end if;
   
   -- open file
   Open (data_file, In_File, Command_Line.Argument (1));

   -- read whole file
   while not end_of_file (data_file) loop
      read (data_file, value);
      numbers_read := numbers_read + 1;
	  
	  -- if value positive, set true to boolean array
      if value >= 0 then
	     has_number (value) := true;
      end if;
   end loop;

   -- close file
   close (data_file);

   -- inform user
   Text_IO.Put_Line ("Numbers read = " & Integer'Image (numbers_read));
   -- and print each true value of array to console
   for i in has_number'Range loop
      if has_number (i) then
	     Text_IO.Put_Line (Integer_32'Image (i));
      end if;
   end loop;
end ex1;