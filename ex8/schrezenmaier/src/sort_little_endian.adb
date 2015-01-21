-- usage: sort_little_endian file_name
--
-- Due to memory limitations we read in the file 128 times.
-- This could be changed to a lower value.

with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Sort_Little_Endian is
   type Byte is range 0..255;
   for Byte'Size use 8;
   type First_Bytes is array (0 .. 2) of Byte;
   type Last_Byte is range -128..127;
   for Last_Byte'Size use 8;
   type Little_Endian is record
      First_Byte_Values : First_Bytes;
      Last_Byte_Value : Last_Byte;
   end record;
   Number : Little_Endian;

   package Byte_IO is new Ada.Sequential_IO(Little_Endian);
   File : Byte_IO.File_Type;
   Byte_Number_Exception : exception;

   type Bool_Arr is array (Byte range 0 .. 255, Byte range 0 .. 255, Byte range 0 .. 255) of Boolean;
   pragma Pack (Bool_Arr);
   Contained : Bool_Arr;
   
   L : Last_Byte;   
   I, J, K : Integer;
   Value : Integer;
begin
   for L in Last_Byte range 0 .. 127 loop
      Contained := (others => (others => (others => False)));
      -- read in the file
      Byte_IO.Open(File => File, Mode => Byte_IO.In_File, Name => Ada.Command_Line.Argument (1));
      while not Byte_IO.End_Of_File(File => File) loop
         Byte_IO.Read (File => File, Item => Number);
         if (Number.Last_Byte_Value = L) then
            Contained(Number.First_Byte_Values(0), Number.First_Byte_Values(1), Number.First_Byte_Values(2)) := True;
         end if;
      end loop; -- while
      -- print out the sorted numbers
      for K in Byte range 0 .. 255 loop
         for J in Byte range 0 .. 255 loop
            for I in Byte range 0 .. 255 loop
               if Contained(I, J, K) then
                  Value := Integer(I) + 2**8 * Integer(J) + 2**16 * Integer(K) + 2**24 * Integer(L);
                  Ada.Text_IO.Put_Line (Integer'Image(Value));
               end if;
            end loop; -- for I
         end loop; -- for J
      end loop; -- for K
      Byte_IO.Close (File);
   end loop; -- for L
end Sort_Little_Endian;
