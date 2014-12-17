with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Sequential_IO;
with Interfaces; use Interfaces; -- for Unsigned_32
with Ada.Command_line; use Ada.Command_line; -- evaluating command line arguments
with Ada.Unchecked_Deallocation; -- for definition of free

procedure ex8 is

   -- instantiate package for reading a binary file containing 32-bit Integer values
   package Seq_I32_IO is new Ada.Sequential_IO (Element_Type => Integer_32);
   -- for putting Integer_32 values without leading space
   package I32_Text_IO is new Ada.Text_IO.Integer_IO (Integer_32);

   type bitMap is array (Integer_32 range <>) of Unsigned_8
      with Default_Component_Value => 0;
   type bitMap_Ptr is access bitMap;
   procedure Free is new Ada.Unchecked_Deallocation (bitMap, bitMap_Ptr);

   File  : Seq_I32_IO.File_Type;
   value : Integer_32; -- variable for reading and printing the numbers

   x     : Integer_32 := 2 ** 28; -- needed bitmap size: (positive numbers in Integer_32)/(number of bits in an Unsigned_8) = (2 ^ 31) / (8) = 2 ^ 28 */
   bmp   : bitMap_Ptr := new bitMap (0 .. x-1);

   -- usage procedure
   procedure usage is
   begin
      Put ("Usage: " & Command_Name & " datafile");
      Set_Exit_Status (Failure);
   end usage;

begin

   -- check for exactly one command line argument
   if Argument_Count /= 1 then
      usage;
      return;
   end if;

   Seq_I32_IO.Open (File => File,
                    Mode => Seq_I32_IO.In_File,
                    Name => Argument(1));

   while not Seq_I32_IO.End_Of_File (File) loop
      -- assumption: little endian system
      Seq_I32_IO.Read (File, value);
      -- Put_Line ("Integer a: " & Integer_32'Image (value));

      -- store only positive values
      if (value >= 0) then
         declare
            q    : Integer_32 := value / Integer_32 (8); --  quotient: correct position in the bitmap-array
            r    : Integer_32 := value rem Integer_32 (8); -- remainder: correct bit in the Unsigned_32 at this position
            mask : Unsigned_8 := Shift_Left (1, Integer (r));
         begin
            if (bmp.all (q) and mask) = 0 then -- if bitMap has a zero at the position of value
               bmp.all (q) := bmp.all (q) + mask; -- then turn it into a 1
            end if;
         end;
      end if;
   end loop;

   Seq_I32_IO.Close(File);

   value:=0; -- represents the current integer stored
   for i in bmp.all'Range loop
      declare
         bitVector : Unsigned_8 := bmp(i);
         mask      : Unsigned_8 := 1;
      begin
         for j in 1 .. 8 loop
            if (mask and bitVector) > 0 then
               I32_Text_IO.Put (value, Width => 0);
               New_Line(1);
            end if;
            value := value + 1;
            mask := Shift_Left(mask,1);
         end loop;
      end;
   end loop;

   Free(bmp);

end ex8;
