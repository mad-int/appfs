with Ada.Command_line;           use Ada.Command_line; -- Command line arguments
with Ada.Sequential_IO;                                -- Reading file
with Ada.Text_IO;                use Ada.Text_IO;      -- Put_Line
with Ada.Unchecked_Deallocation;                       -- Deallocate arrays
with Interfaces;                 use Interfaces;       -- Integer_32


procedure ex8 is
   -- Package declaration
   package I32_Seq_IO is new Ada.Sequential_IO(Element_Type => Integer_32);
   use I32_Seq_IO;
   package I32_Text_IO is new Ada.Text_IO.Integer_IO (Integer_32);
   use I32_Text_IO;

   -- Type declaration
   type Binary    is new Integer range 0 .. 1;
   type Arr_Type  is array (Integer_32 range <>) of Binary;
   type Arr_Ptr   is access Arr_Type;

   -- Procedure declaration
   procedure Free is new Ada.Unchecked_Deallocation (Arr_Type, Arr_Ptr);

   -- Variable declaration
   File     : I32_Seq_IO.FILE_TYPE;
   Value    : Integer_32;
   ReadFlag : Arr_Ptr           := new Arr_Type (0 .. Integer_32'Last);

begin
   -- Check for exactly one argument
   if Argument_Count /= 1 then
      Put ("Usage: ");
      Put (Command_Name);
      Put (" filename");
      Set_Exit_Status (Failure);
      return;
   end if;

   -- Set array to initial value
   for i in ReadFlag'Range loop
      ReadFlag(i) := 0;
   end loop;

   -- Read file
   Open(File, In_File, Argument(1));
   while not End_Of_File (File) loop
      Read(File, Value);
      if Value >= 0 then
         ReadFlag(Value) := 1;
      end if;
   end loop;
   Close(File);

   -- Check array for set values
   for i in ReadFlag'Range loop
      if ReadFlag(i) = 1 then
         Put_Line (Integer_32'Image (i));
      end if;
   end loop;

   Free(ReadFlag);
end;
