-- with packages
with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
with Binary_Program;

-- use packages
use Interfaces;
use Ada.Text_IO;
use Binary_Program;

-- main procedure starts to read from a given binary  program
-- and try to solve it. It lists all solutions on the console.
procedure main is
   -- returns next index of white space character
   function Get_Next_Space ( str   : in String;  -- typically a line
			     start : in Integer; -- (start+1) index to search
			     max   : in Integer )-- max index to search
			    return Integer is    -- return value
   begin
      -- from start+1 to max, search for white space
      for index in (start+1) .. max loop
	 if str (index) = ' ' then
	    return index;
	 end if;
      end loop;
      return -1; -- return -1 if no white space found
   end Get_Next_Space;

   -- returns first index of white space character in a string
   function Get_First_Space
     ( str : in String;
       max : in Natural )
      return Integer is
   begin
      return Get_Next_Space(str, 0, max);
   end Get_First_Space;

   -- define header informations for # variables and # constraints
   type Header_Arr is array (Integer range 1 .. 2) of Legal_Int;

   --
   data_file   : File_Type;
   line_buffer : String (1 .. 1024);
   last_index  : Natural;
   Header      : Header_Arr;

begin
   -- check input data
   if Ada.Command_Line.Argument_Count < 1 then
      raise Constraint_Error with "Specify a binary program to start please.";
   end if;

   -- open file, a binary program
   Open (data_file, In_File, Ada.Command_Line.Argument (1));

   -- analyze header of binary program
   -- this part contains number of variables and constraints
   declare
      idx : Integer;
   begin
      -- read both header lines
      for i in Header'Range loop
		Get_Line (data_file, line_buffer, last_index);
		idx := Get_First_Space (line_buffer, last_index);
		Header(i) := Legal_Int'Value( line_buffer( 1 .. idx ) );
      end loop;
   end;

   -- inform the user about the header informations
   Put_Line ( "Read binary program with" &
		Legal_Int'Image (Header (1)) &
		" variables and" &
		Legal_Int'Image (Header (2)) &
		" constraints." );

   declare
      const_types: Const_Type_Arr ( 1 .. Header (2) );
      solutions  : Floating_Arr   ( 1 .. Header (2) );
      matrix     : Matrix_Arr_Arr ( 1 .. Header (2), 1 .. Header (1) );

      curr       : Integer := 1;
      prev       : Integer := 1;

   begin
      for Con in 1 .. Header (2) loop
	 Get_Line (data_file, line_buffer, last_index);
	 curr       := 1;

	 Put_Line ( line_buffer (1 .. last_index) );

	 for Var in 1 .. Header (1) loop
	    prev := curr;
	    curr := Get_Next_Space (line_buffer, curr, last_index);
	    matrix(Con,Var) := Float'Value( line_buffer(prev .. curr) );
	 end loop;

	 prev := curr;
	 curr := Get_Next_Space (line_buffer, curr, last_index);

	 const_types (Con) := Get_Constraint_Type ( line_buffer (prev .. curr) );

	 solutions (Con) := Float'Value ( line_buffer ((curr + 1) .. last_index) );
      end loop;

      -- solve loaded problem
      Solve( matrix, const_types, solutions );

   end;


   -- close file
   Close (data_file);
end main;
