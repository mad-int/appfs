-- with packages
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
with Binary_Program; -- BIP

-- use packages
use Interfaces;
use Ada.Text_IO;
use Binary_Program; -- BIP

-- Binary Integer Program Solver
--
-- main procedure starts to read from a given binary  program
-- and try to solve it. It lists all solutions on the console.
--
procedure main is
   -- first declare some useful methods

   -- Get Next Space needs a string, start position and
   -- returns next index of white space character
   function Get_Next_Space ( str   : in String;  -- typically a line
			     start : in Integer; -- (start+1) index to search
			     max   : in Integer  -- max index to search
			    ) return Integer     -- return index
   is
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

   -- define header array informations for # variables and # constraints
   type Header_Arr is array (Integer range 1 .. 2) of Legal_Int;

   data_file   : File_Type;
   line_buffer : String (1 .. 1024); -- line buffer
   last_index  : Natural;
   Header      : Header_Arr;

begin
   -- check input data
   if Ada.Command_Line.Argument_Count < 1 then
      raise Constraint_Error with "Specify a binary program to start please.";
   end if;

   -- open file, a binary integer program
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

   -- read constraints
   declare
      const_types: Const_Type_Arr ( 1 .. Header (2) ); -- constraint types, see binary_program.ads
      solutions  : Floating_Arr   ( 1 .. Header (2) ); -- array of floating point values
      matrix     : Matrix_Arr_Arr ( 1 .. Header (2), 1 .. Header (1) ); -- matrix A, see binary_program.ads

      curr       : Integer := 1; -- index pointer current
      prev       : Integer := 1; -- index pointer previous
   begin
      -- for each constraint
      for Con in 1 .. Header (2) loop
	 -- read line
	 Get_Line (data_file, line_buffer, last_index);
	 curr       := 1;

	 -- show line on console
	 Put_Line ( line_buffer (1 .. last_index) );

	 -- analyze variables
	 for Var in 1 .. Header (1) loop
	    -- searching from whitespace to whitespace
	    prev := curr;
	    curr := Get_Next_Space (line_buffer, curr, last_index);
	    -- fill matrix A
	    matrix(Con,Var) := Float'Value( line_buffer(prev .. curr) );
	 end loop;

	 -- get constraint type
	 prev := curr;
	 curr := Get_Next_Space (line_buffer, curr, last_index);
	 const_types (Con) := Get_Constraint_Type ( line_buffer (prev .. curr) );

	 -- get value b
	 solutions (Con) := Float'Value ( line_buffer ((curr + 1) .. last_index) );
      end loop;

      -- inform user
      New_Line;
      Put_Line ("BIP read. Searching for solutions...");

      -- solve loaded problem
      Solve( matrix, const_types, solutions );
   end;

   -- close file
   Close (data_file);
end main;
