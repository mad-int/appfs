with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Ex8 is
package NumbersIO is new Ada.Sequential_IO(Integer);
NumbersFile : NumbersIO.File_Type;
Number : Integer;
type Bitmap is array (0 .. Integer'Last) of Boolean with Default_Component_Value => False;
pragma Pack(Bitmap);
type BitmapPointer is access Bitmap;
NumbersBitmap : BitmapPointer; 

begin
	if (Argument_Count > 0) then	
		NumbersBitmap := new Bitmap;
		NumbersIO.Open(File => NumbersFile, Mode => NumbersIO.In_File, Name => Argument(1));
		while not NumbersIO.End_Of_File(NumbersFile) loop
			NumbersIO.Read(NumbersFile, Number);
			if (Number >= 0) then
				NumbersBitmap(Number) := True;
			end if;
		end loop; 
		NumbersIO.Close(NumbersFile);
		for i in NumbersBitmap'Range loop
			if (NumbersBitmap(i)) then
				Put_Line(Trim(Natural'Image(i), Ada.Strings.Left));
			end if;
		end loop;
	else
		Put_Line("Usage: " & Command_Name & " filename");
	end if;
	exception
		when Name_Error =>
			Put_Line("Could not open the file " & Argument(1));
end Ex8;
