with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Command_Line;

procedure Print_Nums is
    type Bool_Arr is array(Natural) of Boolean; -- one entry for each 0 <= int <= Integer'Last
    pragma Pack (Bool_Arr); -- might optimize the array size
    type Bool_Arr_Access is access Bool_Arr; -- pointer type for array, needed to enable heap allocation
    procedure free is new Ada.Unchecked_Deallocation(Bool_Arr, Bool_Arr_Access);
    Input_File : File_Type;
    Input_Stream : Stream_Access;
    Next : Integer;
    Arr : Bool_Arr_Access := new Bool_Arr; -- allocate on heap instead of stack
begin
    if Ada.Command_Line.Argument_Count /= 1 then
        Ada.Text_IO.Put_Line("Usage: " & Ada.Command_Line.Command_Name & " <file>");
        Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
    end if;

    Open(Input_File,
         In_File,
         "nums.dat");
    Input_Stream := Stream(Input_File);

    -- I would like to initialize like this
    -- Arr.all := (others => False);
    -- but this will also allocate a Bool_Arr on the stack...
    for Num in Natural loop
        Arr(Num) := False;
    end loop;

    -- go through the file and check the entries in the array
    while not End_Of_File(Input_File) loop
        Integer'Read(Input_Stream, Next);
        if Next > 0 then
            Arr(Next) := True;
        end if;
    end loop;

    -- print the numbers that we found in the file
    for Num in Natural loop
        if Arr(Num) then
            -- Integer'Image has a leading space...
            -- so we Put here with no leading stuff
            Ada.Integer_Text_IO.Put(Num, 0);
            Ada.Text_IO.New_Line;
        end if;
    end loop;

    free(Arr);
end Print_Nums;
