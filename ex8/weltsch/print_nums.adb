with Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure Print_Nums is
    type Bool_Arr is array(0 .. Integer'Last) of Boolean;
    pragma Pack (Bool_Arr);
    Input_File : File_Type;
    Input_Stream : Stream_Access;
    Next : Integer;
    Arr : Bool_Arr;
begin
    Open(Input_File,
         In_File,
         "nums.dat");
    Input_Stream := Stream(Input_File);

    while not End_Of_File(Input_File) loop
        Integer'Read(Input_Stream, Next);
        if Next > 0 then
            Arr(Next) := True;
        end if;
    end loop;

    for Num in Integer loop
        if Arr(Num) then
            Ada.Text_IO.Put_Line(Integer'Image(Num));
        end if;
    end loop;
end Print_Nums;
