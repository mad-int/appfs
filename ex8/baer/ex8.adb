with Ada.Assertions;    -- Assert()
with Ada.Containers.Generic_Array_Sort;
with Ada.Directories;   -- File_Size, Size()
with Ada.Sequential_IO; -- Open(), Read(), Close()
with Ada.Strings.Fixed; -- Trim()
with Ada.Text_IO;       -- Put_Line()

use Ada.Assertions;
use Ada.Strings.Fixed;
use Ada.Text_IO;

procedure ex8
    with Pre => Integer'Size = 32
is
    package IntIO is new Ada.Sequential_IO (Element_Type => Integer);
    use IntIO;

    type ArrType is array (Integer range <>) of Integer;
    type ArrRef is access ArrType;

    procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type  => Integer, Element_Type => Integer, Array_Type => ArrType);

    filesize: constant Integer := Integer(Ada.Directories.Size("ndata.dat"));
    -- arr is never freed in the program!
    arr     : constant ArrRef  := new ArrType (1 .. filesize/4);
begin
    Assert(0 = filesize mod 4, "Error: ndata.dat size is not a multiple of 4!");

    -- Read file to array.
    declare
        infile  : IntIO.File_Type;
        noElems : constant Integer := filesize / 4;
        pos     : Integer := 1;
    begin
        Open(infile, In_File, "ndata.dat"); -- "/usr/local/src/ndata.dat");
        Read(infile, arr(pos));
        Assert(123456789 = arr(pos), "Error: First number in ndata.dat is not 123456789!");
        pos := pos + 1;

        -- If filesize changes in between check for EOF and that we don't outgrow the array.
        -- We started counting with 1.
        while (not End_Of_File(infile)) and (pos <= noElems)
        loop
            Read(infile, arr(pos));
            pos := pos + 1;
        end loop;
        --Put_Line(Integer'Image(noElems));
        --Put_Line(Integer'Image(pos));

        Close(infile);
    end;

    -- Sort array.
    Sort(arr.all);

    -- Print sorted array.
    declare
        last    : Integer := -1;
    begin
        for elem of arr.all
        loop
            if (0 <= elem) and (last /= elem) then
                -- Trim() is for trimming the leading whitespaces of Integer'Image()
                -- => Integer'Image() suckz.
                Put_Line( Trim(Integer'Image(elem), Ada.Strings.Left) );
                last := elem;
            end if;
        end loop;
    end;

end ex8;

