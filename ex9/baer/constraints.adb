with Ada.Assertions;
with Ada.Exceptions;
-- with Ada.Text_IO; -- Open(), Get_Line(), Put_Line(), Close()
with Ada.Strings.Fixed; -- Trim()
with Ada.Strings.Maps; -- Maps.Character_Set, To_Set()
with Ada.Characters.Latin_1;

use Ada;
use Ada.Strings.Fixed;

--
-- Package Implementation
--
package body Constraints is

    package Double_Text_IO is new Ada.Text_IO.Float_IO(Double);
    --package Bin_Text_IO is new Ada.Text_IO.Integer_IO(Bin);

    ---

    PRECISION : constant Double := 0.000001; -- 1E-5 doesn't work!?

    function LessEq(d1 : Double; d2 : Double) return Boolean is
    begin
        return (d1-d2) <= PRECISION;
    end LessEq;

    function GreatEq(d1 : Double; d2 : Double) return Boolean is
    begin
        return (d1-d2) >= -PRECISION;
    end GreatEq;

    function Equal(d1 : Double; d2 : Double) return Boolean is
    begin
        return abs(d1-d2) <= PRECISION;
    end Equal;

    ---

    function Get_Rows(cs : in Constraints_t) return Int is
    begin
        return cs.matrix'Last(1);
    end Get_Rows;

    function Get_Cols(cs : in Constraints_t) return Int is
    begin
        return cs.matrix'Last(2);
    end Get_Cols;

    --
    --
    --
    function is_Feasible(cs : in Constraints_t; x : in BinVector_Type) return Boolean
    is
        feasible : Boolean := TRUE;
        sum : Double;

    begin
        Check_loop:
        for row in 1 .. cs.rows loop
            sum := 0.0;
            for col in 1 .. cs.cols loop
                if x(col) then
                    sum := sum + cs.matrix(row, col);
                end if;
            end loop;

            case cs.cmp(row) is
                when LESSEQ => feasible :=  LessEq(sum, cs.rhs(row));
                when GREATEQ=> feasible := GreatEq(sum, cs.rhs(row));
                when EQUAL  => feasible :=   Equal(sum, cs.rhs(row));
            end case;

            exit Check_Loop when not feasible;
        end loop Check_Loop;

        return feasible;
    end;

    --
    --
    --
    function is_Feasible_BitFlip(cs : in Constraints_t; x : in BinVector_Type;
            updatemask : in BinVector_Type; actualLhs : in out Vector_Type) return Boolean
    is
        col : Int := 1;
        feasible : Boolean := FALSE;
    begin

        --
        -- Determin column to change.
        --
        -- TODO: How to do it better with this intrinsic or binary-search bit-magic?
        for i in updatemask'Range loop
            if updatemask(i) = ONE then
                col := i;
                exit;
            end if;
        end loop;

        Assertions.Assert(updatemask(col) /= ZERO, "No set bit found in updatemask.");

        --
        -- Update the actualLhs.
        --
        if x(col) = ONE then -- bit set (1)
            for row in 1 .. cs.rows loop
                actualLhs(row) := actualLhs(row) + cs.matrix(row, col);
            end loop;

        else -- bit cleared (0)
            for row in 1 .. cs.rows loop
                actualLhs(row) := actualLhs(row) - cs.matrix(row, col);
            end loop;
        end if;

        --
        -- Check the actualLhs
        --
        for row in 1 .. cs.rows loop
            case cs.cmp(row) is
                when LESSEQ => feasible :=  LessEq(actualLhs(row), cs.rhs(row));
                when GREATEQ=> feasible := GreatEq(actualLhs(row), cs.rhs(row));
                when EQUAL  => feasible :=   Equal(actualLhs(row), cs.rhs(row));
            end case;

            exit when not feasible;
        end loop;

        return feasible;
    end;

    --
    --
    --
    function Read_Constraints(filename: in String) return Constraints_t
    is
        use Ada.Exceptions;

        file        : Text_IO.File_Type;
        linecount   : Integer := 1;

    begin
        -- Raises Exception if can't be opened.
        Text_IO.Open (File => file,
            Mode => Text_IO.In_File,
            Name => filename);

        declare
            cols : constant Int := Read_Int(file, linecount);
            rows : constant Int := Read_Int(file, linecount);
            -- cs : constant Constraints_t(rows, cols) := Read_Constraints(file, linecount, rows, cols);

            cs : Constraints_t(rows, cols);
            -- TODO: Initalise cs? The following doesn't work:
            --   := (matrix => (others => (others => 0.0)), cmp => (others => LESSEQ), rhs => (others => 0.0));
            -- the compiler says:
            --   no value supplied for discriminant "rows"
            -- There seems to be a problem with initalising the multi-dim array "matrix" in the record.
        begin
            for constrno in 1 .. cs.rows loop
                Read_Constraint(file, linecount, constrno, cs);
            end loop;

            -- Loop through possible existing trailing-lines.
            while not Text_IO.End_Of_File(file) loop
            declare
                line : constant String := Trim_Line(Text_IO.Get_Line(file));

            begin
                if line'length /= 0 then
                    raise Constraint_Error with "Found trailing stuff after the last "
                                                & "constraint-line was read: " & line;
                end if;

                linecount := linecount + 1;
            end;
            end loop;

            Text_IO.Close(file);
            return cs;
        end;

    exception
        when ex : Constraint_Error =>
                        Text_IO.Close(file); -- necessary ?
                        raise Constraint_Error with
                            "Error in " & filename & "(" & Integer'Image(linecount) & "): "
                            & Exception_Message(ex);
        when others =>  Text_IO.Close(file); -- necessary ?
                        raise;
    end Read_Constraints;


    --
    --
    --
    function Read_Int(file : in Text_IO.File_Type; linecount : in out Integer) return Int
    is
    begin
        while not Text_IO.End_Of_File(file) loop
        declare
            line : constant String := Trim_Line(Text_IO.Get_Line(file));

        begin
            -- Text_IO.Put_Line(Natural'Image(linecount) & ": `" & line & "'");

            -- If Positive'Value raises an Exception, linecount is not(!) updated to the outside.
            -- I rely on this here.
            linecount := linecount + 1;

            if line'length /= 0 then
                return Positive'Value(line);
            end if;
        end;
        end loop;

        raise Constraint_Error with "Premature End-Of-File: excpected to read an integer.";
    end;


    --
    --
    --
    procedure Read_Constraint(file : in Text_IO.File_Type; linecount : in out Integer;
            constrno : in Natural; cs : in out Constraints_t)
            -- rows: in Int; cols: in Int) return Constraints_t
    is
    begin
        while not Text_IO.End_Of_File(file) loop
        declare
            use Ada.Strings;

            line : constant String := Trim_Line(Text_IO.Get_Line(file));

            --spaces : constant Maps.Character_Set := Maps.To_Set(Space);
            spaces : constant Maps.Character_Set := Maps.To_Set(" " & Ada.Characters.Latin_1.HT);

            start   : Natural := Index(line, From => 1, Set => spaces, Test => Outside);
            ende    : Natural := 1;


        begin
            --Text_IO.Put_Line(Natural'Image(linecount) & ": `" & line & "'");

            if line'length /= 0 then

                -- Read Constraint-Vector, the left-hand-side.
                for j in 1 .. cs.cols loop
                    ende := Index(line, From => start, Set => spaces);
                    cs.matrix(constrno, j) := Double'Value(
                            Trim(line(start .. ende), Left => spaces, Right => spaces));
                    start := Index(line, From => ende, Set => spaces, Test => Outside);
                end loop;

                -- Read Compare.
                ende := Index(line, From => start, Set => spaces, Test => Inside);
                declare
                    cmp_str : constant String := Trim(line(start .. ende), Left => spaces, Right => spaces);
                begin
                    if cmp_str = "<="       then cs.cmp(constrno) := LESSEQ;
                    elsif cmp_str = ">="    then cs.cmp(constrno) := GREATEQ;
                    elsif cmp_str = "=="    then cs.cmp(constrno) := EQUAL;
                    else raise Constraint_Error with "Expected `<=', '>=' or `==', "
                                                & "but got `" & cmp_str & "'";
                    end if;
                end;
                start := Index(line, From => ende, Set => spaces, Test => Outside);

                -- Read the right-hand-side.
                ende := Index(line, From => start, Set => spaces, Test => Inside);
                if ende = 0 then
                    ende := line'Last;
                end if;
                cs.rhs(constrno) := Double'Value(
                        Trim(line(start .. ende), Left => spaces, Right => spaces));
                start := ende + 1;

                -- Check for trailing stuff.
                if line(start .. line'Last) /= "" then
                    raise Constraint_Error with "Found trailing stuff `" & line(start .. line'Last)
                                                & "' after finishing parsing the line";
                end if;

                linecount := linecount + 1; -- next line
                return;
            end if;
            -- else line'length = 0 then
            linecount := linecount + 1; -- next line
        end;
        end loop;

        raise Constraint_Error with "Premature End-Of-File: There are still missing "
                                    & "constraint-lines missing";
    end;

    --
    -- Trim_Line() gets a line and omits its comment (if exists) and trims the line.
    --
    -- Omitting the comment means deleting every from the first occurrence of "#".
    -- Afterwards triming the line (deleting the whitespaces) from both sides.
    --
    --
    function Trim_Line(line_org : in String) return String
    is
        use Ada.Strings;

        comment_index : constant Natural := Index(line_org, Pattern => "#");
        line_length   : constant Natural
            := (if comment_index = 0 then line_org'length else comment_index-1);

        line : constant String := Head(line_org, line_length);
    begin
        return Trim(line, Left => Maps.To_Set(Space), Right => Maps.To_Set(Space));
    end Trim_Line;

    --
    --
    --
    procedure Print_BinVector(x : in BinVector_Type)
    is
    begin
        Text_IO.Put("(");
        for i in x'First .. x'Last-1 loop
            --Bin_Text_IO.Put(x(i), Width => 1);
            Text_IO.Put(if(x(i) = ONE) then "1" else "0");
            Text_IO.Put(", ");
        end loop;
        -- Bin_Text_IO.Put(x(x'Last), Width => 1);
        Text_IO.Put(if(x(x'Last) = ONE) then "1" else "0");
        Text_IO.Put_Line(")");
    end Print_BinVector;

    --
    --
    --
    procedure Print_Constraints(cs : in Constraints_t)
    is
    begin
        Text_IO.Put_Line( Trim(Int'Image(Get_Cols(cs)), Ada.Strings.Left) );
        Text_IO.Put_Line( Trim(Int'Image(Get_Rows(cs)), Ada.Strings.Left) );

        for i in 1 .. cs.rows loop
            for j in 1 .. cs.cols loop
                Double_Text_IO.Put(cs.matrix(i,j), Fore => 1, Aft => 2, Exp => 0);
                Text_IO.Put(" ");
            end loop;

            case cs.cmp(i) is
                when LESSEQ => Text_IO.Put("<= ");
                when GREATEQ=> Text_IO.Put(">= ");
                when EQUAL  => Text_IO.Put("== ");
            end case;
            --Text_IO.Put( Double'Image(cs.rhs(i)) );
            Double_Text_IO.Put(cs.rhs(i), Fore => 1, Aft => 2, Exp => 0);
            Text_IO.Put_Line("");
        end loop;
    end Print_Constraints;

    --
    --
    --
    procedure inc(x : in out BinVector_Type) is
        overlap : Bin := FALSE;
    begin
        for i in x'Range loop
            if x(i) = ZERO then
                x(i) := ONE;
                overlap := FALSE;
            else
                x(i) := ZERO;
                overlap := TRUE;
            end if;
            exit when overlap = FALSE;
        end loop;

        --if overlap then
        --    raise Constraint_Error with "integer overflow detected";
        --end if;
    end;

    --
    --
    --
    procedure dec(x : in out BinVector_Type) is
        overlap : Bin := FALSE;
    begin
        for i in x'Range loop
            if x(i) = ONE then
                x(i) := ZERO;
                overlap := FALSE;
            else -- x(1) = ZERO
                x(i) := ONE;
                overlap := TRUE;
            end if;
            exit when overlap = FALSE;
        end loop;

        --if overlap then
        --    raise Constraint_Error with "integer underflow detected";
        --end if;
    end;

    --
    --
    --
    function andBin(a : in BinVector_Type; b : in BinVector_Type) return BinVector_Type
    is
        ret : BinVector_Type(a'Range);
    begin
        for i in a'Range loop
            ret(i) := a(i) and b(i);
        end loop;

        return ret;
    end;

    --
    --
    --
    function xorBin(a : in BinVector_Type; b : in BinVector_Type) return BinVector_Type
    is
        ret : BinVector_Type(a'Range);
    begin
        for i in a'Range loop
            ret(i) := a(i) xor b(i);
        end loop;

        return ret;
    end;

end Constraints;

