with Ada.Command_Line;
-- with Ada.Exceptions;
with Ada.Text_IO; -- Put_Line()

with Constraints;

use Ada;

procedure ex9 is
    pragma Assertion_Policy(Check);
    use Constraints;

begin
    if Command_Line.Argument_Count < 1 then
        raise Constraint_Error with "Usage: " & Command_Line.Command_Name & " <filename>";
    end if;

    declare
        solutions : Natural := 0;

        Ab : constant Constraints_t := Read_Constraints(Command_Line.Argument(1));
        x : BinVector_Type(1 .. Ab.cols) := (others => ZERO);

        n   : BinVector_Type(1 .. Ab.cols) := (1 => ONE, others => ZERO);
        negn: BinVector_Type(1 .. Ab.cols) := (others => ONE);
        updatemask : BinVector_Type(1 .. Ab.cols) := (others => ONE);
        zeroVector: constant BinVector_Type(1 .. Ab.cols) := (others => ZERO);

        actualLhs : Vector_Type(1 .. Ab.rows) :=  (others => 0.0);

    begin
        Print_Constraints(Ab);

        -- x is 0-Vector, check it separatly.
        -- There are faster ways to do this.
        if is_Feasible(Ab, x) then
            Print_BinVector(x);
        end if;

        while negn /= zeroVector loop
            -- updatemask = n & negn
            -- x ^= updatemask
            updatemask := andBin(n, negn);
            x := xorBin(x, updatemask);

            if is_Feasible_BitFlip(Ab, x, updatemask, actualLhs) then
                Print_BinVector(x);
                solutions := solutions + 1;
            end if;

            inc(n);
            dec(negn);
        end loop;

        Text_IO.Put_Line(Natural'Image(solutions) & " feasible vectors found.");
    end;
end ex9;


