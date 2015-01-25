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
        x : BinVector_Type(1 .. Ab.cols) := (others => 0);

        n   : BinVector_Type(1 .. 4) := (1 => 1, others => 0);
        negn: BinVector_Type(1 .. 4) := (others => 1);

        zero: constant BinVector_Type(1 .. 4) := (others => 0);

    begin
        Print_Constraints(Ab);
        Print_BinVector(x);

        while negn /= zero loop
            -- updatemask = n & negn
            -- x ^= updatemask
            x := xorBin(x, andBin(n, negn));

            if is_Feasible(Ab, x) then
                Print_BinVector(x);
                solutions := solutions + 1;
            end if;

            inc(n);
            dec(negn);
        end loop;

        Text_IO.Put_Line(Natural'Image(solutions) & " feasible vectors found.");
    end;
end ex9;


