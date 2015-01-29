-- TK 23 Jan 2015
-- Enumerating a BIP in parallel
--
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Real_Time;    use Ada.Real_Time;
with Ada.Unchecked_Deallocation;
with Interfaces;
-- with Ada.Execution_Time; use Ada.Execution_Time;

with BIP;
with Report;
with Verbosity;        use Verbosity;
use Ada;

-- main program
--
procedure Ex9 is

   type Vecs_per_s is digits 14;

   level_no : constant array (Character'Pos('0') .. Character'Pos('4')) of Verbosity.Level :=
     (Verbosity.Quiet, Verbosity.Normal, Verbosity.Verbose, Verbosity.Chatter, Verbosity.Debug);

   clock_start : Time;
   elapsed     : Time_Span;
   b           : BIP.Instance;
   count       : BIP.Count32 := 0;
   speed       : Vecs_per_s;
   prefix_bits : Natural := 0;

   package Flt_IO is new Ada.Text_IO.Float_IO(Vecs_per_s); use Flt_IO;

   procedure generate_tasks(prefix_length : Natural)
   is
      task type Enum_Task(pmask : Interfaces.Unsigned_32);
      type      Enum_Task_Ptr is access Enum_Task;
      subtype   prefix_range  is Natural range 0 .. 2**prefix_length - 1;

      procedure task_free is new Unchecked_Deallocation(Enum_Task, Enum_Task_Ptr);

      task body Enum_Task is
      begin
         count := count + BIP.enumerate(inst => B, report_solution => Report.Display.Solution'Access,
                                        prefix_mask => pmask, prefix_length => prefix_length);
      end Enum_Task;

      cols        : constant Natural := BIP.get_cols(B);
      prefix_mask : Interfaces.Unsigned_32;
      etask_ptr   : array (prefix_range) of Enum_Task_Ptr;

   begin
      for i in prefix_range loop

         -- we want to set a mask on the top most bits.
         -- So we generate the bits and then shift them to the top (left).
         prefix_mask := Interfaces.Shift_Left(Value  => Interfaces.Unsigned_32(i), Amount => cols - prefix_length);

         -- Start a new task for every mask
         etask_ptr(i) := new Enum_Task(prefix_mask);
      end loop;
      -- deallocate the tasks
      for i in prefix_range loop
         task_free(etask_ptr(i));
      end loop;
   end generate_tasks;


begin
   -- Check if we have enough arguments.
   -- We need at least a filename.
   if Command_Line.Argument_Count < 1 then
      raise Constraint_Error with "Not enough arguments";
   end if;

   -- for all arguments apart from the filename, check if
   -- they have the right form (minus letter number) and
   -- if we know the letter.
   for i in 1 .. Command_Line.Argument_Count - 1 loop
      if Command_Line.Argument(i)(1) /= '-' then
         raise Constraint_Error with "Unkown commandline parameter";
      end if;
      case Command_Line.Argument(i)(2) is
         when 'p' =>
            prefix_bits := Natural'Value(Command_Line.Argument(i)(3..Command_Line.Argument(i)'Length));
         when 'v' =>
            BIP.set_verbosity(B, level_no(Character'Pos(Command_Line.Argument(i)(3))));
         when others =>
            raise Constraint_Error with "Unkown commandline parameter";
      end case;
   end loop;

   BIP.read(b, Command_Line.Argument(Command_Line.Argument_Count));

   if BIP.get_verbosity(b) >= Verbosity.Verbose then
      BIP.print(b);
   end if;

   if BIP.get_verbosity(b) >= Verbosity.Normal then
      Text_IO.put_line("Prefix Bits: " & Natural'Image(prefix_bits));
   end if;

   Clock_Start := Clock;

   -- Shitty trick to be able to compare parallel to sequential.
   -- if the prefix is larger than columns - 1 we do no prefix at
   -- all. This is bad. Correct behaviour would be to raise an exception.
   -- One could do this for prefix length 0, but then we could not compare
   -- the overhead.
   if prefix_bits < BIP.get_cols(b) then
      generate_tasks(prefix_bits);
   else
      count := BIP.enumerate(inst => b, report_solution => Report.Display.Solution'Access);
   end if;
   elapsed := clock - clock_Start;
   speed := (Vecs_per_s(2**BIP.get_cols(B)) / Vecs_per_s(To_Duration(elapsed)) / 1000.0);

   if BIP.get_verbosity(B) >= Verbosity.Normal then
      Text_IO.put("Checked " & Natural'Image(2**BIP.get_cols(b)) & " vectors ");
      Text_IO.put(" in " & Duration'Image(To_Duration(elapsed)) & " s = ");
      put(speed, 8, 3, 0);
      Text_IO.put_line(" kvecs/s");
      Text_IO.put_line("Found " & BIP.Count32'Image(count) & " feasible solutions");
   end if;
end Ex9;
