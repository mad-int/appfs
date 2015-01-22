with Ada.Assertions;             use Ada.Assertions;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Command_line;           use Ada.Command_line;       -- Command line arguments
with Ada.Text_IO;                use Ada.Text_IO;            -- Put_Line
with binaryProgram;              use binaryProgram;
with misc;                       use misc;
with reader;                     use reader;

-- pragma Optimize (time);
procedure Main is
   bp          : Problem_Ptr;       -- problem data
   start       : Time;              -- clock to meassure running time
   enumeration : Time;              -- clock to meassure running time
   retcode     : BP_RETCODE;
begin
   -- start clock
   start := Clock;

   -- check whether there are enough arguments given
   if Argument_Count /= 1 then
      Put ("Usage: ");
      Put (Command_Name);
      Put (" filename");
      Set_Exit_Status (Failure);
      return;
   end if;

   -- read problem data from file and creates problem data
   -- returns amount of lines
   -- returns -1 when there was a reading error
   -- returns -2 when the problem is infeasible

   Process_File (Argument(1), bp, retcode);
   if retcode /= BP_OKAY then
      if bp /= NULL then
         Free_Problem (bp);
      end if;
      if retcode = BP_INFEASIBLE then
         Put_Line("0 solutions found.");
         Put_Line("Problem is infeasible.");
      else
         Set_Exit_Status (Failure);
      end if;
      Put("Running time: ");
      Put(Duration'Image (GET_SEC(start, Clock)));
      Put_Line(" seconds.");
      return;
   end if;
   Assert (retcode = BP_OKAY);

   enumeration := Clock;
   -- solve the problem
   solveBT(bp, retcode);
--    solveBP(bp, retcode);
   Assert (retcode = BP_OKAY or else retcode = BP_INFEASIBLE);

   if retcode = BP_INFEASIBLE then
      Put_Line("Problem is infeasible.");
   end if;
   Put("Enumeration time: ");
   Put(Duration'Image (GET_SEC(enumeration, Clock)));
   Put_Line(" seconds.");

   -- free problem data
   bp_free(bp);

   Put("Running time: ");
   Put(Duration'Image (GET_SEC(start, Clock)));
   Put_Line(" seconds.");

   return;
end Main;
