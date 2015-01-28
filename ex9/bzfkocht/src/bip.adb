with Ada.Text_IO;
with Interfaces;
with Ada.Characters.Latin_1;
with GNAT.String_Split;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Verbosity;

use Ada;
use Interfaces;
use Ada.Text_IO;
use Ada.Characters;
use GNAT;
use Verbosity;

-- @summary Binary Integer Program package
-- @description
-- Allows to read a BIP from a file and to enumerate all feasible solutions.
package body BIP is

   function is_valid(inst : Instance) return Boolean is
   begin
      if inst.acr = null then
         if inst.equs /= 0 then
            return false;
         end if;
      else
         if inst.equs > inst.acr'Last(DimRow)then
            return false;
         end if;
      end if;
      return true;
   end;

   procedure read (inst : in out Instance; filename : String) is

      type State_T      is ( READ_COLS, READ_ROWS, READ_COEF );
      type Sense_T      is ( LE, GE, EQ );

      input_file  : File_Type;
      line_count  : Natural   := 0;
      state       : State_T   := READ_COLS;
      current_row : Natural   := 1;
      rows        : Row_Count := 0;
      cols        : Col_Count := 0;
      sense       : array (1 .. Max_Rows) of Sense_T;

      procedure matrix_free is new Ada.Unchecked_Deallocation(Matrix, Matrix_Ptr);

      procedure preprocess is

         subtype Row_Index is Row_Count range 1 .. inst.acr'Last(DimRow);
         subtype Col_Index is Col_Count range 1 .. inst.acr'Last(DimCol);

         acr_new : constant Matrix_Ptr := new Matrix(1 .. cols, 1 .. rows);
         rhs_new : Row_Vector;

      begin
         current_row := 0;

         -- copy equations to col order and remove GE
         for r in Row_Index loop
            if sense(r) = EQ then
               current_row := current_row + 1;
               rhs_new(current_row) := inst.rhs(r);

               for c in Col_Index loop
                  acr_new(c, current_row) := inst.acr(c, r);
               end loop;
               inst.equs := inst.equs + 1;
            end if;
         end loop;

         -- copy rest
         for r in Row_Index loop
            case sense(r) is
            when LE =>
               current_row := current_row + 1;
               rhs_new(current_row) := inst.rhs(r);

               for c in Col_Index loop
                  acr_new(c, Current_Row) := inst.acr(c, r);
               end loop;
            when GE =>
               current_row := current_row + 1;
               rhs_new(current_row) := -inst.rhs(r);

               for c in Col_Index loop
                  acr_new(c, current_row) := -inst.acr(c, r);
               end loop;
            when EQ => null;
            end case;
         end loop;
         pragma Assert(current_row = inst.acr'Last(DimRow));

         matrix_free(inst.acr);
         inst.acr := acr_new;
         inst.rhs := rhs_new;
      end preprocess;


      procedure scale_to_integer is

         package Math_Functions is new Ada.Numerics.Generic_Elementary_Functions(Real);
         use Math_Functions;

         subtype Row_Index is Row_Count range 1 .. inst.acr'Last(DimRow);
         subtype Col_Index is Col_Count range 1 .. inst.acr'Last(DimCol);

         scale         : Real;
         fraction      : Real;
         a_abs         : Real;
         row_magnitude : Natural;
         magnitude     : Natural;

      begin
         for r in Row_Index loop
            row_magnitude := 0;

            -- Compute scaling factor to remove fractions
            for c in Col_Index loop
               a_abs    := abs(inst.acr(c,r));
               fraction := a_abs - Real'Floor(a_abs);

               if fraction > 0.0 then
                  if inst.level >= Verbosity.Debug then
                     put_line("[" & Row_Count'Image(r) & "," & Col_Count'Image(c) & "] " & Real'Image(fraction));
                  end if;

                  magnitude := Natural(Real'Ceiling(-log(Base => 10.0, X => fraction)));

                  if magnitude > row_magnitude then
                     row_magnitude := magnitude;
                  end if;
               end if;
            end loop;

            -- If necessary, scale row to make all coeficients integer
            if row_magnitude > 0 then
               scale := 10.0**row_magnitude;

               for c in Col_Index loop
                  inst.acr(c,r) := inst.acr(c,r) * scale;
               end loop;

               inst.rhs(r) := inst.rhs(r) * scale;

               if inst.level >= Verbosity.Verbose then
                  put_line("Reordered row " & Row_Count'Image(r) & " has been scaled with factor " & Real'Image(scale));
               end if;
            end if;
         end loop;
      end scale_to_integer;

   begin
      if inst.level >= Verbosity.Normal then
         put_line("Reading " & filename);
      end if;
      open(input_file, In_File, filename);

      while not end_of_file(input_file) loop
         declare
            input_line : constant String := get_line(input_file);
            fields     : String_Split.Slice_Set;
            sep_chars  : constant String := " " & Latin_1.HT;
         begin
            line_count := line_count + 1;
            String_Split.create(s          => fields,
                                from       => trim(input_line, Left),
                                separators => sep_chars,
                                mode       => String_Split.Multiple);

            if inst.level >= Verbosity.Debug then
               put_line(input_line);
               for i in 1 .. String_Split.slice_count(fields) loop
                  put_line("Field 1: /" & String_Split.slice(fields, 1) & "/");
               end loop;
            end if;

            case State is
               when READ_COLS =>
                  cols  := Integer'Value(String_Split.slice(fields, 1));
                  state := READ_ROWS;
               when READ_ROWS =>
                  rows      := Integer'Value(String_Split.slice(fields, 1));
                  pragma assert(rows > 0 and cols > 0);
                  matrix_free(inst.acr);
                  inst.acr := new Matrix(1 .. cols, 1 .. rows);
                  state    := READ_COEF;
               when READ_COEF =>
                  if (Natural(String_Split.slice_count(fields)) < cols + 2) then
                     raise Constraint_Error with "Not enough fields";
                  end if;
                  for c in 1 .. cols loop
                     inst.acr(c, current_row) := Real'Value(String_Split.slice(fields, String_Split.slice_number(Natural(c))));
                  end loop;
                  declare
                     sense_field : constant String := String_Split.slice(fields, String_Split.slice_number(cols + 1));
                  begin
                     if sense_field = "<=" then
                        sense(current_row) := LE;
                     elsif  sense_field = ">=" then
                        sense(current_row) := GE;
                     elsif sense_field = "==" or sense_field = "=" then
                        sense(current_row) := EQ;
                     else
                        raise Constraint_Error with "Line " & Natural'Image(line_count) & " Illegal row sense" & sense_field;
                     end if;
                  end;
                  inst.rhs(current_row) := Real'Value(String_Split.slice(fields, String_Split.slice_number(cols + 2)));
                  current_row := current_row + 1;
            end case;
         end;
      end loop;
      if inst.level >= Verbosity.Normal then
         put_line("Read " & Natural'Image(rows) & " rows, " & Natural'Image(cols) & " cols");
      end if;

      close(input_file);

      if current_row - 1 /= rows then
         raise Constraint_Error with "Not enough Rows";
      end if;

      preprocess;
      scale_to_integer;
   end read;

   -- print Instance of BIP.
   -- @param Inst Instance of BIP
   --
   procedure print(Inst : Instance) is

      subtype Row_Index is Row_Count range 1 .. inst.acr'Last(DimRow);
      subtype Col_Index is Col_Count range 1 .. inst.acr'Last(DimCol);

      package Flt_IO is new Ada.Text_IO.Float_IO(Real); use Flt_IO;

   begin
      for r in Row_Index loop
         for c in Col_Index loop
            put(inst.acr(c, r), 8, 3, 0);
         end loop;
         if r <= inst.equs then
            put(" == ");
         else
            put(" <= ");
         end if ;
         put(inst.rhs(r), 8, 3, 0);
         new_line;
      end loop;
   end print;

   procedure set_verbosity(Inst : in out Instance; vlevel : Verbosity.Level) is
   begin
      inst.level := vlevel;
   end set_verbosity;

   function get_verbosity(Inst : Instance) return Verbosity.Level is
   begin
      return inst.level;
   end get_verbosity;

   function get_cols(Inst : Instance) return Natural is
   begin
      return inst.acr'Last(DimCol);
   end;

   function enumerate(
                      inst            : Instance;
                      report_solution : Report_Proc;
                      prefix_mask     : Unsigned_32 := 0;
                      prefix_length   : Natural := 0)
                      return Count32
   is
      subtype idx32 is Unsigned_32 range 0 .. 31;

      -- http://en.wikipedia.org/wiki/De_Bruijn_sequence
      index32 : constant array (idx32) of Natural :=
        (
           1,  2, 29,  3, 30, 15, 25, 4, 31, 23, 21, 16, 26, 18,  5,  9,
          32, 28, 14, 24, 22, 20, 17, 8, 27, 13, 19,  7, 12,  6, 11, 10
         );
      cols       : constant Col_Count   := inst.acr'Last(DimCol);
      rows       : constant Row_Count   := inst.acr'Last(DimRow);
      equs       : constant Row_Count   := inst.equs;
      debruijn32 : constant Unsigned_32 := 16#077CB531#;

      subtype Row_Index         is Row_Count   range 1 .. rows;
      subtype Col_Index         is Col_Count   range 1 .. cols - prefix_length;
      subtype Enumeration_Range is Unsigned_32 range 1 .. 2**(cols - prefix_length)- 1;

      res            : array (Row_Index) of Real;
      x              : Unsigned_32 := prefix_mask;
      updatemask     : Unsigned_32;
      colidx         : Col_Index;
      solution_count : Count32     := 0;

      procedure check_feasibility with inline is
      begin
         for i in 1 .. Equs loop
            if res(i) /= 0.0 then
               return;
            end if;
         end loop;

         for i in Equs + 1 .. Rows loop
            if res(i) > 0.0 then
               return;
            end if;
         end loop;

         report_solution(inst, x);
         solution_count := solution_count + 1;
      end check_feasibility;

   begin
      put_line("Start Enumerating with prefix " & Unsigned_32'Image(prefix_mask));

      for i in Row_Index loop
         res(i) := -inst.rhs(i);
      end loop;

      -- adjust prefix
      for c in cols - prefix_length + 1 .. cols loop
         if (prefix_mask and 2**(c - 1)) /= 0 then
            for r in Row_Index loop
               res(r) := res(r) + inst.acr(c, r);
            end loop;
         end if;
      end loop;

      -- Check whether 0 is a feasible solution
      check_feasibility;

      -- Starting with x = 0000, n = 0001, negn = 1111, the algorithm
      -- enumerates all x vectors by always doing only one flip, which means
      -- we only have to update the residium for a single column.
      -- This is done as follows:
      --   1. updatemask = n & -n (always only one bit set!)
      --   2. colidx = bit number which is set in updatemask
      --   2. xor-bitflip: x ^= updatemask
      --   3. add or substract col(colidx) from activities and check feasibility
      -- Thereby, the x vectors are scanned in the following order:
      --   0000, 0001, 0011, 0010, 0110, 0111, 0101, 0100,
      --   1100, 1101, 1111, 1110, 1010, 1011, 1001, 1000
      for n in Enumeration_Range loop
         --https://en.wikipedia.org/wiki/Gray_code
         updatemask := n and -n;
         x          := x xor updatemask;
         colidx     := index32(shift_right(updatemask * debruijn32, 27));

         if (x and updatemask) /= 0 then
            for k in Row_Index loop
               res(k) := res(k) + inst.acr(colidx, k);
            end loop;
         else -- bit changed from 1 to 0
            for k in Row_Index loop
               res(k) := res(k) - inst.acr(colidx, k);
            end loop;
         end if;
         check_feasibility;
      end loop;
      put_line("End Enumerating with prefix " & Unsigned_32'Image(prefix_mask));

      return Solution_Count;
   end enumerate;
end BIP;
