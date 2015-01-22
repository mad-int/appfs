with Ada.Assertions;           use Ada.Assertions;         -- Assert
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1; -- HT = Tabs
with Ada.Strings;              use Ada.Strings;            -- Both
-- with Ada.Strings.Fixed;        use Ada.Strings.Fixed;      -- Trim
with Ada.Strings.Maps;         use Ada.Strings.Maps;       -- Character_Set
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;            -- Reading file


-- pragma Optimize (time);
package body reader is

procedure Next_Field (
   Source : in out Unbounded_String;
   Set    : in     Maps.Character_Set;
   Field  : out    Unbounded_String
) is
   Nat : Natural;
   Pos : Positive;
begin
   Find_Token (Source => Source, Set => Set, First => Nat, Last => Pos, From => 1, Test => Outside);
   Field := To_Unbounded_String (To_String (Source) (Nat .. Pos));
   Delete (Source => Source, From => 1, Through => Pos);
   Trim (Source => Source, Side => Both);
end Next_Field;

procedure Process_File (
   filename : in  String;
   bp       : out Problem_Ptr;
   retcode  : out BP_RETCODE
) is
   File         : Ada.Text_IO.File_Type;
   Line_Count   : Natural := 0;
   Switch_Count : Natural := 0;
   n            : Integer range 1 .. MAX_MATRIX_SIZE := MAX_MATRIX_SIZE;
   m            : Integer range 1 .. MAX_MATRIX_SIZE := MAX_MATRIX_SIZE;
   equal        : Integer range 0 .. MAX_MATRIX_SIZE := 0;
   Coefficients : Double_Arr_Ptr;
   Rhs          : Double;
   Relation     : String := "  ";
begin
   -- Read file
   begin
      Open (File => File, Mode => In_File, Name => filename);
   exception
      when Ada.Text_IO.Name_Error =>
      Put_Line(File => Standard_Error, Item => "Can't open file " & filename);
      retcode := BP_NOFILE;
      return;
   end;

   while not End_Of_File (File) loop
      Line_Count := Line_Count + 1;
      declare
         I           : Natural;
         Line        : Unbounded_String       := To_Unbounded_String (Get_Line (File));
         Field       : Unbounded_String;
         TabAndSpace : constant Character_Set := To_Set (" " & HT);
         Comments    : constant String        := "#";
      begin
         -- Remove comments
         I := Index (Line, Comments);
         if I > 0 and then I <= Length (Line) then
            Line := Delete(Line, I, Length (Line));
         end if;

         -- Skip over leading space
         Trim (Source => Line, Side => Both);

         if Length (Line) /= 0 then
            Switch_Count := Switch_Count + 1;

            case Switch_Count is
               -- first line contains amount of columns (variables)
               when 1 =>
                  begin
                     Next_Field (Source => Line, Set => TabAndSpace, Field => Field);
                     n := Integer'Value (To_String(Field));
                  exception
                     -- dimension is wrong
                     when Constraint_Error =>
                     Put_Line ("Wrong input data for number of variables. Dimension of Matrix must lay in {1, ..., " & Integer'Image (MAX_MATRIX_SIZE) & "}");
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end;
                  -- return, when there is an additional entry in the line
                  if Length (Line) > 0 then
                     Put_Line ("Too many arguments for number of columns.");
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end if;
                  Coefficients := new Double_Arr (1 .. n);

               -- second line contains amount of rows (constraints)
               when 2 =>
                  begin
                     Next_Field (Source => Line, Set => TabAndSpace, Field => Field);
                     m := Integer'Value (To_String(Field));
                  exception
                     -- dimension is wrong
                     when Constraint_Error =>
                     Put_Line ("Wrong input data for number of constraints. Dimension of Matrix must lay in {1, ..., " & Integer'Image (MAX_MATRIX_SIZE) & "}");
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end;
                  -- return, when there is an additional entry in the line
                  if Length (Line) > 0 then
                     Put_Line ("Too many arguments for number of rows.");
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end if;
                  bp_new(m, n, bp);

               when others =>
                  declare
                     j : Integer := 1;
                  begin
                     -- while the current tok is not the relation sign, add coefficient to coefficient array
                     Next_Field (Source => Line, Set => TabAndSpace, Field => Field);
                     while Length (Line) > 0
                           and then To_String (Field) /= "<=" and then To_String (Field) /= "=<"
                           and then To_String (Field) /= ">=" and then To_String (Field) /= "=>"
                           and then To_String (Field) /= "==" and then To_String (Field) /= "=" loop
                        -- when j >= n, then we have too many coefficient entries in this line or no relation sign
                        if j > n then
                           Put_Line("Too many coefficients or no or wrong relation sign in line " & Integer'Image (Line_Count) & ".");
                           Free_Double_Array (Coefficients);
                           Close (File);
                           retcode := BP_INVALIDDATA;
                           return;
                        end if;

                        -- insert value in coefficient array
                        begin
                           Coefficients(j) := Double'Value (To_String (Field));
                        exception
                           -- when test not empty, then the data type of the coefficient was not correct
                           when Constraint_Error =>
                           Put_Line("Wrong input data in line " & Integer'Image (Line_Count) & ". Wrong type of coefficient (" & To_String(Field) & ") of variable " & Integer'Image (j) & ".");
                           Free_Double_Array (Coefficients);
                           Close (File);
                           retcode := BP_INVALIDDATA;
                           return;
                        end;
                        Next_Field (Source => Line, Set => TabAndSpace, Field => Field);
                        j := j + 1;
                     end loop;

                     -- when j < n, then we have not enough coefficient entries in this line
                     if j < n+1 then
                        Put_Line("Not enough entries in line " & Integer'Image (Line_Count) & ".");
                        Free_Double_Array (Coefficients);
                        Close (File);
                        retcode := BP_INVALIDDATA;
                        return;
                     end if;
                  end;

                  -- when tok is NULL, then there is no relation operator given in this line
                  if Length (Line) = 0 then
                     Put_Line("No relation operator in line " & Integer'Image (Line_Count) & ".");
                     Free_Double_Array (Coefficients);
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end if;

                  -- copy relation operator
                  if Length (Field) < 2 then
                     Assert (Field = "=");
                     Append (Field, " ");
                  end if;
                  Relation := To_String (Field);


                  -- get next token, which is right-hand side
                  Next_Field (Source => Line, Set => TabAndSpace, Field => Field);
                  begin
                  -- check for additional information after right-hand side
                     Rhs := Double'Value (To_String (Field));
                  exception
                     -- convert right-hand side and check whether the format is correct
                     when Constraint_Error =>
                     Put_Line("Wrong input data in line " & Integer'Image (Line_Count) & ". (wrong type of right-hand side)");
                     Free_Double_Array (Coefficients);
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end;

                  if Length (Line) > 0 then
                     Put_Line("Too many information after relation sign of line " & Integer'Image (Line_Count) & ".");
                     Free_Double_Array (Coefficients);
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end if;

                  -- add constraint(s) to the problem
                  if Relation = "<=" or else Relation = "=<" or else Relation = "= " or else Relation = "==" then
                     bp_put(bp, Coefficients, Rhs, retcode);
                     if retcode = BP_INFEASIBLE then
                        Free_Double_Array (Coefficients);
                        Close (File);
                        return;
                     end if;
                     Assert (retcode = BP_OKAY);
                  end if;

                  -- reverse constraint if sign is not "<="
                  if Relation = ">=" or else Relation = "=>" or else Relation = "= " or else Relation = "==" then
                     for k in 1 .. n loop
                        Coefficients(k) := Coefficients(k) * (-1.0);
                     end loop;
                     Rhs := Rhs * (-1.0);
                     bp_put (bp, Coefficients, Rhs, retcode);
                     if retcode = BP_INFEASIBLE then
                        Free_Double_Array (Coefficients);
                        Close (File);
                        return;
                     end if;
                     Assert (retcode = BP_OKAY);
                  end if;

                  -- increase m, if sign of original constraint was "="
                  if Relation = "= " or else Relation = "==" then
--                      m := m + 1;
                     equal := equal + 1;
                     Assert (equal <= m);
                  end if;

                  -- check whether we have too many rows
                  if bp_getM(bp) + bp_getRedundant(bp) > m + equal then
                     Put_Line("Too many rows.");
                     Free_Double_Array (Coefficients);
                     Close (File);
                     retcode := BP_INVALIDDATA;
                     return;
                  end if;

            end case;
         end if;
      end;
   end loop;
--    close file
   Close (File);

   if Switch_Count < 2 + m then
      Put_Line ("File contains corrupted data.");
      retcode := BP_INVALIDDATA;
      return;
   end if;

   -- Free_Double_Array  array
   Free_Double_Array (Coefficients);

   -- check whether we have not enough rows
   if bp_getM(bp) + bp_getRedundant(bp) < m + equal then
      Put_Line ("Not enough rows.");
      retcode := BP_INVALIDDATA;
      return;
   end if;


   retcode := BP_OKAY;
   return;
end Process_File;

end reader;
