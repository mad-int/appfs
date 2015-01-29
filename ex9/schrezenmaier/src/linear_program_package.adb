-- linear_program_package.adb

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings;
with ada.Strings.Maps;
with Ada.Characters.Latin_1;

with GNAT.String_Split;
with GNAT.OS_Lib;

with Interfaces; use Interfaces;


package body Linear_Program_Package is

   EPS : constant Float := 1.0e-7;

   function New_LP (Vars, Constrs : Natural) return Linear_Program is
      Result : Linear_Program (Vars, Constrs);
   begin
      return Result;
   end New_LP;

   procedure Print (Self : Linear_Program)
   is
      use Ada.Text_IO;
   begin
      for I in Self.A'Range (1) loop
         for J in Self.A'Range (2) loop
            Put (Float'Image (Self.A (I, J)) & " ");
         end loop;
         if I <= Self.Last_Inequality then
            Put ("<= ");
         else
            Put ("== ");
         end if;
         Put (Float'Image (Self.Rhs (I)));
         New_Line;
      end loop;
   end Print;

   procedure Add_Constraint (Self : in out Linear_Program;
                             Row  : Vector;
                             Sign : Comp_Sign;
                             Rhs  : Float)
   is
   begin
      case Sign is
         when LEQ =>
            Self.Last_Inequality := Self.Last_Inequality + 1;
            for J in Self.A'Range (2) loop
               Self.A (Self.Last_Inequality, J) := Row (J);
            end loop;
            Self.Rhs (Self.Last_Inequality) := Rhs;
         when GEQ =>
            Self.Last_Inequality := Self.Last_Inequality + 1;
            for J in Self.A'Range (2) loop
               Self.A (Self.Last_Inequality, J) := - Row (J);
            end loop;
            Self.Rhs (Self.Last_Inequality) := - Rhs;
         when EQ =>
            Self.First_Equality := Self.First_Equality - 1;
            for J in Self.A'Range (2) loop
               Self.A (Self.First_Equality, J) := Row (J);
            end loop;
            Self.Rhs (Self.First_Equality) := Rhs;
      end case;
   end Add_Constraint;

   function Remove_Comments (Source: String) return String
   is
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use Ada.Strings.Maps;
      use Ada.Characters;
      Seps : Character_Set := To_Set (' ') or To_Set (Latin_1.HT);
      Ind  : Natural       := Index (Source => Source, Pattern => "#", From => 1);
      Copy : String        := Source;
   begin
      if Ind > 0 then
         Delete (Source => Copy, From => Ind, Through => Source'Length);
      end if;
      return Trim (Source => Copy, Left => Seps, Right => Seps);
   end Remove_Comments;

   procedure Parse_Error (File_Name  : String;
                          Line_Count : Positive;
                          Message : String)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "Parsing error in " &
                                File_Name &
                                " (line " &
                                Positive'Image( Line_Count) &
                                "): " &
                                Message);
      Gnat.OS_Lib.OS_Exit (Integer (Ada.Command_Line.Failure));
   end;

   function Read_LP_From_File (File_Name : String) return Linear_Program
   is
      use Ada.Text_IO;
      use Ada.Characters;
      use GNAT;
      type Parser_Status is (Read_Vars, Read_Constrs, Read_Matrix);
      File       : File_Type;
      Line_Count : Natural                := 0;
      Result_LP  : access Linear_Program;
      Status     : Parser_Status          := Read_Vars;
      Seps       : constant String        := " " & Latin_1.HT;
      Vars       : Positive;
      Constrs    : Positive;
   begin
      begin
         Open (File => File, Mode => In_File, Name => File_Name);
      exception
         when others =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Can't open file " & File_Name);
            Gnat.OS_Lib.OS_Exit (Integer (Ada.Command_Line.Failure));
      end;
      while not End_Of_File (File) loop
         <<Continue>>
         declare
            Line        : String := Remove_Comments (Get_Line (File));
            Subs        : String_Split.Slice_Set;
            Slice_Count : String_Split.Slice_Number;
         begin
            Line_Count := Line_Count + 1;
            if Line'Length = 0 then
               goto Continue;
            end if;
            String_Split.Create (S          => Subs,
                                 From       => Line,
                                 Separators => Seps,
                                 Mode       => String_Split.Multiple);
            Slice_Count := String_Split.Slice_Count (Subs);
            case Status is
               when Read_Vars =>
                  if Integer (Slice_Count) > 1 then
                     Parse_Error (File_Name  => File_Name,
                                  Line_Count => Line_Count,
                                  Message    => "too many entries in line");
                  end if;
                  for I in 1 .. Slice_Count loop
                     declare
                        Sub : constant String := String_Split.Slice (Subs, I);
                     begin
                        Vars := Positive'Value (Sub);
                     exception
                        when others =>
                           Parse_Error (File_Name  => File_Name,
                                        Line_Count => Line_Count,
                                        Message    => "wrong format for the number of variables");
                     end;
                  end loop;
                  Status := Read_Constrs;
               when Read_Constrs =>
                  if Integer (Slice_Count) > 1 then
                     Parse_Error (File_Name  => File_Name,
                                  Line_Count => Line_Count,
                                  Message    => "too many entries in line");
                  end if;
                  for I in 1 .. Slice_Count loop
                     declare
                        Sub : constant String := String_Split.Slice (Subs, I);
                     begin
                        Constrs := Positive'Value (Sub);
                     exception
                        when others =>
                           Parse_Error (File_Name  => File_Name,
                                        Line_Count => Line_Count,
                                        Message    => "wrong format for the number of variables");
                     end;
                  end loop;
                  Result_LP := new Linear_Program (Vars, Constrs);
                  Status := Read_Matrix;
               when Read_Matrix =>
                  declare
                     Row  : Vector (1..Vars);
                     Sign : Comp_Sign;
                     Rhs  : Float;
                  begin
                     if Integer (Slice_Count) /= Vars + 2 then
                        Parse_Error (File_Name  => File_Name,
                                     Line_Count => Line_Count,
                                     Message    => "wrong number of entries in line");
                     end if;
                     for I in 1 .. Slice_Count loop
                        declare
                           Sub : constant String := String_Split.Slice (Subs, I);
                        begin
                           if Integer (I) <= Vars then
                              Row (Integer (I)) := Float'Value (Sub);
                           elsif Integer (I) = Vars + 1 then
                              if Sub = "<=" then
                                 Sign := LEQ;
                              elsif Sub = "==" then
                                 Sign := EQ;
                              elsif Sub = ">=" then
                                 Sign := GEQ;
                              else
                                 Parse_Error (File_Name  => File_Name,
                                              Line_Count => Line_Count,
                                              Message    => "no valid comparison sign");
                              end if;
                           else
                              Rhs := Float'Value (Sub);
                           end if;
                        exception
                           when others =>
                              Parse_Error (File_Name  => File_Name,
                                           Line_Count => Line_Count,
                                           Message    => "wrong format for a matrix entry or the rhs");
                        end;
                     end loop;
                     Add_Constraint (Result_LP.all, Row, Sign, Rhs);
                  end;
            end case;
         end;
      end loop;
      Close (File);
      return Result_LP.all;
   end Read_LP_From_File;
   
   function Count_Feasible_Binary (Self : Linear_Program) return Int_64
   is
      Index      : Int_64                    := 0;
      Eval       : Vector (Self.A'Range (1)) := (others => 0.0);
      Solutions  : Int_64                    := 0;
   begin
      loop
         declare
            Is_Feasible       : Boolean := True;
            Changed_Bit      : Position_Type;
            Is_Positive_Switch : Boolean;
         begin
            for J in 1 .. Self.Last_Inequality loop
               if Eval (J) > Self.Rhs (J) + EPS then
                  Is_Feasible := False;
               end if;
               exit when not Is_Feasible;
            end loop;
            for J in Self.First_Equality .. Self.Constrs loop
               exit when not Is_Feasible;
               if (Eval (J) > Self.Rhs (J) + EPS) or (Eval (J) < Self.Rhs (J) - EPS) then
                  Is_Feasible := False;
               end if;
            end loop;
            if Is_Feasible then
               Solutions := Solutions + 1;
            end if;
            Index := Index + 1;
            Get_Changed_Bit (Index, Changed_Bit, Is_Positive_Switch);
            exit when Integer (Changed_Bit) >= Self.Vars;
            if Is_Positive_Switch then
               for J in Self.A'Range (1) loop
                  Eval (J) := Eval (J) + Self.A (J, Integer (Changed_Bit) + 1);
               end loop;
            else
               for J in Self.A'Range (1) loop
                  Eval (J) := Eval (J) - Self.A (J, Integer (Changed_Bit) + 1);
               end loop;
            end if;
         end;
      end loop;
      return Solutions;
   end Count_Feasible_Binary;

end Linear_Program_Package;
