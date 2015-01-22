with Ada.Text_IO; use Ada.Text_IO; -- file processing
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1; -- define 'NUL' and 'Space'
with Ada.Strings; use Ada.Strings; -- e.g. for Membership definition (Inside,Outside)
with Ada.Strings.Maps; -- defines Character_Set, Character_Range and To_Set
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded; -- defines Index for fixed length strings
with Ada.Exceptions; use Ada.Exceptions; -- exception handling
-- local
with numb; use numb;

package body Read_Bip is

   procedure Read_Token (line : in Unbounded_String; linePos : in out Natural; token : out Unbounded_String) is
      wsRange    : Maps.Character_Range := (NUL, ' ');
      wsSet      : Maps.Character_Set := Maps.To_Set (wsRange);
      First              : Positive;      -- first position of token to search (is zero if token has not been found)
      Last               : Natural;       --  last position of token to search
   begin
      Find_Token (Source => line, From => linePos, Set => wsSet, Test => Outside, First => First, Last => Last);
      token := Unbounded_Slice (line, First, Last);
      if Last /= 0 then
         linePos := Last + 1;
      else
         linePos := 0;
      end if;
   end Read_Token;

   function Read_Bip_From_File (fileName : String; cap : out Constr_Array_Ptr) return EXIT_STATUS is

      type READ_MODE is (READ_NVARS, READ_NCONSTR, READ_CONSTR, DONE);

      mode                  : READ_MODE := READ_NVARS;
      status                : EXIT_STATUS := NORMAL;
      file                  : File_Type;
      nLines                : Positive := 1;
      nVars                 : Positive range 1 .. 32;
      nConstr               : Positive;
      nReadConstr           : Natural := 0;
      nUsedConstr           : Natural := 0;
      Read_Error            : exception;

   begin

      Open (File => file, Mode => In_File, Name => fileName);

      File_Loop :
      while not End_Of_File (file) loop

         declare
            line                  : Unbounded_String := To_Unbounded_String (Get_Line (file));
            token                 : Unbounded_String;
            hashPos               : Natural := Index (Source => line, Pattern => "#", From => 1); -- position of the first '#' in the line
            linePos               : Natural := 1; --  current position of line token
         begin

            if (hashPos /= 0 ) then
               line := Unbounded_Slice (line, 1 , hashPos - 1);
            end if;
            line := Trim (Source => line, Side => Both);

            if Length (line) /= 0 then

               -- search from previous last value +1 to next token surrounded by whitespace
               Read_Token (line => line, linePos => linePos, token => token);

               case mode is

               when READ_NVARS =>
                  begin
                     nVars := Integer'Value (To_String (token));
                  exception
                     when Constraint_Error =>
                        raise Read_Error with "Illegal number of variables definition! Should be one positive integer smaller then 33, but is '" & To_String (token) & "'.";
                  end;
                  if linePos /= Length (line)+1 then
                     raise Read_Error with "Illegal number of variables definition! Should be only one whitespace separated element, but there are more: '" & To_String (line) & "'.";
                  end if;
                  mode := READ_NCONSTR;

               when READ_NCONSTR =>
                  begin
                     nConstr := Integer'Value (To_String (token));
                  exception
                     when Constraint_Error =>
                        raise Read_Error with "Illegal number of constraints definition! Should be one positive integer, but is '" & To_String (token) & "'.";
                  end;
                  if linePos /= Length (line)+1 then
                     raise Read_Error with "Illegal number of constraints definition! Should be only one whitespace separated element, but there are more: '" & To_String (line) & "'.";
                  end if;
                  mode := READ_CONSTR;
                  cap := create_Constr_Array (Dimension => nVars, nConstrs => nConstr);
               when READ_CONSTR =>

                  declare
                     type CONSTR_MODE is (READ_COEFFS, READ_TYPE, READ_RHS, DONE);

                     c_mode            : CONSTR_MODE := READ_COEFFS;
                     cV                : Coeff_Vector (1 .. nVars);
                     nCoeffs           : Positive := 1;
                     cType             : Constr_Type;
                     currCoeff, rhs    : Number;
                     constr            : Constraint_Ptr;
                  begin
                     Token_Loop :
                     while linePos /= 0 loop
                        case c_mode is
                           when READ_COEFFS =>
                              begin
                                 currCoeff := Parse_String_To_Number (To_String (token));
                              exception
                                 when Constraint_Error =>
                                    raise Read_Error with "Illegal definition of constraint" & Integer'Image (nReadConstr) & ": Coefficient" & Integer'Image (nCoeffs) & " is no " & number_Name & " value, but is '" & To_String (token) & "'.";
                              end;
                              cV (nCoeffs) := currCoeff;
                              nCoeffs := nCoeffs + 1;
                              if nCoeffs > cV'Last then
                                 c_mode := READ_TYPE;
                              end if;
                           when READ_TYPE =>
                              if "<=" = token then
                                 cType := LESS_EQUAL;
                              elsif "==" = token then
                                 cType := EQUAL;
                              elsif ">=" = token then
                                 cType := GREATER_EQUAL;
                              else
                                 raise Read_Error with "Illegal definition of constraint" & Integer'Image (nReadConstr) & ": Unknown relation sign given! Expected '>=', '==' or '<=', but given symbol is '" & To_String (token) & "'.";
                              end if;
                              c_mode := READ_RHS;
                           when READ_RHS =>
                              begin
                                 rhs := Parse_String_To_Number  (To_String (token));
                              exception
                                 when Constraint_Error =>
                                    raise Read_Error with "Illegal definition of constraint" & Integer'Image (nReadConstr) & ": RHS is no " & number_Name & " value, but is '" & To_String (token) & "'.";
                              end;
                              c_mode := DONE;
                           when DONE =>
                              raise Read_Error with "Illegal definition of constraint" & Integer'Image (nReadConstr) & ": Multiple RHSs defined!";
                        end case; -- CONSTR_MODE cases
                        Read_Token (line => line, linePos => linePos, token => token);
                     end loop Token_Loop;

                     if c_mode /= DONE then
                        raise Read_Error with "Illegal definition of constraint" & Integer'Image (nReadConstr) & ": Incomplete constraint!";
                     end if;
                     constr := new Constraint'(Dimension => nVars, coeffs => cV, rhs => rhs, cType => cType);
                     nReadConstr := nReadConstr + 1;
                     begin
                        case Add_Constraint_To_Array (c => constr,  cap => cap, index => nUsedConstr + 1) is
                        when INFEASIBLE =>
                           Free (constr);
                           status := INFEASIBLE;
                        when NORMAL =>
                           nUsedConstr := nUsedConstr + 1;
                        when REDUNDANT =>
                           Free (constr);
                        end case;
                     exception
                        when error : Overflow_Error =>
                           Free (constr);
                           raise Read_Error with "Illegal definition of constraint" & Integer'Image (nReadConstr) & ": LHS may add up to " & Exception_Message (error) & " situation of type " & number_Name & ".";
                     end;
                     if nReadConstr = nConstr then
                        mode := DONE;
                     end if;
                  end; -- declare of 'mode == READ_CONSTR'

               when DONE =>
                  raise Read_Error with "Too many constraints given! The number of constraints per definition restricted to" & Integer'Image (nConstr) & ".";
               end case; -- READ_MODE cases
            end if; -- empty line
         exception
            when Event : Read_Error =>
               Put_Line ("Error reading data of file " & fileName & " in line" & Integer'Image (nLines) & ": ");
               Put (Exception_Message (Event));
               if (null /= cap) then
                  Free_Constr_Array (cap);
               end if;
               Close (file);
               return READING_ERROR;
         end; -- declare in file loop
         nLines := nLines + 1;
      end loop File_Loop;

      if mode /= DONE then
         Put_Line ("Error reading data of file " & fileName & ": Incomplete data definition. Maybe the defined number of constraints is bigger then the number of given constraints?");
         Free_Constr_Array (cap);
         Close (file);
         return READING_ERROR;
      end if;

      Close (file);
      if 0 = nUsedConstr and then not (status = INFEASIBLE) then
         return UNCONSTRAINED;
      else
         if not (nUsedConstr = nReadConstr) and then not (status = INFEASIBLE) then
            Resize_Constr_Array (cap => cap, newNConstraints => nUsedConstr);
         end if;
         return status;
      end if;

   end Read_Bip_From_File;

end Read_Bip;