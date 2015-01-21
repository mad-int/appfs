-- with Ada.Assertions;             use Ada.Assertions;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;

-- pragma Optimize (time);
package body binaryProgram is

procedure print_solution (
   str      : in String;
   length   : in Integer;
   feasible : in Boolean
) is
   length2 : constant Long_Integer := 2 ** length;
   p       : Long_Integer;
begin
   for i in 1 .. length2 loop
      Put (str (str'First .. Index_Non_Blank (Source => str, Going => Backward)));
      p := i;
      for k in 1 .. length+1 loop
         Put (" " & Long_Integer'Image (p mod 2));
         p := p / 2;
      end loop;
      Put_Line (" is" & (if feasible then "" else " not") & " feasible.");
   end loop;
end;

procedure update_lhs (
   lhs : in out Double_Arr_Ptr;
   bp  : in     Problem_Ptr;
   j   : in     Integer;
   add : in     Boolean
) is
begin
   for i in 1 .. bp.m loop
      lhs(i) := lhs(i) + bp.coefs((i-1)*bp.n+j) * (if add then 1.0 else (-1.0));
   end loop;
end update_lhs;

procedure set_minmax (
   min : in out Double_Arr_Ptr;
   max : in out Double_Arr_Ptr;
   bp  : in     Problem_Ptr
) is
begin
   for i in 1 .. bp.m loop
      for j in 2 .. bp.n loop
         min(i) := min(i) + Double'Min(bp.coefs((i-1)*bp.n+j), 0.0);
         max(i) := max(i) + Double'Max(bp.coefs((i-1)*bp.n+j), 0.0);
      end loop;
   end loop;
end set_minmax;

procedure update_minmax (
   min : in out Double_Arr_Ptr;
   max : in out Double_Arr_Ptr;
   bp  : in     Problem_Ptr;
   j   : in     Integer;
   add : in     Boolean
) is
begin
   for i in 1 .. bp.m loop
      min(i) := min(i) + Double'Min(bp.coefs((i-1)*bp.n+j), 0.0) * (if add then 1.0 else (-1.0));
      max(i) := max(i) + Double'Max(bp.coefs((i-1)*bp.n+j), 0.0) * (if add then 1.0 else (-1.0));
   end loop;
end update_minmax;

-- creates a new binary program
procedure bp_new (
   m  : in  Integer;
   n  : in  Integer;
   bp : out Problem_Ptr
) is
   mm : constant Integer := 2*m;
begin
   bp := new Problem'(m => 0, n => n, size => mm, redundant => 0, coefs => new Double_Arr (1 .. mm * n), rhs => new Double_Arr (1 .. mm));
--    bp           := new Problem;
--    bp.m         := 0;
--    bp.n         := n;
--    bp.size      := 2*m;
--    bp.redundant := 0;
--    bp.coefs     := new Double_Arr (1 .. bp.size * bp.n);
--    bp.rhs       := new Double_Arr (1 .. bp.size);

--    Assert (bp_is_valid (bp));
end bp_new;

-- frees binary program data
procedure bp_free (
   bp : in out Problem_Ptr
) is
begin
--    Assert (bp_is_valid(bp));

   Free_Double_Array (bp.rhs);
   Free_Double_Array (bp.coefs);
   Free_Problem (bp);
end bp_free;

-- adds a new row (constraint)
procedure bp_put (
   bp      : in  Problem_Ptr;
   coefs   : in      Double_Arr_Ptr;
   rhs     : in      Double;
   retcode : out     BP_RETCODE
) is
   max : Double := 0.0;
   min : Double := 0.0;
begin
--    Assert (bp_is_valid(bp));

   -- abort if more rows (constraints) wants to be added than announced
   if bp.m = bp.size then
      Put_Line ("Problem overflow size = " & Integer'Image (bp.m));
      retcode := BP_ERROR;
      return;
   end if;
--    Put_Line ("Assert m < size");
--    Assert (bp.m < bp.size);

   -- compute maximal and minimal activity
--    if DEBUG then
--       Put ("Add consttraint: ");
--    end if; -- DEBUG
   for j in 1 .. bp.n loop
      if coefs(j) > 0.0 then
         max := max + coefs(j);
      elsif coefs(j) < 0.0 then
            min := min + coefs(j);
      end if;
--       if DEBUG then
--          if j > 1 then
--             Put ("+ ");
--          end if;
--          Put (Double'Image (coefs(j)));
--          Put (" x_");
--          Put (Integer'Image (j));
--          Put (" ");
--       end if; -- DEBUG
   end loop;
--    if DEBUG then
--       Put_Line ("<= " & Double'Image (rhs));
--    end if; -- DEBUG

   -- constraint is redundant, when maximal activity is smaller or equal to rhight-hand side
   --    => constraint is not added

   if max <= rhs then
--    if DEBUG then
--       Put_Line ("Constraint is redundant.");
--    end if; -- DEBUG
      bp.redundant := bp.redundant + 1;
      retcode := BP_OKAY;
      return;
   end if;

   -- constraint is infeasible, when minimal activity is greater than rhight-hand side
   --    => return infeasible

   if min > rhs then
      retcode := BP_INFEASIBLE;
      return;
   end if;

   -- check for redundancy (multiple of other constraint)
   --    => constraint is not added, when it is redundant

   for i in 1 .. bp.m loop
      if (rhs = 0.0 and then bp.rhs(i) = 0.0) or else (rhs /= 0.0 and then bp.rhs(i) /= 0.0) then
         declare
            quotposmax, quotposmin, quotnegmax, quotnegmin, quot : Double := 0.0;
--             quotposmax : Double                   := 0.0;
--             quotposmin : Double                   := 0.0;
--             quotnegmax : Double                   := 0.0;
--             quotnegmin : Double                   := 0.0;
--             quot       : Double                   := 0.0;
            dom        : Integer range -1   .. 1  := 0;
            j          : Integer                  := 1;
         begin
            while j <= bp.n loop

               -- break, when one coefficient is zero and the other is negative
               exit when (coefs(j) = 0.0 and then bp.coefs(j) < 0.0) or else (coefs(j) < 0.0 and then bp.coefs(j) = 0.0);
               -- nothing to do, when both coefficients are zero
               if coefs(j) = 0.0 and then bp.coefs(j) = 0.0 then
                  null;
               -- only stored constraint can dominates new one, when coefficient of new one is zero
               elsif coefs(j) = 0.0 and then bp.coefs(j) > 0.0 then
                  exit when dom = -1 or else quot /= 0.0;
                  dom := 1;
               -- only new constraint can dominates stored one, when coefficient of stored one is zero
               elsif coefs(j) > 0.0 and then bp.coefs(j) = 0.0 then
                  exit when dom = 1 or else quot /= 0.0;
                  dom := -1;
               else
--                   Put_Line ("Assert coefficients /= 0");
--                   Assert (coefs(j) /= 0.0);
--                   Assert (bp.coefs(j) /= 0.0);
                  declare
                     q : constant Double := coefs(j) / bp.coefs((i-1)*bp.n+j);
                  begin
                     -- compute quotient for positive coefficients
                     if coefs(j) > 0.0 and then bp.coefs((i-1)*bp.n+j) > 0.0 then
                        exit when quot /= 0.0;
                        if quotposmax = 0.0 then
--                            Put_Line ("Assert quotposmin = 0");
--                            Assert (quotposmin = 0.0);
                           quotposmax := q;
                           quotposmin := q;
                        else
                           quotposmax := Double'Max(quotposmax, q);
                           quotposmin := Double'Min(quotposmin, q);
                        end if;
                        exit when quotnegmax /= 0.0 and then ((dom = 1 and then quotposmax > quotnegmin) or else (dom = -1 and then quotposmin > quotnegmax));
                     -- compute quotient for negative coefficients
                     elsif coefs(j) < 0.0 and then bp.coefs((i-1)*bp.n+j) < 0.0 then
                        exit when quot /= 0.0;
                        if quotnegmax = 0.0 then
--                            Put_Line ("Assert quotnegmin = 0");
--                            Assert (quotnegmin = 0.0);
                           quotnegmax := q;
                           quotnegmin := q;
                        else
                           quotnegmax := Double'Max (quotnegmax, q);
                           quotnegmin := Double'Min (quotnegmin, q);
                        end if;
                        exit when quotposmax /= 0.0 and then ((dom = 1 and then quotposmax > quotnegmin) or else (dom = -1 and then quotposmin > quotnegmax));

                     -- compute quotient for coefficients with different sign
                     else
                        exit when quotnegmax /= 0.0 or else quotposmax /= 0.0 or else dom /= 0 or else (quot /= 0.0 and then abs(quot) - abs(q) < EPSILON);
                        if quot = 0.0 then
                           quot := q;
                        end if;
                     end if;

                     if quotposmax /= 0.0 or else quotnegmax /= 0.0 then
                        exit when quotposmax /= 0.0 and then quotnegmax /= 0.0 and then (quotposmax > quotnegmin or else quotposmin < quotnegmax);
                        if quotposmax /= 0.0 or else quotnegmax /= 0.0 then
                           -- check whether stored constraint dominates new one
                           if (quotposmax > 0.0 and then rhs >= bp.rhs(i) * quotposmax) or else (quotposmax = 0.0 and then rhs >= bp.rhs(i) * quotnegmin) then
                              exit when dom = -1;
                              dom := 1;
                           -- check whether new constraint dominates stored one
                           elsif (quotposmin > 0.0 and then rhs <= bp.rhs(i) * quotposmin) or else (quotposmin = 0.0 and then rhs <= bp.rhs(i) * quotnegmax) then
                              exit when dom = 1;
                              dom := -1;
                           -- none of the two constraints is dominating
                           else
                              exit;
                           end if;
                        end if;
                        -- should be dead code
                        exit when dom /= 0 and then quot /= 0.0;
                     end if;
                  end;
               end if;
               j := j + 1;
--                Put_Line ("dom: " & Integer'Image (dom) & "  quotposmax: " & Double'Image (quotposmax) & "  quotposmin: " & Double'Image (quotposmin) & "  quotnegmax: " & Double'Image (quotnegmax) & "  quotnegmin: " & Double'Image (quotnegmin));
            end loop;
            if j > bp.n then
--                Put_Line ("Assert Some value must be set");
--                Assert (quotposmax /= 0.0 or else quotnegmax /= 0.0 or else quot /= 0.0);
               if quot /= 0.0 then
                  if rhs / quot + bp.rhs(i) < -EPSILON and then dom = 0 then
                     retcode := BP_INFEASIBLE;
                     return;
                  end if;
               elsif dom = 0
                     or else (dom = -1 and then (quotposmin = 0.0 or else rhs <= bp.rhs(i) * quotposmin) and then (quotposmin > 0.0 or else rhs <= bp.rhs(i) * quotnegmax))
                     or else (dom = 1 and then ((quotposmax = 0.0 or else rhs >= bp.rhs(i) * quotposmax) and then (quotposmax > 0.0 or else rhs < bp.rhs(i) * quotnegmin))) then

                  if quotposmax = quotposmin and then quotnegmax = quotnegmin then
--                      Assert (quotposmax /= 0.0 or else quotnegmax /= 0.0);
--                      Assert (quotposmax = quotnegmax or else quotposmax = 0.0 or else quotnegmax = 0.0);
                     if dom = 0 then
                        bp.rhs(i) := Double'Min (bp.rhs(i), rhs / quotposmax);
                     elsif dom = -1 then
--                         if (quotposmin > 0.0 and then rhs > bp.rhs(i) * quotposmin) or else (quotposmin = 0.0 and then rhs > bp.rhs(i) * quotnegmax) then
--                            Put_Line ("ERROR 1");
--                         end if;
                        for k in 1 .. bp.n loop
                           bp.coefs((i-1)*bp.n+k) := coefs(k);
                        end loop;
                        bp.rhs(i) := rhs;
--                      elsif (quotposmax > 0.0 and then rhs < bp.rhs(i) * quotposmax) or else (quotposmax = 0.0 and then rhs < bp.rhs(i) * quotnegmin) then
--                         Put_Line ("ERROR 2");
                     end if;
                  else
--                      Assert (dom /= 0);
                     if dom = -1 then
--                         if (quotposmin > 0.0 and then rhs > bp.rhs(i) * quotposmin) or else (quotposmin = 0.0 and then rhs > bp.rhs(i) * quotnegmax) then
--                            Put_Line ("ERROR 3");
--                         end if;
                        for k in 1 .. bp.n loop
                           bp.coefs((i-1)*bp.n+k) := coefs(k);
                        end loop;
                        bp.rhs(i) := rhs;
--                      elsif (quotposmax > 0.0 and then rhs < bp.rhs(i) * quotposmax) or else (quotposmax = 0.0 and then rhs < bp.rhs(i) * quotnegmin) then
--                         Put_Line ("ERROR 4");
                     end if;
                  end if;
--                   if DEBUG then
--                      Put_Line ("redundant constraint.\n");
--                   end if; -- DEBUG
                  bp.redundant := bp.redundant + 1;
                  retcode := BP_OKAY;
                  return;
               end if;
            end if;
         end;
      end if;
   end loop;

   -- add row (constraint) to binary program
   for i in 1 .. bp.n loop
      bp.coefs(bp.m * bp.n + i) := coefs(i);
   end loop;
   bp.m := bp.m + 1;
   bp.rhs(bp.m) := rhs;

   retcode := BP_OKAY;
end bp_put;

-- gets the number of rows (constraints)
function bp_getM (bp : Problem_Ptr) return Integer is (bp.m);

-- gets redundant number of rows (constraints)
function bp_getRedundant (bp : Problem_Ptr) return Integer is (bp.redundant);

-- -- solves the BP
-- procedure solveBP (
--    bp      : in  Problem_Ptr;
--    retcode : out BP_RETCODE
-- ) is
--    count  : Long_Integer    := 0;
--    values : Integer_Arr_Ptr;
--    length : constant Long_Integer := 2 ** bp.n;
--    sum    : Double;
--    p      : Long_Integer;
--    valid  : Boolean         := TRUE;
-- begin
--    values := new Integer_Arr (1 .. bp.n);
--
--    count := length;
--    -- for all vectors
--    for i in 1 .. length loop
--       p := i-1;
--       -- create vector
--       for k in 1 .. bp.n loop
--          values(k) := Integer (p mod 2);
--          p := p / 2;
--       end loop;
--       -- for all constraints
--       for j in 1 .. bp.m loop
--          sum := 0.0;
--          -- compute left-hand side of the constraint
--          for k in 1 .. bp.n loop
--             sum := sum + bp.coefs((j-1)*bp.n+k) * Double (values(k));
--          end loop;
--          -- check constraint for infeasibility
--          if sum > bp.rhs(j) then
-- --          if DEBUG then
-- --             valid := FALSE;
-- --          end if; -- DEBUG
--             -- decrease number of possible feasible solutions
--             count := count - 1;
--             exit;
--          end if;
--       end loop;
-- --       if DEBUG then
-- -- --          print solution
-- --          for k in 1 .. bp.n loop
-- --             Put (Integer'Image (values(k)));
-- --             if k < bp.n then
-- --                Put (" ");
-- --             else
-- --                Put (" is ");
-- --                Put ((if valid then "" else "not "));
-- --                Put_Line ("feasible.");
-- --             end if;
-- --          end loop;
-- --       end if; -- DEBUG
--    end loop;
--
--    -- free vector
--    Free_Integer_Array (values);
-- --    Assert (bp_is_valid(bp));
--
--    Put (Long_Integer'Image (count));
--    Put_Line (" solutions found");
--    if count > 0 then
--       retcode := BP_OKAY;
--       return;
--    end if;
--
--    retcode := BP_INFEASIBLE;
-- end solveBP;

-- solves the BP with branching tree
procedure solveBT (
   bp      : in  Problem_Ptr;
   retcode : out BP_RETCODE
) is
   count           : Long_Integer    := 0;
   lhs             : Double_Arr_Ptr  := new Double_Arr (1 .. bp.m);
   min             : Double_Arr_Ptr  := new Double_Arr (1 .. bp.m);
   max             : Double_Arr_Ptr  := new Double_Arr (1 .. bp.m);
   depth           : Integer         := 0;
   solution_String : String (1 .. 2*bp.n-1);
begin
   -- check whether there are only redundant constraint in the bp
   if bp.m = 0 then
      -- set number of feasible solutions
      count := 2 ** bp.n;
--       if DEBUG then
--          print_solution(str => "", length => bp.n , feasible => TRUE);
--       end if; -- DEBUG

      Put (Long_Integer'Image (count));
      Put_Line (" solutions found");
      retcode := BP_OKAY;
      Free_Double_Array (max);
      Free_Double_Array (min);
      Free_Double_Array (lhs);
      return;
   end if;

   -- initialize arrays
   lhs.all := (others => 0.0);
   min.all := (others => 0.0);
   max.all := (others => 0.0);

   depth := depth + 1;

   -- initialize min, max and lhs
--    for i in 1 .. bp.m loop
--       declare
--          coef : Double;
--       begin
-- --          lhs(i) := 0.0;
-- --          max(i) := 0.0;
-- --          min(i) := 0.0;
--          for j in 2 .. bp.n loop
--             coef := bp.coefs((i-1)*bp.n+j);
--             max(i) := max(i) + Double'Max(coef, 0.0);
--             min(i) := min(i) + Double'Min(coef, 0.0);
-- --             if coef > 0.0 then
-- --                max(i) := max(i) + coef;
-- --             elsif coef < 0.0 then
-- --                min(i) := min(i) + coef;
-- --             end if;
--          end loop;
--          -- add coefficients of first variable to fixing
-- --          lhs(i) := lhs(i) + bp.coefs((i-1)*bp.n+depth);
--       end;
--    end loop;
   set_minmax (min, max, bp);
   update_lhs (lhs, bp, depth, TRUE);

   -- branch on first variable => false
--    if DEBUG then
--       Overwrite (solution_String, solution_String'First, "1");
--    end if; -- DEBUG
   branch (bp => bp, lhs => lhs, min => min, max => max, depth => depth, solutions => count, solution_String => solution_String);

   -- remove coefficients of first variable to fixing
--    for i in 1 .. bp.m loop
--       lhs(i) := lhs(i) - bp.coefs((i-1)*bp.n+depth);
--    end loop;
   update_lhs (lhs, bp, depth, FALSE);

   -- branch on first variable => true
--    if DEBUG then
--       Overwrite (solution_String, solution_String'First, "0");
--    end if; -- DEBUG
   branch(bp => bp, lhs => lhs, min => min, max => max, depth => depth, solutions => count, solution_String => solution_String);

   -- free arrays
   Free_Double_Array (max);
   Free_Double_Array (min);
   Free_Double_Array (lhs);

   Put(Long_Integer'Image (count));
   Put_Line(" solutions found.");

   retcode := BP_OKAY;
   if count = 0 then
      retcode := BP_INFEASIBLE;
   end if;

end solveBT;

-- branches in the tree
procedure branch (
   bp              : in     Problem_Ptr;
   lhs             : in out Double_Arr_Ptr;
   min             : in out Double_Arr_Ptr;
   max             : in out Double_Arr_Ptr;
   depth           : in     Integer;
   solutions       : in out Long_Integer;
   solution_String : in String              := ""
) is
   unrestricted        : Boolean            := TRUE;
   new_depth           : Integer;
   new_Solution_String : String (1 .. solution_String'Length);
   position            : constant Integer   := new_Solution_String'First+2*depth;
--    j                   : Integer;
begin
--    Assert (min /= NULL);
--    Assert (max /= NULL);
--    if depth = bp.n then
--       for i in 1 .. bp.m loop
--          Put_Line (Integer'Image (i) & ": " & Double'Image (min(i)) & "  " & Double'Image (max(i)));
--          Assert (min(i) = 0.0);
--          Assert (max(i) = 0.0);
--       end loop;
--    end if;

--    if DEBUG then
--       Overwrite (new_Solution_String, new_Solution_String'First, solution_String);
--    end if;

   declare
      i : Integer := 1;
   begin
      while i <= bp.m and then lhs(i) + min(i) <= bp.rhs(i) loop
         -- check activities
--          exit when lhs(i) + min(i) > bp.rhs(i);
--          if lhs(i) + max(i) > bp.rhs(i) then
--             unrestricted := FALSE;
--          end if;
         unrestricted := (unrestricted and then lhs(i) + max(i) <= bp.rhs(i));
         i := i + 1;
      end loop;

      -- when CUTOFF is defined, do not branch further, when subtree is infeasible
      if i <= bp.m then
--          if DEBUG then
--             print_solution(str => solution_String (solution_String'First .. Index_Non_Blank (Source => solution_String, Going => Backward)), length => bp.n-depth , feasible => FALSE);
--          end if; -- DEBUG
         return;
      end if;
   end;

   -- when all solutions were feasible
   if unrestricted then
--       if DEBUG and then depth < bp.n then
--          print_solution(str => solution_String (solution_String'First .. Index_Non_Blank (Source => solution_String, Going => Backward)), length => bp.n-depth , feasible => TRUE);
--       end if; -- DEBUG
      solutions := solutions + 2 ** (bp.n - depth);
      return;
   end if;

--    if DEBUG and then depth = bp.n then
--       Put_Line (solution_String & " is not feasible.");
--    end if; -- DEBUG

   new_depth := depth + 1;
   if new_depth <= bp.n then

--       j := new_depth - bp.n;
--       for i in 1 .. bp.m loop
--          j := j + bp.n;
--          -- update min and max
--          max(i) := max(i) - Double'Max(bp.coefs(j), 0.0);
-- --          max(i) := max(i) - (bp.coefs(j) + abs (bp.coefs(j))) / 2.0;
--          min(i) := min(i) - Double'Min(bp.coefs(j), 0.0);
-- --          min(i) := min(i) - (bp.coefs(j) - abs (bp.coefs(j))) / 2.0;
--          -- add coefficients of first variable to fixing
--          lhs(i) := lhs(i) + bp.coefs(j);
--       end loop;
--       Put (Double'Image (max(1)));
      update_minmax (min, max, bp, new_depth, FALSE);
--       Put_Line (" => " & Double'Image (max(1)));
      update_lhs (lhs, bp, new_depth, TRUE);


      -- branch on next variable => false
--       if DEBUG then
-- --          Overwrite (new_Solution_String, new_Solution_String'First+2*depth, "1");
--          Overwrite (new_Solution_String, position, "1");
--       end if; -- DEBUG
      branch(bp, lhs, min, max, new_depth, solutions, new_Solution_String);

      -- remove coefficients of first variable to fixing
--       j := new_depth - bp.n;
--       for i in 1 .. bp.m loop
--          j := j + bp.n;
--          lhs(i) := lhs(i) - bp.coefs(j);
--       end loop;
      update_lhs (lhs, bp, new_depth, FALSE);

   -- branch on next variable => true
--       if DEBUG then
-- --          Overwrite (new_Solution_String, new_Solution_String'First+2*depth, "0");
--          Overwrite (new_Solution_String, position, "0");
--       end if; -- DEBUG
      branch(bp, lhs, min, max, new_depth, solutions, new_Solution_String);

      -- update min and max
--       j := new_depth - bp.n;
--       for i in 1 .. bp.m loop
--          j := j + bp.n;
--          max(i) := max(i) + Double'Max(bp.coefs(j), 0.0);
-- --          max(i) := max(i) + (bp.coefs(j) + abs (bp.coefs(j))) / 2.0;
--          min(i) := min(i) + Double'Min(bp.coefs(j), 0.0);
-- --          min(i) := min(i) + (bp.coefs(j) - abs (bp.coefs(j))) / 2.0;
--       end loop;
      update_minmax (min, max, bp, new_depth, TRUE);
   end if;
end branch;

end binaryProgram;
