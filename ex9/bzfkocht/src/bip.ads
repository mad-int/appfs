with Interfaces;
with Verbosity;

-- @summary
-- Routines for reading and enumerating binary programs (BIP)
-- @description
-- This package contains the core reoutines for the enumerator,
-- the definition of the data structure, and the supporting
-- routines to read files and manipulate the data structure.
--
package BIP
is
   Max_Cols : constant := 32;
   Max_Rows : constant := 128;

   subtype   Count32      is Long_Integer range 0 .. 2**Max_Cols;
   type      Instance     is private;
   type      Report_Proc  is access protected procedure (inst : Instance; x : Interfaces.Unsigned_32);

   procedure read           (inst : in out Instance; Filename : String);
   procedure print          (inst : Instance);
   function  get_cols       (inst : Instance) return Natural;
   function  get_verbosity  (inst : Instance) return Verbosity.Level;
   function  enumerate      (inst : Instance; report_solution : Report_Proc;
                             prefix_mask : Interfaces.Unsigned_32 := 0; prefix_length : Natural := 0)
                             return Count32 with pre => prefix_length < get_cols(inst);
   procedure set_verbosity  (inst : in out Instance; vlevel : Verbosity.Level);

private
   DimCol    : constant := 1;
   DimRow    : constant := 2;

   subtype   Real         is Long_Float range -10.0**Long_Float'Digits .. 10.0**Long_Float'Digits;
   subtype   Col_Count    is Natural    range 0 .. Max_Cols;
   subtype   Row_Count    is Natural    range 0 .. Max_Rows;
   type      Row_Vector   is array (1 .. Row_Count'Last) of Real;

   type      Matrix       is array (Positive range <>, Positive range <>) of Real;
   type      Matrix_Ptr   is access Matrix;

   function  is_valid     (Inst : Instance) return Boolean;

   type Instance is
      record
         acr            : Matrix_ptr      := new Matrix(1..1,1..1);
         rhs            : Row_Vector;
         equs           : Row_Count       := 0;
         level          : Verbosity.Level := Verbosity.Normal;
      end record
     with Type_Invariant => is_valid(Instance);
end BIP;
