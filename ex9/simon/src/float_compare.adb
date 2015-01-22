package body Float_Compare is

   function Longfloat_Equals_Longfloat (x : Long_Float; y : Long_Float) return Boolean is
   begin
      return abs (x - y) < Longfloat_Eps;
   end Longfloat_Equals_Longfloat;

   function Longfloat_Less_Than_Longfloat (x : Long_Float; y : Long_Float) return Boolean is
   begin
      return y - x > Longfloat_Eps;
   end Longfloat_Less_Than_Longfloat;

   function Longfloat_Greater_Than_Longfloat (x : Long_Float; y : Long_Float) return Boolean is
   begin
      return x - y > Longfloat_Eps;
   end Longfloat_Greater_Than_Longfloat;

   function Float_Equals_Float (x : Float; y : Float) return Boolean is
   begin
      return abs (x - y) < Float_Eps;
   end Float_Equals_Float;

   function Float_Less_Than_Float (x : Float; y : Float) return Boolean is
   begin
      return y - x > Float_Eps;
   end Float_Less_Than_Float;

   function Float_Greater_Than_Float (x : Float; y : Float) return Boolean is
   begin
      return x - y > Float_Eps;
   end Float_Greater_Than_Float;

end Float_Compare;