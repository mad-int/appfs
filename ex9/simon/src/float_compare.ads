package Float_Compare is

   Float_Eps : constant Float := 10.0 ** (-4);

   Longfloat_Eps : constant Long_Float := 10.0 ** (-8);

   function Longfloat_Equals_Longfloat (x : Long_Float; y : Long_Float) return Boolean;

   function Longfloat_Less_Than_Longfloat (x : Long_Float; y : Long_Float) return Boolean;

   function Longfloat_Greater_Than_Longfloat (x : Long_Float; y : Long_Float) return Boolean;

   function Float_Equals_Float (x : Float; y : Float) return Boolean;

   function Float_Less_Than_Float (x : Float; y : Float) return Boolean;

   function Float_Greater_Than_Float (x : Float; y : Float) return Boolean;

end Float_Compare;
