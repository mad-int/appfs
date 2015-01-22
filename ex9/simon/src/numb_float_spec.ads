package numb is

   subtype Number is Long_Float;

   Number_Zero : constant Number := 0.0;

   Number_Name : constant String := "Long_Float";

   function Parse_String_To_Number (numberString : String) return Number;

   function Parse_Number_To_String (numb : Number) return String;

   function Parse_Number_To_String_With_Length (numb : Number; length : Positive) return String;

   function Number_Equals_Number (x : Number; y : Number) return Boolean;

   function Number_Less_Than_Number (x : Number; y : Number) return Boolean;

   function Number_Greater_Than_Number (x : Number; y : Number) return Boolean;

   function Int_To_Numb (x : Integer) return Number;

end numb;
