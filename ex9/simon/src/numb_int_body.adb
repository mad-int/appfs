with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
package body numb is

   function Parse_String_To_Number (numberString : String) return Number is
   begin
      return Integer'Value (numberString);
   end Parse_String_To_Number;

   function Parse_Number_To_String (numb : Number) return String is
   begin
      return Integer'Image (numb);
   end Parse_Number_To_String;

   function Parse_Number_To_String_With_Length (numb : Number; length : Positive) return String is
      output       : String (1 .. length);
      parsedString : String := Integer'Image (numb);
   begin
      if not (output'Length < parsedString'Length) then
         Ada.Strings.Fixed.move (Source => parsedString, Target => output);
         return output;
      else
         return parsedString;
      end if;
   end Parse_Number_To_String_With_Length;

   function Number_Equals_Number (x : Number; y : Number) return Boolean is
   begin
      return x = y;
   end Number_Equals_Number;

   function Number_Less_Than_Number (x : Number; y : Number) return Boolean is
   begin
      return x < y;
   end Number_Less_Than_Number;

   function Number_Greater_Than_Number (x : Number; y : Number) return Boolean is
   begin
      return x > y;
   end Number_Greater_Than_Number;

   function Int_To_Numb (X : Integer) return Number is
   begin
      return X;
   end Int_To_Numb;

end numb;
