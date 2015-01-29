-- gray_code_package.adb

with Interfaces;

package body Gray_Code_Package is
   
   procedure Get_Changed_Bit (Index              : in  Int_64;
                              Bit                : out Position_Type;
                              Is_Positive_Switch : out Boolean)
   is
      Mask   : Int_64 := ((not Index) + 1) and Index;
   begin
      Bit := 0;
      if ((Mask and 16#ffffffff00000000#) /= 0) then
         Bit := Bit + 32;
      end if;
      if ((Mask and 16#ffff0000ffff0000#) /= 0) then
         Bit := Bit + 16;
      end if;
      if ((Mask and 16#ff00ff00ff00ff00#) /= 0) then
         Bit := Bit + 8;
      end if;
      if ((Mask and 16#f0f0f0f0f0f0f0f0#) /= 0) then
         Bit := Bit + 4;
      end if;
      if ((Mask and 16#cccccccccccccccc#) /= 0) then
         Bit := Bit + 2;
      end if;
      if ((Mask and 16#aaaaaaaaaaaaaaaa#) /= 0) then
         Bit := Bit + 1;
      end if;
      Is_Positive_Switch := (Interfaces.Shift_Left (Mask, 1) and Index) = 0;
   end Get_Changed_Bit;
   
   function Get_Code_Word (Index : Int_64) return Int_64 is
   begin
      return Index xor Interfaces.Shift_Right (Index, 1);
   end Get_Code_Word;

end Gray_Code_Package;
