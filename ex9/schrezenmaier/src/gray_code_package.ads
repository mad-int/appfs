-- gray_code_package.ads

with Interfaces; use Interfaces;

package Gray_Code_Package is

   type Position_Type is range 0 .. 63;   

   subtype Int_64 is Unsigned_64;

   procedure Get_Changed_Bit (Index              : in  Int_64;
                              Bit                : out Position_Type;
                              Is_Positive_Switch : out Boolean);

   function Get_Code_Word (Index : Int_64) return Int_64;

end Gray_Code_Package;
