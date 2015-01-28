with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces;   use Interfaces;

with Verbosity;    use Verbosity;

package body Report is
   protected body Display is
      procedure Solution(inst : BIP.Instance; x : Interfaces.Unsigned_32) is

         package U32_IO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_32);
         use U32_IO;

         bit : Interfaces.Unsigned_32 := 1;

      begin
         if (BIP.get_verbosity(inst) >= Verbose) then
            put("solution: ");
            put(x, 10, 16);
            for column in 1 .. bip.get_cols(inst) loop
               if (x and bit) /= 0 then
                  put(" 1");
               else
                  put(" 0");
               end if;
               bit := bit + bit;
            end loop;
            new_line;
         end if;
      end Solution;
   end Display;
end Report;
