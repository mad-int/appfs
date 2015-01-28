with BIP;
with Interfaces;

package Report is
   protected Display is
      procedure Solution(inst : BIP.Instance; x : Interfaces.Unsigned_32);
   end Display;
end Report;
