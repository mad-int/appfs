with constraint_array; use constraint_array;

package Read_Bip is

   type Exit_Status is (NORMAL, INFEASIBLE, UNCONSTRAINED, READING_ERROR);

   function Read_Bip_From_File (fileName : String; cap : out Constr_Array_Ptr) return Exit_Status;

end Read_Bip;