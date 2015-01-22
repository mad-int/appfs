with Ada.Text_IO;
use Ada.Text_IO;

package body Vector is

   --Prints a vector--
   procedure Print_Vector(vec : in Vector_ptr)
   is
   begin
      for I in vec'Range loop
         Put(Integer'Image(vec(I)) &  " ");
      end loop;
      Put_Line(" ");
   end Print_Vector;

   --Prints a vector--
   procedure Print_Binary_Vector(vec : in BinaryVector_ptr)
   is
   begin
      for I in vec'Range loop
         Put(Binary'Image(vec(I)) &  " ");
      end loop;
      Put_Line(" ");
   end Print_Binary_Vector;

end Vector;