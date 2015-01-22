package Vector is

   type Vector is array (Integer range <>) of Integer;
   type Vector_ptr is access Vector;

   type Binary is new Integer range 0..1;

   type BinaryVector is array (Integer range <>) of Binary;
   type BinaryVector_ptr is access BinaryVector;

   -- Prints a binary vector --
   procedure Print_Binary_Vector(vec : in BinaryVector_ptr);

   -- Prints vector --
   procedure Print_Vector(vec : in Vector_ptr);

end Vector;