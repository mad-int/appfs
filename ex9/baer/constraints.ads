with Ada.Text_IO;

--
-- Package Declaration
--
package Constraints is

    subtype Int is Integer range 1 .. 32;
    subtype Bin is Integer range 0 .. 1;
    -- for Bin'Width use 1;
    --pragma Assert(Bin'Width = 1);

    --type Double is digits 11;
    type Double is new Long_Float;

    type Compare_Type is (LESSEQ, GREATEQ, EQUAL);

    type BinVector_Type is array (Int range <>) of Bin with pack;
    type Matrix_Type    is array (Int range <>, Int range <>) of Double;
    type Vector_Type    is array (Int range <>) of Double;
    type CmpVector_Type is array (Int range <>) of Compare_Type;

    type Constraints_t(rows : Int; cols : Int) is
    record
        matrix  : Matrix_Type   (1 .. rows, 1 .. cols);
        cmp     : CmpVector_Type(1 .. rows);
        rhs     : Vector_Type   (1 .. rows);
    end record;

    --
    --
    --

    -- Alternativ: cs.rows is possible.
    function Get_Rows(cs : in Constraints_t) return Int;

    -- Alternativ: cs.cols is possible.
    function Get_Cols(cs : in Constraints_t) return Int;

    function is_Feasible(cs : in Constraints_t; x : in BinVector_Type) return Boolean;
    -- TODO: function is_Feasible_Bitflip

    procedure Print_BinVector(x : in BinVector_Type);

    procedure Print_Constraints(cs : in Constraints_t);

    --
    function Read_Constraints(filename : in String) return Constraints_t
        with Pre => filename'Length > 0;

    function Read_Int(file : in Ada.Text_IO.File_Type; linecount : in out Integer) return Int;
    procedure Read_Constraint(file : in Ada.Text_IO.File_Type; linecount : in out Integer;
            constrno : in Natural; cs : in out Constraints_t);

    function Trim_Line(line_org : in String) return String;

    --

    --
    -- This suckz.
    -- Coded binary-functions on the BinVector_Type, because I had no other idea
    -- how to do it better, yet.
    --
    procedure inc(x : in out BinVector_Type);
    procedure dec(x : in out BinVector_Type);

    function andBin(a : in BinVector_Type; b : in BinVector_Type) return BinVector_Type
        with Pre => a'First = b'First and a'Last = b'Last, -- a'Range = b'Range is not allowed!?
            Post => andBin'Result'First = a'First and andBin'Result'Last = a'Last;

    function xorBin(a : in BinVector_Type; b : in BinVector_Type) return BinVector_Type
        with Pre => a'First = b'First and a'Last = b'Last, -- a'Range = b'Range is not allowed!?
            Post => xorBin'Result'First = a'First and xorBin'Result'Last = a'Last;

end Constraints;

