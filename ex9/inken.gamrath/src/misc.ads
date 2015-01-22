with Ada.Calendar;               use Ada.Calendar;
with Ada.Unchecked_Deallocation;

package misc is

DEBUG           : Boolean := FALSE;

MAX_LINE_LEN    : Integer := 512;         -- Maximum input line length
MAX_STR_LEN     : Integer := 512;         -- Maximum string length
MAX_MATRIX_SIZE : Integer := 32;          -- Maximum colums and rows of matrix

type Double is new Long_Float;

EPSILON         : Double   := 0.000000001;

type Double_Arr      is array (Integer range <>) of Double;
type Double_Arr_Ptr  is access Double_Arr;

type Integer_Arr     is array (Integer range <>) of Integer;
type Integer_Arr_Ptr is access Integer_Arr;

-- return codes for methods: non-positive return codes are errors
type BP_RETCODE      is
(
   BP_OKAY,            -- normal termination
   BP_INFEASIBLE,      -- infeasibility was detected
   BP_ERROR,           -- unspecified error
   BP_READERROR,       -- read error
   BP_NOFILE,          -- file not found error
   BP_INVALIDDATA      -- error in input data
);

-- returns time in seconds
function GET_SEC (a, b : Time) return Duration;

-- deallocate arrays
procedure Free_Double_Array  is new Ada.Unchecked_Deallocation (Object => Double_Arr,  Name => Double_Arr_Ptr);
procedure Free_Integer_Array is new Ada.Unchecked_Deallocation (Object => Integer_Arr, Name => Integer_Arr_Ptr);

end misc;
