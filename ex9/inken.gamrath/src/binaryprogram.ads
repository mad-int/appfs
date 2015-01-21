with Ada.Unchecked_Deallocation;
with misc;                               use misc;

package binaryProgram is

type Problem is record
   size      : Integer range 1 .. 2*MAX_MATRIX_SIZE;
   redundant : Integer range 0 .. MAX_MATRIX_SIZE;
   m         : Integer range 0 .. MAX_MATRIX_SIZE;
   n         : Integer range 1 .. MAX_MATRIX_SIZE;
   coefs     : Double_Arr_Ptr;
   rhs       : Double_Arr_Ptr;
end record;

type Problem_Ptr is access Problem;
procedure Free_Problem is new Ada.Unchecked_Deallocation (Problem, Problem_Ptr);

-- determines whether the binary program data is valid
function bp_is_valid (bp : in  Problem_Ptr) return Boolean;

-- creates a new binary program
procedure bp_new (
   m  : in  Integer;
   n  : in  Integer;
   bp : out Problem_Ptr
)
with Pre  => m in 1 .. MAX_MATRIX_SIZE and n in 1 .. MAX_MATRIX_SIZE,
     Post => bp_is_valid (bp);

-- frees binary program data
procedure bp_free (
   bp : in out Problem_Ptr
)
with Pre  => bp_is_valid (bp);

-- adds a new row (constraint)
procedure bp_put (
   bp      : in Problem_Ptr;
   coefs   : in     Double_Arr_Ptr;
   rhs     : in     Double;
   retcode : out    BP_RETCODE
)
with Pre  => bp_is_valid (bp),
     Post => bp_is_valid (bp);

-- gets the number of rows (constraints)
function bp_getM (bp : Problem_Ptr) return Integer
   with Pre => bp_is_valid(bp);

-- gets redundant number of rows (constraints)
function bp_getRedundant (bp : Problem_Ptr) return Integer
   with Pre => bp_is_valid(bp);

-- -- solves the BP
-- procedure solveBP (
--    bp      : in  Problem_Ptr;
--    retcode : out BP_RETCODE
-- )
-- with Pre  => bp_is_valid (bp),
--      Post => bp_is_valid (bp);

-- solves the BP with branching tree
procedure solveBT (
   bp      : in  Problem_Ptr;
   retcode : out BP_RETCODE
)
with Pre  => bp_is_valid (bp),
     Post => bp_is_valid (bp);

-- branches in the tree
procedure branch (
   bp              : in     Problem_Ptr;
   lhs             : in out Double_Arr_Ptr;
   min             : in out Double_Arr_Ptr;
   max             : in out Double_Arr_Ptr;
   depth           : in     Integer;
   solutions       : in out Long_Integer;
   solution_String : in String := ""
)
with Pre  => bp_is_valid (bp),
     Post => bp_is_valid (bp);

private

-- determines whether the binary program data is valid
function bp_is_valid (bp : in  Problem_Ptr) return Boolean is (bp /= NULL and then bp.m <= bp.size and then bp.coefs /= NULL and then bp.rhs /= NULL);

end binaryProgram;
