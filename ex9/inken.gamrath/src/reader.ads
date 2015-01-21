with binaryProgram;              use binaryProgram;
with misc;                       use misc;

package reader is

procedure Process_File (
   filename : in  String;
   bp       : out Problem_Ptr;
   retcode  : out BP_RETCODE
);

end reader;
