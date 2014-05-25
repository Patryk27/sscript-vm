(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 Debug data parser.
*)
Unit DbgParser;

 Interface
 Uses Stream, SysUtils;

 Const DebugDataVersion: uint16 = 1;

 { EDebugDataReaderException }
 Type EDebugDataReaderException = Class(Exception);

 { TDBGFileData }
 Type TDBGFileData =
      Record
       FileName    : String;
       BytecodeSize: uint32;
      End;

 { TDBGFunctionData }
 Type TDBGFunctionData =
      Record
       FunctionName, LabelName: String;
       RangeBegin, RangeEnd   : uint32;
      End;

 { TDBGLineData }
 Type TDBGLineData =
      Record
       FileID, FunctionID: uint16; // taken from the debug symbol list
       Opcode            : uint32;
       Line              : uint16;
      End;

 { lists }
 Type TDBGFileList     = Array of TDBGFileData;
      TDBGFunctionList = Array of TDBGFunctionData;
      TDBGLineDataList = Array of TDBGLineData;

 { TDebugData }
 Type TDebugData =
      Record
       Version: uint16;

       FileCount    : uint32;
       FunctionCount: uint32;
       LineDataCount: uint32;

       FileList    : TDBGFileList;
       FunctionList: TDBGFunctionList;
       LineDataList: TDBGLineDataList;
      End;

 { TDebugDataReader }
 Type TDebugDataReader =
      Class
       Private
        Stream: TStream;

       Public
        Constructor Create(const fStream: TStream);
        Destructor Destroy; override;

        Function Read: TDebugData;
       End;

 Implementation

(* TDebugDataReader.Create *)
Constructor TDebugDataReader.Create(const fStream: TStream);
Begin
 Stream := fStream;
End;

(* TDebugDataReader.Destroy *)
Destructor TDebugDataReader.Destroy;
Begin
 inherited Destroy;
End;

(* TDebugDataReader.Read *)
Function TDebugDataReader.Read: TDebugData;
Var I: uint32;
Begin
 With Result do
 Begin
  // read version
  Version := Stream.read_uint16;

  if (Version <> DebugDataVersion) Then
   raise EDebugDataReaderException.CreateFmt('Unsupported debug data version! (got %d, expected %d)', [Version, DebugDataVersion]);

  // read sizes
  FileCount     := Stream.read_uint32;
  FunctionCount := Stream.read_uint32;
  LineDataCount := Stream.read_uint32;

  // allocate arrays
  SetLength(FileList, FileCount);
  SetLength(FunctionList, FunctionCount);
  SetLength(LineDataList, LineDataCount);

  // read file list
  if (FileCount > 0) Then
  Begin
   For I := 0 To FileCount-1 Do
   Begin
    FileList[I].FileName     := Stream.read_string;
    FileList[I].BytecodeSize := Stream.read_uint32;
   End;
  End;

  // read function list
  if (FunctionCount > 0) Then
  Begin
   For I := 0 To FunctionCount-1 Do
   Begin
    FunctionList[I].FunctionName := Stream.read_string;
    FunctionList[I].LabelName    := Stream.read_string;
    FunctionList[I].RangeBegin   := Stream.read_uint32;
    FunctionList[I].RangeEnd     := Stream.read_uint32;
   End;
  End;

  // read line data
  if (LineDataCount > 0) Then
  Begin
   For I := 0 To LineDataCount-1 Do
   Begin
    LineDataList[I].FileID     := Stream.read_uint16;
    LineDataList[I].FunctionID := Stream.read_uint16;
    LineDataList[I].Opcode     := Stream.read_uint32;
    LineDataList[I].Line       := Stream.read_uint16;

    if (I > 0) Then
    Begin
     if (LineDataList[I].Opcode < LineDataList[I-1].Opcode) Then
      raise EDebugDataReaderException.Create('Debug data was expected to be sorted!');
    End;
   End;
  End;
 End;
End;
End.
