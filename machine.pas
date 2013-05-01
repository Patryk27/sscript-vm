(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODE OBJFPC}
{$H+}
{$MODESWITCH ADVANCEDRECORDS}

Unit Machine;

 Interface
 Uses SysUtils, Opcodes, Objects, Exceptions, Classes, Zipper;

 Const EXCEPTIONSTACK_SIZE = 1*1024*1024; // 1 MB exception-stack; I guess it's enough
       STACK_SIZE          = 100000000; // this value is counted in elements count

       bytecode_version_major = 0;
       bytecode_version_minor = 4;

 Const TYPE_BOOL   = 3; // do not modify
       TYPE_CHAR   = 4;
       TYPE_INT    = 5;
       TYPE_FLOAT  = 6;
       TYPE_STRING = 7;

       TypeSize: Array[TYPE_BOOL..TYPE_STRING] of Byte = (sizeof(Boolean), sizeof(Char), sizeof(Integer), sizeof(Extended), sizeof(String));

 Type TMachine = class;

 { opcode's parameters }
 Type POpParam = ^TOpParam;
      TOpParam = Record
                  Public
                   M    : TMachine;
                   Typ  : TPrimaryType;
                   Val  : Int64;
                   fVal : Extended;
                   sVal : String;
                   Index: Byte;

                   Function getBool: Boolean; inline;
                   Function getChar: Char; inline;
                   Function getInt: Int64; inline;
                   Function getLongword: LongWord; inline;
                   Function getFloat: Extended; inline;
                   Function getString: String; inline;
                   Function getReference: LongWord; inline;

                   Function getTypeName: String;
                  End;

 { virtual machine class }
 Type TMachine = Class
                  Private
                   Procedure OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
                   Procedure OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

                   Procedure ParseHeader(const AStream: TStream);
                   Procedure ParseBytecode(const AStream: TStream);

                  Public
                   // variables
                   InputFile: String;

                   CodeData      : PByte;
                   ExceptionStack: PLongWord;
                   Stack         : Array of TOpParam;

                   breg: Array[1..5] of Boolean;
                   creg: Array[1..4] of Char;
                   ireg: Array[1..5] of Int64;
                   freg: Array[1..4] of Extended;
                   sreg: Array[1..4] of String;
                   rreg: Array[1..4] of LongWord;

                   Position     : PByte; // current position in `code` section
                   LastOpcodePos: LongWord;
                   StackPos     : PLongWord; // points at `ireg[5]` (the `stp` register)
                   DebugMode    : Boolean;
                   OpcodeNo     : QWord;

                   is_runnable: Boolean;

                   exception_handler: LongWord;
                   last_exception   : String;

                   exitcode: Integer;

                  Public
                   // methods

                   { reading }
                   Function c_read_byte: Byte; inline;
                   Function c_read_integer: Integer; inline;
                   Function c_read_longword: LongWord; inline;
                   Function c_read_int64: Int64; inline;
                   Function c_read_float: Extended; inline;
                   Function c_read_string: String; inline;

                   Function getString(Pos: LongWord): String;

                   Function read_param: TOpParam; {inline (?)}

                   { stack operations }
                   Procedure StackPush(Value: Integer);
                   Procedure StackPush(Value: Extended);
                   Procedure StackPush(Value: String);
                   Procedure StackPush(Value: Char);
                   Procedure StackPush(Value: Boolean);
                   Procedure StackPush(Value: TOpParam);
                   Procedure CallstackPush(Value: LongWord);
                   Function StackPop: TOpParam;
                   Function CallstackPop: LongWord;

                   { position-related }
                   Function getPosition: LongWord; inline;
                   Procedure setPosition(NewPos: LongWord); inline;

                   { object-related }
                   Function getObject(Address: LongWord): TMObject;
                   Function getArray(Address: LongWord): TMArray;

                   Procedure ThrowException(const Msg: String);

                   { some other stuff }
                   Constructor Create(const FileName: String);
                   Procedure Prepare;
                   Procedure Run;
                   Procedure RunFunction(PackageName, FunctionName: String);

                   Procedure DumpCallstack;

                   Function disasm(Pos: LongWord): String;
                   Procedure FetchLineAndFile(const Pos: LongWord; out eLine: Integer; out eFile: String);
                  End;

 Type TOpcodeProc = Procedure (M: TMachine);
      THandlerProc = Procedure (M: TMachine);

 Var FunctionList: Array of Record // `icall` function list
                             Name, Package: String;
                             Handler      : THandlerProc;
                            End;

 Procedure Log(Txt: String);
 Procedure NewFunction(PackageName, FunctionName: String; fHandler: THandlerProc);

 Var VerboseMode: Boolean=True;

 Implementation
Uses Procs, mOutput, mString, mMath, mTime, mInput, mVM;

Const OpcodeTable: Array[TOpcode_E] of TOpcodeProc =
(
 @op_NOP,
 @op_STOP,
 @op_PUSH,
 @op_POP,
 @op_ADD,
 @op_SUB,
 @op_MUL,
 @op_DIV,
 @op_NEG,
 @op_MOV,
 @op_JMP,
 @op_TJMP,
 @op_FJMP,
 @op_CALL,
 @op_ICALL,
 @op_ACALL,
 @op_RET,
 @op_IF_E,
 @op_IF_NE,
 @op_IF_G,
 @op_IF_L,
 @op_IF_GE,
 @op_IF_LE,
 @op_STRJOIN,
 @op_NOT,
 @op_OR,
 @op_XOR,
 @op_AND,
 @op_SHL,
 @op_SHR,
 @op_MOD,
 @op_ARSET,
 @op_ARGET,
 @op_ARCRT,
 @op_ARLEN,
 @op_OBJFREE,
 @op_OBJINC,
 @op_OBJDEC,
 @op_LOCATION
);

{ Log }
Procedure Log(Txt: String);
Begin
 if (VerboseMode) Then
  Writeln(Txt);
End;

{ NewFunction }
Procedure NewFunction(PackageName, FunctionName: String; fHandler: THandlerProc);
Begin
 SetLength(FunctionList, Length(FunctionList)+1);
 With FunctionList[High(FunctionList)] do
 Begin
  Name    := FunctionName;
  Package := PackageName;
  Handler := fHandler;
 End;
End;

{ TOpParam.getBool }
Function TOpParam.getBool: Boolean;
Begin
 if (Typ in [ptBool, ptBoolReg, ptInt, ptIntReg]) Then
 Begin
  Case Val of
   0: Exit(False);
   else Exit(True);
  End
 End Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getBool) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> bool');
End;

{ TOpParam.getChar }
Function TOpParam.getChar: Char;
Begin
 if (Typ in [ptChar, ptCharReg, ptInt, ptIntReg]) Then
  Exit(chr(Val)) Else
 if (Typ in [ptString, ptStringReg]) Then
  Exit(sVal[1]) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getChar) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> char');
End;

{ TOpParam.getInt }
Function TOpParam.getInt: Int64;
Begin
 if (Typ in [ptInt, ptIntReg, ptBool, ptBoolReg, ptChar, ptCharReg, ptReferenceReg]) Then
  Exit(Val) Else
 if (Typ in [ptFloat, ptFloatReg]) Then
  Exit(Round(fVal)) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getInt) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int');
End;

{ TOpParam.getLongword }
Function TOpParam.getLongword: LongWord;
Begin
 if (Typ in [ptInt, ptIntReg, ptReferenceReg]) Then
  Exit(Val) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getInt) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int (longword)');
End;

{ TOpParam.getFloat }
Function TOpParam.getFloat: Extended;
Begin
 if (Typ in [ptFloat, ptFloatReg]) Then
  Exit(fVal) Else
 if (Typ in [ptInt, ptIntReg, ptBool, ptBoolReg]) Then
  Exit(Val) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getFloat) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> float');
End;

{ TOpParam.getString }
Function TOpParam.getString: String;
Begin
 if (Typ in [ptString, ptStringReg]) Then
  Exit(sVal) Else
 if (Typ in [ptChar, ptCharReg]) Then
  Exit(chr(Val)) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getString) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> string');
End;

{ TOpParam.getReference }
Function TOpParam.getReference: LongWord;
Begin
 if (Typ in [ptInt, ptIntReg, ptReferenceReg, ptCallstackRef]) Then
  Exit(Val) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getReference) Else
  raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int');
End;

{ TOpParam.getTypeName }
Function TOpParam.getTypeName: String;
Begin
 Result := PrimaryTypeNames[Typ];

 if (Typ = ptStackVal) Then
  Result += ' ('+M.Stack[M.StackPos^+Val].getTypeName+')';
End;

{ TMachine.OnCreateStream }
Procedure TMachine.OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Begin
 AStream := TMemoryStream.Create;
End;

{ TMachine.OnDoneStream }
Procedure TMachine.OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Begin
 Log('Parsing unzipped file: '+AItem.ArchiveFileName+' (size: '+IntToStr(AItem.Size)+' bytes)');

 AStream.Position := 0;

 Case AItem.ArchiveFileName of
  '.header': ParseHeader(AStream);
  '.bytecode': ParseBytecode(AStream);
  else
   raise eInternalError.Create('Unknown file to parse: '+AItem.ArchiveFileName);
 End;

 AStream.Free;
End;

{ TMachine.ParseHeader }
Procedure TMachine.ParseHeader(const AStream: TStream);
Var magic_number                : Longword;
    version_major, version_minor: Byte;

    // EndingZero
    Function EndingZero(const Text: String): String;
    Begin
     if (Length(Text) = 1) Then
      Exit(Text+'0') Else
      Exit(Text);
    End;

Begin
 Log('Parsing header...');
 magic_number  := BEtoN(AStream.ReadDWord);
 is_runnable   := Boolean(AStream.ReadByte);
 version_major := AStream.ReadByte;
 version_minor := AStream.ReadByte;

 Log('Magic number: 0x'+IntToHex(magic_number, 8));
 if (magic_number <> $0DEFACED) Then
  raise eInvalidFile.Create('Invalid magic number.');

 Log('Bytecode version: '+IntToStr(version_major)+'.'+EndingZero(IntToStr(version_minor)));
 if (version_major <> bytecode_version_major) or (version_minor <> bytecode_version_minor) Then
  raise eInvalidFile.Create('Unsupported bytecode version.');
End;

{ TMachine.ParseBytecode }
Procedure TMachine.ParseBytecode(const AStream: TStream);
Var I: LongWord;
Begin
 Log('Parsing bytecode...');

 CodeData := AllocMem(AStream.Size);

 if (AStream.Size <> 0) Then
  For I := 0 To AStream.Size-1 Do // @TODO: why `AStream.Read` doesn't work? :|
   CodeData[I] := AStream.ReadByte;
End;

{ TMachine.c_read_byte }
Function TMachine.c_read_byte: Byte;
Begin
 Result := Position^;
 Inc(Position, sizeof(Byte));
End;

{ TMachine.c_read_integer }
Function TMachine.c_read_integer: Integer;
Begin
 Result := BEtoN(PInteger(Position)^);
 Inc(Position, sizeof(Integer));
End;

{ TMachine.c_read_longword }
Function TMachine.c_read_longword: LongWord;
Begin
 Result := BEtoN(PLongWord(Position)^);
 Inc(Position, sizeof(LongWord));
End;

{ TMachine.c_read_int64 }
Function TMachine.c_read_int64: Int64;
Begin
 Result := BEtoN(PInt64(Position)^);
 Inc(Position, sizeof(Int64));
End;

{ TMachine.c_read_float }
Function TMachine.c_read_float: Extended;
Begin
 Result := PExtended(Position)^;
 Inc(Position, sizeof(Extended));
End;

{ TMachine.c_read_string }
Function TMachine.c_read_string: String;
Begin
 Result := '';

 While (Position^ <> 0) Do
 Begin
  Result += chr(Position^);
  Inc(Position);
 End;

 Inc(Position);
End;

{ TMachine.getString }
Function TMachine.getString(Pos: LongWord): String;
Var Ch: PChar;
Begin
 Result := '';

 Ch := PChar(LongWord(CodeData)+Pos);

 While (Ch^ <> #0) do // fetch string until the terminator (null) char
 Begin
  Result += Ch^;
  Inc(Ch);
 End;
End;

{ TMachine.read_param }
Function TMachine.read_param: TOpParam;
Begin
 Result.M   := self;
 Result.Typ := TPrimaryType(c_read_byte);

 if (Result.Typ in [ptBoolReg..ptReferenceReg]) Then
  Result.Index := c_read_byte; // register ID

 Case Result.Typ of
  ptBoolReg: Result.Val := Byte(breg[Result.Index]);
  ptCharReg: Result.Val := ord(creg[Result.Index]);
  ptIntReg: Result.Val := ireg[Result.Index];
  ptFloatReg: Result.fVal := freg[Result.Index];
  ptStringReg: Result.sVal := sreg[Result.Index];
  ptReferenceReg: Result.Val := rreg[Result.Index];

  ptBool: Result.Val := c_read_byte;
  ptChar: Result.Val := c_read_byte;
  ptInt: Result.Val := c_read_int64;
  ptFloat: Result.fVal := c_read_float;
  ptString: Result.sVal := c_read_string;

  else
   Result.Val := c_read_integer;
 End;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Integer);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptInt;
 Stack[StackPos^].Val := Value;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Extended);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ  := ptFloat;
 Stack[StackPos^].fVal := Value;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: String);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ  := ptString;
 Stack[StackPos^].sVal := Value;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Char);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptChar;
 Stack[StackPos^].Val := ord(Value);
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Boolean);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptBool;
 Stack[StackPos^].Val := Integer(Value);
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: TOpParam);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^] := Value;
End;

{ TMachine.CallstackPush }
Procedure TMachine.CallstackPush(Value: LongWord);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptCallstackRef;
 Stack[StackPos^].Val := Value;
End;

{ TMachine.StackPop }
Function TMachine.StackPop: TOpParam;
Begin
 if (StackPos^ = 0) Then
  raise eInternalError.Create('Cannot ''pop'' - stack is empty.');

 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''pop'' - pointer is outside of the stack.');

 Result := Stack[StackPos^];
 Dec(StackPos^);
End;

{ TMachine.CallstackPop }
Function TMachine.CallstackPop: LongWord;
Begin
 if (StackPos^ = 0) Then
  raise eInternalError.Create('Cannot ''pop'' - stack is empty.');

 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''pop'' - pointer is outside of the stack.');

 Result := Stack[StackPos^].getReference;
 Dec(StackPos^);
End;

{ TMachine.getPosition }
Function TMachine.getPosition: LongWord;
Begin
 Result := LongWord(Position) - LongWord(CodeData);
End;

{ TMachine.setPosition }
Procedure TMachine.setPosition(NewPos: LongWord);
Begin
 Position := PByte(NewPos) + LongWord(CodeData);
End;

{ TMachine.getObject }
Function TMachine.getObject(Address: LongWord): TMObject;
Var Obj: TMObject;
Begin
 if (Address = 0) Then
  raise eNullPointerReference.Create('Null pointer reference');

 Try
  Obj := TMObject(Address);
  Obj.Test;
  Exit(Obj);
 Except
  raise eInvalidReference.Create('Not a valid object reference: 0x'+IntToHex(Address, 2*sizeof(LongWord)));
 End;
End;

{ TMachine.getArray }
Function TMachine.getArray(Address: LongWord): TMArray;
Begin
 Result := TMArray(getObject(Address));
End;

{ TMachine.ThrowException }
Procedure TMachine.ThrowException(const Msg: String);
Begin
 if (exception_handler = 0) Then
  raise eThrow.Create(msg) Else
  Begin
   last_exception := Msg;
   setPosition(exception_handler);
  End;
End;

{ TMachine.Create }
Constructor TMachine.Create(const FileName: String);
Var Zip     : TUnzipper;
    FileList: TStringList;
Begin
 Log('Input file: '+FileName);

 if (not FileExists(FileName)) Then // file not found
 Begin
  Writeln('Input file not found: ', FileName);
  Exit;
 End;

 FileList     := TStringList.Create;
 Zip          := TUnzipper.Create;
 Zip.FileName := FileName;

 CodeData  := nil;
 DebugMode := False;

 Try
  Zip.Examine;
  Zip.OnCreateStream := @OnCreateStream;
  Zip.OnDoneStream   := @OnDoneStream;

  { unzip files }
  FileList.Clear;
  FileList.Add('.header');
  Zip.UnzipFiles(FileList);

  FileList.Clear;
  FileList.Add('.bytecode');
  Zip.UnzipFiles(FileList);
 Finally
  Zip.Free;
  FileList.Free;
 End;

 Log('Allocating memory...');
 ExceptionStack := GetMem(EXCEPTIONSTACK_SIZE);
 SetLength(Stack, STACK_SIZE);

 Log('File has been loaded.');
End;

{ TMachine.Prepare }
Procedure TMachine.Prepare;
Begin
 if (CodeData = nil) Then // no file has been loaded
 Begin
  raise eInvalidFile.Create('Cannot execute program: no file has been loaded (or damaged ''code'' section).');
  Exit;
 End;

 setPosition(0);

 exception_handler := 0;
 last_exception    := '';

 ireg[5]  := 0;
 StackPos := @ireg[5];
End;

{ TMachine.Run }
Procedure TMachine.Run;

  // ParseOpcode
  Procedure ParseOpcode; inline;
  Begin
   LastOpcodePos := getPosition;
   Inc(OpcodeNo);

  //{
   Begin
//    Writeln('CODE:0x', IntToHex(getPosition, 8), ' -> ', disasm(getPosition));
//    Readln;
   End;
  //}

   OpcodeTable[TOpcode_E(c_read_byte)](self);
  End;


// function body
Begin
 Log('-- PROGRAM START --');

 if (not is_runnable) Then
 Begin
  Log('Not a runnable program.');
  Log('Stopping...');
  Exit;
 End;

 OpcodeNo := 0;
 exitcode := 0;

 While (true) do
  ParseOpcode;
End;

{ TMachine.RunFunction }
Procedure TMachine.RunFunction(PackageName, FunctionName: String);
Var I: Integer;
Begin
 For I := Low(FunctionList) To High(FunctionList) Do
  if (FunctionList[I].Package = PackageName) and (FunctionList[I].Name = FunctionName) Then
  Begin
   FunctionList[I].Handler(self);
   Exit;
  End;

 raise eInvalidOpcode.Create('Invalid `icall`: '+PackageName+'.'+FunctionName);
End;

{ TMachine.DumpCallstack }
Procedure TMachine.DumpCallstack;
Var eLine: Integer;
    eFile: String;

    Frames    : PPointer; // VM stack frames
    Frame     : Integer = -1;
    FrameCount: Integer;

    Procedure DisplayVMFrames(const Pnt: Integer);
    Var UntilFunction, FDump, FFunction, FLine, FFileName: String;
    Begin
     Try
      UntilFunction := UpperCase('op_'+getOpcodeName(PByte(LongWord(CodeData)+Pnt)^)); // display VM callstack until this function is found
     Except
      UntilFunction := '';
     End;

     While (true) Do
     Begin
      if (Frame > FrameCount) Then // no more frames on the stack
       Exit;

      if (Frame = -1) Then
       FDump := BackTraceStrFunc(ExceptAddr) Else
       FDump := BackTraceStrFunc(Frames[Frame]);
      Inc(Frame);

      FDump := Trim(StringReplace(FDump, '  ', ' ', [rfReplaceAll]));
      { <address> <function name>, line <line> of <file name> }

      Delete(FDump, 1, Pos(' ', FDump));
      FFunction := Trim(Copy(FDump, 1, Pos(',', FDump)-1));

      Delete(FDump, 1, Pos(',', FDump));
      Delete(FDump, 1, Pos('e', FDump)+1); // delete until `line `
      FLine := Trim(Copy(FDump, 1, Pos(' ', FDump)));
      Delete(FDump, 1, Pos(' ', FDump));

      Delete(FDump, 1, Pos('f', FDump)+1); // delete until `of `
      FFileName := Trim(FDump);

      if (FFunction = UntilFunction) Then
       Exit;

      if (FFunction = '') Then
       Writeln('   at 0x', FFileName, ' [VM]') Else
       Writeln('   at ', FFileName, ' (function: ', FFunction, ', line: ', FLine, ') [VM]');
     End;
    End;

Begin
 if (CodeData = nil) Then // no code has been loaded
 Begin
  Writeln('No callstack available.');
  Exit;
 End;

 Frames     := ExceptFrames; // VM except frames
 FrameCount := ExceptFrameCount; // VM except frame count

 FetchLineAndFile(LastOpcodePos, eLine, eFile);

 DisplayVMFrames(LastOpcodePos);

 if (eLine = -1) Then
  Write('   at unknown source (', eFile, ')') Else
  Write('   at ', eFile, ' (line: ', eLine, ')');

 Writeln(' :: ', disasm(LastOpcodePos));

 // @TODO: 10 references max
 if (StackPos <> nil) Then
  While (StackPos^ > 0) Do
  Begin
   if (Stack[StackPos^].Typ = ptCallstackRef) Then
   Begin
    FetchLineAndFile(Stack[StackPos^].getReference, eLine, eFile);

    if (eLine = -1) Then
     Writeln('   ... from unknown source (', eFile, ')') Else
     Writeln('   ... from ', eFile, ' (', eLine, ')');
   End;
   Dec(StackPos^);
  End;

 DisplayVMFrames(0);
End;

{ TMachine.disasm }
Function TMachine.disasm(Pos: LongWord): String;
Var TmpPos: LongWord;
    Opcode: Byte;
    I     : Integer;
    Param : TOpParam;
Begin
 if (CodeData = nil) Then
  Exit('invalid opcode');

 Try
  TmpPos := getPosition;
  setPosition(Pos);

  Opcode := c_read_byte;

  if (Opcode > OPCODE_MAX) Then
   Exit('invalid opcode');

  Result := getOpcodeName(Opcode)+'(';

  For I := 1 To OpcodesParamCount[TOpcode_E(Opcode)] Do
  Begin
   if (I <> 1) Then
    Result += ', ';

   Param := read_param;

   With Param do
    Case Typ of
     ptBoolReg     : if (Index = 5) Then Result += 'if' Else Result += 'eb'+IntToStr(Index);
     ptCharReg     : Result += 'ec'+IntToStr(Index);
     ptIntReg      : if (Index = 5) Then Result += 'stp' Else Result += 'ei'+IntToStr(Index);
     ptFloatReg    : Result += 'ef'+IntToStr(Index);
     ptStringReg   : Result += 'es'+IntToStr(Index);
     ptReferenceReg: Result += 'er'+IntToStr(Index);

     ptBool  : Result += BoolToStr(Val <> 0, 'true', 'false');
     ptChar  : Result += '#'+IntToStr(Val);
     ptInt   : Result += IntToStr(Val);
     ptFloat : Result += FloatToStr(fVal);
     ptString: Result += '"'+sVal+'"';

     ptStackVal: Result += '['+IntToStr(Val)+']';

     else Result += '<invalid parameter>';
    End;
  End;

  Result += ')';

  setPosition(TmpPos);
 Except
  On E: Exception Do
   Exit('invalid opcode -> '+E.Message);
 End;
End;

{ TMachine.FetchLineAndFile }
Procedure TMachine.FetchLineAndFile(const Pos: LongWord; out eLine: Integer; out eFile: String);
Var Opcode, Param: Byte;
Begin
 eLine := -1;
 eFile := '0x'+IntToHex(Pos, sizeof(LongWord)*2);

 if (CodeData = nil) Then
  Exit;

 setPosition(0);

 While (getPosition <= Pos) Do
 Begin
  Opcode := c_read_byte;

  if (Opcode > OPCODE_MAX) Then
   Exit;

  if (Opcode = ord(o_location)) Then
  Begin
   eLine := read_param.getInt;
   eFile := read_param.getString;
   Continue;
  End;

  For Param := 1 To OpcodesParamCount[TOpcode_E(Opcode)] Do
   read_param;
 End;
End;
End.
