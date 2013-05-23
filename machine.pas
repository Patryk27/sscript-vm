(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)

{$MODE OBJFPC}
{$MODESWITCH ADVANCEDRECORDS}
{$H+}

Unit Machine;

 Interface
 Uses SysUtils, Math, Classes, Variants, FGL, Zipper,
      Opcodes, Objects, Exceptions;

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

 { icall-s }
 Type TCallParamType = (cpNone, cpBool, cpChar, cpInt, cpFloat, cpString);
 Type TCallValue = Record
                    Typ  : TCallParamType;
                    Value: Variant;
                   End;
 Type TCallValues = Array of TCallValue;

 Type TCallHandler = Procedure (M: TMachine; Params: TCallValues; var Result: TCallValue);

 Type PCall = ^TCall;
      TCall = Record
               Package, Func, Full: String;
               ParamCount         : Byte;
               Handler            : TCallHandler;
              End;

 Type TCallList = specialize TFPGList<PCall>;

 { opcode's parameters }
 Type POpParam = ^TOpParam;
      TOpParam = Record
                  Public
                   M    : TMachine; // point to `TMachine`
                   Typ  : TPrimaryType; // parameter type
                   Index: Byte; // register index (if used)
                   Value: Record // value of parameter
                           Int  : Int64;
                           Float: Extended;
                           Str  : String;
                          End;

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

                   Function c_read_byte: Byte; inline;
                   Function c_read_integer: Integer; inline;
                   Function c_read_longword: LongWord; inline;
                   Function c_read_int64: Int64; inline;
                   Function c_read_float: Extended; inline;
                   Function c_read_string: String; inline;

                   Function getString(Pos: LongWord): String;

                   Procedure Prepare;

                  Public
                   // registers
                   breg: Array[1..5] of Boolean;
                   creg: Array[1..4] of Char;
                   ireg: Array[1..5] of Int64;
                   freg: Array[1..4] of Extended;
                   sreg: Array[1..4] of String;
                   rreg: Array[1..4] of LongWord;

                   // fields
                   InputFile: String;

                   CodeData      : PByte; // bytecode data block
                   ExceptionStack: PLongWord;
                   Stack         : Array of TOpParam;

                   icall: TCallList;

                   Position     : PByte; // current position
                   CurrentOpcode: PByte; // current opcode (pointer to its first byte)
                   StackPos     : PLongWord; // points at `ireg[5]` (the `stp` register)
                   ParsedOpcodes: QWord;

                   isRunnable: Boolean;

                   ExceptionHandler: Int64; // must be a signed type!
                   LastException   : String;
                   ExitCode        : Int64;

                  Public
                  // methods
                   Function read_param: TOpParam; {inline (?)}

                   { stack operations }
                   Procedure StackPush(Value: Boolean);
                   Procedure StackPush(Value: Char);
                   Procedure StackPush(Value: Int64);
                   Procedure StackPush(Value: Extended);
                   Procedure StackPush(Value: String);
                   Procedure StackPush(Value: TOpParam);
                   Function StackPop: TOpParam;
                   Procedure CallstackPush(Value: LongWord);
                   Function CallstackPop: LongWord;

                   { position-related }
                   Function getPosition: LongWord; inline;
                   Procedure setPosition(NewPos: LongWord); inline;

                   { object-related }
                   Function getObject(Address: LongWord): TMObject;
                   Function getArray(Address: LongWord): TMArray;

                   Procedure ThrowException(const Msg: String);

                  Public
                   Constructor Create(const FileName: String);

                   Procedure Run(const EntryPoint: LongWord = 0);
                   Procedure AddInternalCall(const Package, Func: String; ParamCount: Byte; Handler: TCallHandler);

                   Function Disassembly(const Pos: LongWord): String;
                   Procedure FetchLocation(const Pos: LongWord; out eLine: Integer; out eFile, eFunc: String);
                  End;

 Var VerboseMode: Boolean=False;

 Implementation
Uses Procs;

// Log
Procedure Log(const Text: String);
Begin
 if (VerboseMode) Then
  Writeln(Text);
End;

{$I opparam.pas}

(* TMachine.OnCreateStream *)
Procedure TMachine.OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Begin
 AStream := TMemoryStream.Create;
End;

(* TMachine.OnDoneStream *)
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

(* TMachine.ParseHeader *)
Procedure TMachine.ParseHeader(const AStream: TStream);
Var MagicNumber               : Longword;
    VersionMajor, VersionMinor: Byte;

    // EndingZero
    Function EndingZero(const Text: String): String;
    Begin
     if (Length(Text) = 1) Then
      Exit(Text+'0') Else
      Exit(Text);
    End;

Begin
 Log('Parsing header...');
 MagicNumber  := BEtoN(AStream.ReadDWord);
 isRunnable   := Boolean(AStream.ReadByte);
 VersionMajor := AStream.ReadByte;
 VersionMinor := AStream.ReadByte;

 Log('Magic number: 0x'+IntToHex(MagicNumber, 8));
 if (MagicNumber <> $0DEFACED) Then
  raise eInvalidFile.Create('Invalid magic number.');

 Log('Bytecode version: '+IntToStr(VersionMajor)+'.'+EndingZero(IntToStr(VersionMinor)));
 if (VersionMajor <> bytecode_version_major) or (VersionMinor <> bytecode_version_minor) Then
  raise eInvalidFile.Create('Unsupported bytecode version.');
End;

(* TMachine.ParseBytecode *)
Procedure TMachine.ParseBytecode(const AStream: TStream);
Var I: LongWord;
Begin
 Log('Parsing bytecode...');

 CodeData := AllocMem(AStream.Size);

 if (AStream.Size <> 0) Then
  For I := 0 To AStream.Size-1 Do // @TODO: why `AStream.Read` doesn't work? :|
   CodeData[I] := AStream.ReadByte;
End;

(* TMachine.c_read_byte *)
Function TMachine.c_read_byte: Byte;
Begin
 Result := Position^;
 Inc(Position, sizeof(Byte));
End;

(* TMachine.c_read_integer *)
Function TMachine.c_read_integer: Integer;
Begin
 Result := BEtoN(PInteger(Position)^);
 Inc(Position, sizeof(Integer));
End;

(* TMachine.c_read_longword *)
Function TMachine.c_read_longword: LongWord;
Begin
 Result := BEtoN(PLongWord(Position)^);
 Inc(Position, sizeof(LongWord));
End;

(* TMachine.c_read_int64 *)
Function TMachine.c_read_int64: Int64;
Begin
 Result := BEtoN(PInt64(Position)^);
 Inc(Position, sizeof(Int64));
End;

(* TMachine.c_read_float *)
Function TMachine.c_read_float: Extended;
Begin
 Result := PExtended(Position)^;
 Inc(Position, sizeof(Extended));
End;

(* TMachine.c_read_string *)
Function TMachine.c_read_string: String;
Begin
 Result := '';

 While (Position^ <> 0) Do // read until terminator char (0x00)
 Begin
  Result += chr(Position^);
  Inc(Position);
 End;

 Inc(Position);
End;

(* TMachine.getString *)
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

{ TMachine.Prepare }
Procedure TMachine.Prepare;
Begin
 if (CodeData = nil) Then // no file has been loaded
  raise eInvalidFile.Create('Cannot execute program: no file has been loaded (or damaged ''code'' section).');

 setPosition(0);

 ExceptionHandler := -1;
 LastException    := '';

 ireg[5]  := 0;
 StackPos := @ireg[5];

 ParsedOpcodes := 0;
 ExitCode      := 0;
End;

// -------------------------------------------------------------------------- //

(* TMachine.read_param *)
Function TMachine.read_param: TOpParam;
Begin
 Result.M   := self;
 Result.Typ := TPrimaryType(c_read_byte);

 if (Result.Typ in [ptBoolReg..ptReferenceReg]) Then
  Result.Index := c_read_byte; // register ID

 Case Result.Typ of
  { register value }
  ptBoolReg     : Result.Value.Int   := Byte(breg[Result.Index]);
  ptCharReg     : Result.Value.Int   := ord(creg[Result.Index]);
  ptIntReg      : Result.Value.Int   := ireg[Result.Index];
  ptFloatReg    : Result.Value.Float := freg[Result.Index];
  ptStringReg   : Result.Value.Str   := sreg[Result.Index];
  ptReferenceReg: Result.Value.Int   := rreg[Result.Index];

  { constant value }
  ptBool  : Result.Value.Int   := c_read_byte;
  ptChar  : Result.Value.Int   := c_read_byte;
  ptInt   : Result.Value.Int   := c_read_int64;
  ptFloat : Result.Value.Float := c_read_float;
  ptString: Result.Value.Str   := c_read_string;

  else
   Result.Value.Int := c_read_integer;
 End;
End;

{$I stack.pas}

(* TMachine.getPosition *)
Function TMachine.getPosition: LongWord;
Begin
 Result := LongWord(Position) - LongWord(CodeData);
End;

(* TMachine.setPosition *)
Procedure TMachine.setPosition(NewPos: LongWord);
Begin
 Position := PByte(NewPos) + LongWord(CodeData);
End;

(* TMachine.getObject *)
Function TMachine.getObject(Address: LongWord): TMObject;
Var Obj: TMObject;
Begin
 if (Address = 0) Then
  ThrowException('Null pointer reference');

 Try
  Obj := TMObject(Address);
  Obj.Test;
  Exit(Obj);
 Except
  ThrowException('Not a valid object reference: 0x'+IntToHex(Address, 2*sizeof(LongWord)));
 End;
End;

(* TMachine.getArray *)
Function TMachine.getArray(Address: LongWord): TMArray;
Begin
 Result := TMArray(getObject(Address));
End;

(* TMachine.ThrowException *)
Procedure TMachine.ThrowException(const Msg: String);
Begin
 if (ExceptionHandler = -1) { no exception handler set } Then
  raise eThrow.Create(msg) Else
  Begin
   LastException := Msg;
   setPosition(ExceptionHandler);
  End;
End;

(* TMachine.Create *)
Constructor TMachine.Create(const FileName: String);
Var Zip     : TUnzipper;
    FileList: TStringList;
Begin
 Log('TMachine.Create()');
 Log('Input file: '+FileName);

 if (not FileExists(FileName)) Then // file not found
  raise eInvalidFile.CreateFmt('Input file not found: %s', [FileName]);

 FileList     := TStringList.Create;
 Zip          := TUnzipper.Create;
 Zip.FileName := FileName;

 CodeData := nil;

 icall := TCallList.Create;

 Try
  Zip.Examine;
  Zip.OnCreateStream := @OnCreateStream;
  Zip.OnDoneStream   := @OnDoneStream;

  { unzip and parse files }
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

(* TMachine.Run *)
Procedure TMachine.Run(const EntryPoint: LongWord=0);
Begin
 if (not isRunnable) Then
  raise eInvalidFile.Create('Cannot directly run a library!');

 Prepare;
 setPosition(EntryPoint);

 Log('-- PROGRAM START (EP='+IntToHex(EntryPoint, 8)+') --');

 While (true) do
 Begin
  Inc(ParsedOpcodes);
  CurrentOpcode := Position;

  OpcodeTable[TOpcode_E(c_read_byte)](self);
 End;
End;

(* TMachine.AddInternalCall *)
Procedure TMachine.AddInternalCall(const Package, Func: String; ParamCount: Byte; Handler: TCallHandler);
Var call: PCall;
Begin
 New(call);
 call^.Package    := Package;
 call^.Func       := Func;
 call^.Full       := Package+'.'+Func;
 call^.ParamCount := ParamCount;
 call^.Handler    := Handler;

 icall.Add(call);
End;

(* TMachine.Disassembly *)
Function TMachine.Disassembly(const Pos: LongWord): String;
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

     ptBool  : Result += BoolToStr(Value.Int <> 0, 'true', 'false');
     ptChar  : Result += '#'+IntToStr(Value.Int);
     ptInt   : Result += IntToStr(Value.Int);
     ptFloat : Result += FloatToStr(Value.Float);
     ptString: Result += '"'+Value.Str+'"';

     ptStackVal: Result += '['+IntToStr(Value.Int)+']';

     else
      Result += '<invalid parameter>';
    End;
  End;

  Result += ')';

  setPosition(TmpPos);
 Except
  On E: Exception Do
   Exit('invalid opcode -> '+E.Message);
 End;
End;

(* TMachine.FetchLocation *)
Procedure TMachine.FetchLocation(const Pos: LongWord; out eLine: Integer; out eFile, eFunc: String);
Var Opcode, Param: Byte;
Begin
 eLine := -1;
 eFile := '0x'+IntToHex(Pos, sizeof(LongWord)*2);
 eFunc := '<unknown>';

 if (CodeData = nil) Then
  Exit;

 setPosition(0);

 While (getPosition <= Pos) Do
 Begin
  Opcode := c_read_byte;

  if (Opcode > OPCODE_MAX) Then
   Exit;

  // loc_file
  if (Opcode = ord(o_loc_file)) Then
  Begin
   eFile := read_param.getString;
   Continue;
  End;

  // loc_func
  if (Opcode = ord(o_loc_func)) Then
  Begin
   eFunc := read_param.getString;
   Continue;
  End;

  // loc_line
  if (Opcode = ord(o_loc_line)) Then
  Begin
   eLine := read_param.getInt;
   Continue;
  End;

  // other opcodes
  For Param := 1 To OpcodesParamCount[TOpcode_E(Opcode)] Do
   read_param;
 End;
End;
End.
