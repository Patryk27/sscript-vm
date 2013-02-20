(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODE OBJFPC}
{$H+}
{$MODESWITCH ADVANCEDRECORDS}

Unit Machine;

 Interface
 Uses SysUtils, Opcodes, Objects;

 Const Header: Array[0..2] of Byte = ($53, $53, $04);
       SectionNames: Array[0..6] of String = ('unusable', 'info', 'unknown', 'exports', 'unknown', 'code', 'debug data');

       { section types }
       section_UNUSABLE = 0;
       section_INFO     = 1;
       section_EXPORTS  = 3;
       section_CODE     = 5;
       section_DEBUG    = 6;

       CALLSTACK_SIZE = 1000000; // in elements

 Const TYPE_BOOL   = 2; // do not modify these, as they have to be exactly the same, as in the compiler
       TYPE_CHAR   = 3;
       TYPE_INT    = 4;
       TYPE_FLOAT  = 5;
       TYPE_STRING = 6;

       TypeSize: Array[TYPE_BOOL..TYPE_STRING] of Byte = (sizeof(Boolean), sizeof(Char), sizeof(Integer), sizeof(Extended), sizeof(String));

 Type TSection = Record
                  Typ            : Byte;
                  Length, DataPnt: LongWord;

                  Data: Pointer;
                 End;

 { 'info' section }
 Type PInfoSectionData = ^TInfoSectionData;
      TInfoSectionData = Packed Record
                          StackSize, EntryPoint: LongWord;
                         End;

 { 'exports' section }
 Type PExport = ^TExport;
      TExport = Packed Record
                 FuncName, Position: LongWord;
                End;

 Type PExportSectionData = ^TExportSectionData;
      TExportSectionData = Packed Record
                            ExportCount: Word;
                            ExportList : Array[0..1024] of TExport;
                           End;

 { 'debug data' section }
 Type PLabel = ^TLabel;
      TLabel = Packed Record
                Name, Position: LongWord;
               End;

 Type PDebugSection = ^TDebugSection;
      TDebugSection = Packed Record
                       LabelsCount: LongWord;
                       LabelList  : Array[0..10240] of TLabel; // FPC gets crazy when I use 'PLabel'...
                      End;

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
                   Function getInt: Integer; inline;
                   Function getLongword: LongWord; inline;
                   Function getFloat: Extended; inline;
                   Function getString: String; inline;
                   Function getReference: LongWord; inline;

                   Function getBoolReg: Pointer;
                   Function getCharReg: Pointer;
                   Function getIntReg: Pointer;
                   Function getFloatReg: Pointer;
                   Function getStringReg: Pointer;
                   Function getReferenceReg: Pointer;

                   Function getTypeName: String;
                  End;

 { virtual machine class }
 Type TMachine = Class
                  Private
                   InfoSectionD: TInfoSectionData;

                  Public
                   // variables
                   ExportSectionD: TExportSectionData;

                   InputFile : String;
                   FileBuffer: PByte;
                   Code      : PByte; // points at `SectionList[SectionCode].Data`
                   Callstack : PLongWord;
                   Stack     : Array of TOpParam;
                   OpcodeNo  : QWord;

                   breg: Array[1..5] of Boolean;
                   creg: Array[1..4] of Char;
                   ireg: Array[1..5] of Integer;
                   freg: Array[1..4] of Extended;
                   sreg: Array[1..4] of String;
                   rreg: Array[1..4] of Integer;

                   Position     : PByte; // current position in `Code` section
                   LastOpcodePos: LongWord;
                   CallstackPos : LongWord;
                   StackPos     : PLongWord;
                   DebugMode    : Boolean;

                   SectionList: Array of TSection;
                   InfoSection, CodeSection, ExportsSection, ImportsSection: Integer;

                  Public
                   // methods

                   { reading }
                   Function c_read_byte: Byte; inline;
                   Function c_read_integer: Integer; inline;
                   Function c_read_longword: LongWord; inline;
                   Function c_read_extended: Extended; inline;
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
                   Function StackPop: TOpParam;

                   { position-related }
                   Function getPosition: LongWord; inline;
                   Procedure setPosition(NewPos: LongWord); inline;

                   { object-handling-related }
                   Function getObject(Address: LongWord): TMObject;
                   Function getArray(Address: LongWord): TMArray;

                   { some stuff }
                   Constructor Create(fFileBuffer: Pointer; BufferSize: LongWord);
                   Constructor Create(FileName: String);
                   Procedure Prepare;
                   Procedure Run;
                   Procedure RunFunction(PackageName, FunctionName: String);

                   Function findSection(Typ: Byte): Integer;
                   Function disasm(Pos: LongWord): String;
                   Function FetchLabelName(Pos: LongWord; out Str: String): Boolean;
                  End;

 Type TOpcodeProc = Procedure (M: TMachine);
      THandlerProc = Procedure (M: TMachine);

 Var FunctionList: Array of Record
                             Name, Package: String;
                             Handler      : THandlerProc;
                            End;

 Procedure Log(Txt: String);
 Procedure NewFunction(PackageName, FunctionName: String; fHandler: THandlerProc);

 Var VerboseMode: Boolean=True;

 Implementation
Uses Procs, mOutput, mString, mMath, mTime, mInput, mVM;

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
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> bool');
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
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> char');
End;

{ TOpParam.getInt }
Function TOpParam.getInt: Integer;
Begin
 if (Typ in [ptInt, ptIntReg, ptBool, ptBoolReg, ptChar, ptCharReg, ptReferenceReg]) Then
  Exit(Val) Else
 if (Typ in [ptFloat, ptFloatReg]) Then
  Exit(Round(fVal)) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getInt) Else
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int');
End;

{ TOpParam.getLongword }
Function TOpParam.getLongword: LongWord;
Begin
 if (Typ in [ptInt, ptIntReg, ptReferenceReg]) Then
  Exit(Val) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getInt) Else
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int (longword)');
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
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> float');
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
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> string');
End;

{ TOpParam.getReference }
Function TOpParam.getReference: LongWord;
Begin
 if (Typ in [ptInt, ptIntReg, ptReferenceReg]) Then
  Exit(Val) Else
 if (Typ = ptStackVal) Then
  Exit(M.Stack[M.StackPos^+Val].getReference) Else
  raise Exception.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int');
End;

{ TOpParam.getBoolReg }
Function TOpParam.getBoolReg: Pointer;
Begin
 if (Typ = ptBoolReg) Then
  Exit(@M.breg[Index]) Else
  raise Exception.Create('Expected register of type ''bool'' while type '''+PrimaryTypeNames[Typ]+''' found');
End;

{ TOpParam.getCharReg }
Function TOpParam.getCharReg: Pointer;
Begin
 if (Typ = ptCharReg) Then
  Exit(@M.creg[Index]) Else
  raise Exception.Create('Expected register of type ''char'' while type '''+PrimaryTypeNames[Typ]+''' found');
End;

{ TOpParam.getIntReg }
Function TOpParam.getIntReg: Pointer;
Begin
 if (Typ = ptIntReg) Then
  Exit(@M.ireg[Index]) Else
  raise Exception.Create('Expected register of type ''int'' while type '''+PrimaryTypeNames[Typ]+''' found');
End;

{ TOpParam.getFloatReg }
Function TOpParam.getFloatReg: Pointer;
Begin
 if (Typ = ptFloatReg) Then
  Exit(@M.freg[Index]) Else
  raise Exception.Create('Expected register of type ''float'' while type '''+PrimaryTypeNames[Typ]+''' found');
End;

{ TOpParam.getStringReg }
Function TOpParam.getStringReg: Pointer;
Begin
 if (Typ = ptStringReg) Then
  Exit(@M.sreg[Index]) Else
  raise Exception.Create('Expected register of type ''string'' while type '''+PrimaryTypeNames[Typ]+''' found');
End;

{ TOpParam.getReferenceReg }
Function TOpParam.getReferenceReg: Pointer;
Begin
 if (Typ = ptReferenceReg) Then
  Exit(@M.rreg[Index]) Else
  raise Exception.Create('Expected register of type ''reference'' while type '''+PrimaryTypeNames[Typ]+''' found');
End;

{ TOpParam.getTypeName }
Function TOpParam.getTypeName: String;
Begin
 Result := PrimaryTypeNames[Typ];

 if (Typ = ptStackVal) Then
  Result += ' ('+M.Stack[M.StackPos^+Val].getTypeName+')';
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
 Result := PInteger(Position)^;
 Inc(Position, sizeof(Integer));
End;

{ TMachine.c_read_longword }
Function TMachine.c_read_longword: LongWord;
Begin
 Result := PLongWord(Position)^;
 Inc(Position, sizeof(LongWord));
End;

{ TMachine.c_read_extended }
Function TMachine.c_read_extended: Extended;
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

 With SectionList[CodeSection] do
 Begin
  if (Pos >= Length) Then
   raise Exception.Create('String address is above available address space. Tried to read string at CODE:0x'+IntToHex(Pos, 2*sizeof(Pos))+' | FILE:0x'+IntToHex(SectionList[CodeSection].DataPnt+Pos, 8));

  Ch := Data + Pos;

  While (Ch^ <> #0) do // fetch string until the terminator (null) char
  Begin
   Result += Ch^;
   Inc(Ch);
  End;
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
  ptBoolReg: Result.Val := Int64(breg[Result.Index]);
  ptCharReg: Result.Val := ord(creg[Result.Index]);
  ptIntReg: Result.Val := ireg[Result.Index];
  ptFloatReg: Result.fVal := freg[Result.Index];
  ptStringReg: Result.sVal := sreg[Result.Index];
  ptReferenceReg: Result.Val := rreg[Result.Index];

  ptBool: Result.Val := c_read_byte;
  ptChar: Result.Val := c_read_byte;
  ptFloat: Result.fVal := c_read_extended;
  ptString: Result.sVal := c_read_string;

  else Result.Val := c_read_integer;
 End;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Integer);
Begin
 if (StackPos^ >= InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptInt;
 Stack[StackPos^].Val := Value;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Extended);
Begin
 if (StackPos^ >= InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ  := ptFloat;
 Stack[StackPos^].fVal := Value;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: String);
Begin
 if (StackPos^ >= InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ  := ptString;
 Stack[StackPos^].sVal := Value;
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Char);
Begin
 if (StackPos^ >= InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptChar;
 Stack[StackPos^].Val := ord(Value);
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: Boolean);
Begin
 if (StackPos^ >= InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^].Typ := ptBool;
 Stack[StackPos^].Val := Integer(Value);
End;

{ TMachine.StackPush }
Procedure TMachine.StackPush(Value: TOpParam);
Begin
 if (StackPos^ >= InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^] := Value;
End;

{ TMachine.StackPop }
Function TMachine.StackPop: TOpParam;
Begin
 if (StackPos^ = 0) Then
  raise Exception.Create('Cannot ''pop'' - stack is empty.');

 if (StackPos^ > InfoSectionD.StackSize) Then
  raise Exception.Create('Cannot ''pop'' - stack position is above available stack.');

 Result := Stack[StackPos^];
 Dec(StackPos^);
End;

{ TMachine.getPosition }
Function TMachine.getPosition: LongWord;
Begin
 Result := LongWord(Position) - LongWord(SectionList[CodeSection].Data);
End;

{ TMachine.setPosition }
Procedure TMachine.setPosition(NewPos: LongWord);
Begin
 Position := PByte(NewPos) + LongWord(SectionList[CodeSection].Data);
End;

{ TMachine.getObject }
Function TMachine.getObject(Address: LongWord): TMObject;
Var Obj: TMObject;
Begin
 Try
  Obj := TMObject(Address);

  if (Obj.getMagic = MagicNumber) Then
   Exit(Obj) Else
   raise Exception.Create('');
 Except
  raise Exception.Create('Not a valid object reference: 0x'+IntToHex(Address, 2*sizeof(LongWord))+' (invalid magic number: 0x'+IntToHex(Obj.getMagic, 2*sizeof(LongWord))+')');
 End;
End;

{ TMachine.getArray }
Function TMachine.getArray(Address: LongWord): TMArray;
Begin
 Result := TMArray(getObject(Address));
End;

{ TMachine.Create }
Constructor TMachine.Create(fFileBuffer: Pointer; BufferSize: LongWord);
Var I        : LongWord;
    RHeader  : Array[0..2] of Byte;
    BufferPos: LongWord=0;

    SectionsCount: Byte;

Function read_byte: Byte;
Begin
 if (BufferPos >= BufferSize) Then
  raise Exception.Create('File damaged.');

 Result := FileBuffer[BufferPos];
 Inc(BufferPos, sizeof(Byte));
End;

Function read_longword: LongWord;
Begin
 if (BufferPos >= BufferSize) Then
  raise Exception.Create('File damaged.');

 Result := PLongword(@FileBuffer[BufferPos])^;
 Inc(BufferPos, sizeof(LongWord));
End;

Procedure read_section(ID: Byte);
Begin
 With SectionList[ID] do
 Begin
  Typ     := read_byte;
  Length  := read_longword;
  DataPnt := read_longword;

  if (Typ > High(SectionNames)) Then
   raise Exception.Create('Section #'+IntToStr(ID)+' is damaged (invalid type: '+IntToStr(Typ)+').');

  if (DataPnt+Length > BufferSize) Then
   raise Exception.Create('Section #'+IntToStr(ID)+' is damaged (invalid data pointer: '+IntToStr(DataPnt)+'; length = '+IntToStr(Length)+').');

  Data := @FileBuffer[DataPnt];

  Case Typ of
   section_INFO:
   if (InfoSection = -1) Then
    InfoSection := ID Else
    Log('[Warning] Multiple ''info'' sections!');

   section_EXPORTS:
   if (ExportsSection = -1) Then
    ExportsSection := ID Else
    Log('[Warning] Multiple ''exports'' sections!');

   section_CODE:
   if (CodeSection = -1) Then
    CodeSection := ID Else
    Log('[Warning] Multiple ''code'' sections!');
  End;

  Log('Section #'+IntToStr(ID)+' ('+SectionNames[Typ]+'); type='+IntToStr(Typ)+'; length='+IntToStr(Length)+'; datapnt='+IntToStr(DataPnt));
  if (DataPnt = BufferSize) Then
   Log('[Warning] Section #'+IntToStr(ID)+' points at the end of the file, so it''s pretty unusable...');
 End;
End;

Begin
 FileBuffer := fFileBuffer;
 Code       := nil;

 InfoSection    := -1;
 CodeSection    := -1;
 ExportsSection := -1;
 ImportsSection := -1;

 DebugMode := False;

 Try
  { check header }
  Log('Reading header...');
  For I := Low(RHeader) To High(RHeader) Do
  Begin
   RHeader[I] := read_byte;
   Log('0x'+IntToHex(RHeader[I], 2));
  End;

  Log('Checking header...');
  For I := Low(RHeader) To High(RHeader) Do
   if (RHeader[I] <> Header[I]) Then
    raise Exception.Create('Invalid header!');

  { read header's data }
  SectionsCount := read_byte;
  Log('Sections count = '+IntToStr(SectionsCount));

  if (SectionsCount = 0) Then
   raise Exception.Create('No sections found!');

  SetLength(SectionList, SectionsCount);
  Dec(SectionsCount);

  { read sections }
  Log('Reading sections...');
  For I := 0 To SectionsCount Do
   read_section(I);

  if (InfoSection = -1) Then
   raise Exception.Create('''info'' section not found!');
  if (CodeSection = -1) Then
   raise Exception.Create('''code'' section not found!');

  { parse 'info' section }
  Log('Parsing ''info'' section...');

  InfoSectionD := PInfoSectionData(SectionList[InfoSection].Data)^;

  With InfoSectionD do
  Begin
   Log('Stack size: '+IntToStr(StackSize));
   Log('Entry point: 0x'+IntToHex(EntryPoint, sizeof(EntryPoint)*2));

   SetLength(Stack, StackSize);
  End;

  if (ExportsSection > -1) Then
  Begin
   { parse 'exports' section }
   Log('Parsing ''exports'' section...');

   ExportSectionD := PExportSectionData(SectionList[ExportsSection].Data)^;
  End Else
   ExportSectionD.ExportCount := 0;
 Finally
 End;

 Code := SectionList[CodeSection].Data;

 Log('File has been loaded.');
End;

{ TMachine.Create }
Constructor TMachine.Create(FileName: String);
Var Input : File of Byte;
    Buffer: PByte;
Begin
 Log('-- create from file --');

 Log('Input file: '+FileName);

 if (not FileExists(FileName)) Then // file not found
 Begin
  Writeln('Input file not found: ', FileName);
  Exit;
 End;

 InputFile := FileName;

 AssignFile(Input, FileName);
 Reset(Input);
 Log('Loading data to the buffer ('+IntToStr(FileSize(Input))+' bytes)...');
 Buffer := AllocMem(FileSize(Input));
 BlockRead(Input, Buffer[0], FileSize(Input));
 Create(Buffer, FileSize(Input));
 CloseFile(Input);

 Log('Allocating memory...');
 Callstack := GetMem(sizeof(LongWord)*CALLSTACK_SIZE);

 Log('Load done.');
End;

{ TMachine.Prepare }
Procedure TMachine.Prepare;
Begin
 if (Code = nil) Then // no file has been loaded
 Begin
  raise Exception.Create('Cannot execute program: no file has been loaded (or damaged ''code'' section).');
  Exit;
 End;

 setPosition(InfoSectionD.EntryPoint);
 CallstackPos := 0;

 ireg[5]  := 0; // mov(stp, 0)
 StackPos := @ireg[5];

 if (getPosition >= SectionList[CodeSection].Length) Then
  Exit;
End;

{ TMachine.Run }
Procedure TMachine.Run;

Procedure ParseOpcode; inline;
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
 @op_, // @TODO
 @op_OBJFREE);
Begin
 LastOpcodePos := getPosition;
 Inc(OpcodeNo);

{
 Begin
  Writeln('CODE:0x', IntToHex(getPosition, 8), ', FILE:', IntToHex(getPosition+SectionList[CodeSection].DataPnt, 8), ' -> ', disasm(getPosition));
  Readln;
 End;
}

 OpcodeTable[TOpcode_E(c_read_byte)](self);
// TOpcodeProc(Pointer(PPointer(LongWord(@OpcodeTable[TOpcode_E(0)])+c_read_byte*sizeof(Pointer)))^)(self); // magic!
End;

Begin
 Log('-- PROGRAM START --');

 OpcodeNo := 0;

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
 raise Exception.Create('Invalid `icall`: '+PackageName+'.'+FunctionName);
End;

{ TMachine.findSection }
Function TMachine.findSection(Typ: Byte): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := 0 To High(SectionList) Do
  if (SectionList[I].Typ = Typ) Then
   Exit(I);
End;

{ TMachine.disasm }
Function TMachine.disasm(Pos: LongWord): String;
Var TmpPos: LongWord;
    Opcode: Byte;
    I     : Integer;
    Param : TOpParam;
Begin
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
     ptFloat : Result += FloatToStr(Val);
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

{ TMachine.FetchLabelName }
Function TMachine.FetchLabelName(Pos: LongWord; out Str: String): Boolean;
Var ID    : Integer;
    DebugS: PDebugSection;
    I     : Integer;
Begin
 Result := False;

 ID := findSection(section_DEBUG); // find 'debug' section
 if (ID = -1) Then // section not found
  Exit;

 DebugS := SectionList[ID].Data;

 With DebugS^ do
  For I := 0 To LabelsCount-1 Do
   if (LabelList[I].Position = Pos) Then
   Begin
    Str := getString(LabelList[I].Name);
    Exit(True);
   End;
End;
End.
