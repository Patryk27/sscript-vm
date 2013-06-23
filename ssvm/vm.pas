(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
{$H+}
Unit VM;

 Interface
 Uses FGL, Stack;

 Const VMVersion = '0.3.2';

 Const SupportedBytecode: Record
                           Major, Minor: uint8;
                          End = (Major: 0; Minor: 41); // current bytecode version: 0.41

 Const ExceptionStackSize = 2*1024; // 2 KB
       StackElementCount  = 1000000; // maximum elements count on the stack; now it's about ~65 MB; @TODO: stack dynamic grow!

 Type Puint8  = ^uint8; // for some reason FPC doesn't have them declared
      Pint8   = ^int8;
      Puint16 = ^uint16;
      Pint16  = ^int16;
      Puint32 = ^uint32;
      Pint32  = ^int32;
      Puint64 = ^uint64;
      Pint64  = ^int64;

 Const TYPE_BOOL   = 3; // do not modify
       TYPE_CHAR   = 4;
       TYPE_INT    = 5;
       TYPE_FLOAT  = 6;
       TYPE_STRING = 7;

       TypeSize: Array[TYPE_BOOL..TYPE_STRING] of Byte = (1, 1, 8, 10, 4);

 // -- internal calls -- //
 Type TCallHandler = Procedure (VM: Pointer; Params: PMixedValue; Result: PMixedValue); // single icall handler

 Type PCall = ^TCall;
      TCall = Record
               PackageName, FunctionName, FullName: String; // package name, function name, full name ('package_name.function_name')
               ParamCount                         : uint8; // amount of params needed by function
               Handler                            : TCallHandler; // handler to be called when calling this function
              End;

 Type TCallList = specialize TFPGList<PCall>;

 // -- stop -- //
 Type TStopReason = (srNormal, srException);

 // -- exceptions -- //
 Type TExceptionType = (etNone=-1, etByObject, etByMessage);
 Type PExceptionBlock = ^TExceptionBlock;
      TExceptionBlock = Record
                         Typ : TExceptionType; // exception type ("throw object;" or "throw string;")
                         Data: Pointer;
                        End;

 // -- virtual machine -- //
 Type PVM = ^TVM;
      TVM = Packed Record
             InternalCallList: TCallList; // list of registered icall-s

             CodeData: PByte; // pointer to the bytecode block

             Regs: Record // registers
                    b: Array[1..5] of Boolean;  // eb1, eb2, eb3, eb4, if
                    c: Array[1..4] of Char;     // ec1, ec2, ec3, ec4
                    i: Array[1..5] of Int64;    // ei1, ei2, ei3, ei4, stp
                    f: Array[1..4] of Extended; // ef1, ef1, ef3, ef4
                    s: Array[1..4] of String;   // es1, es2, es3, es4
                    r: Array[1..4] of Pointer;  // er1, er2, er3, er4
                   End;

             Loader: Record // filled by TLoader
                      MagicNumber               : uint32;
                      isRunnable                : Boolean;
                      VersionMajor, VersionMinor: uint8;
                     End;

             ExceptionStack: Puint32; // exception stack elements
             Stack         : Array of TStackElement; // stack elements

             Position     : PByte; // current position in memory
             CurrentOpcode: PByte; // first byte of currently parsed opcode

             ExceptionHandler: Int64; // current bytecode exception handler (must be a signed type!); `-1` means no handler set
             LastException   : TExceptionBlock; // last exception block
             StackPos        : PInt64; // pointer at `Regs.i[5]` - current stack position

             Stop      : Boolean; // used by the `stop()` opcode
             StopReason: TStopReason;

          (* -- procedures and functions for internal use -- *)
             Procedure SetPosition(const Pos: uint32);
             Function GetPosition: uint32;

             // -- read_* -- //
             Function read_uint8: uint8; inline;
             Function read_uint16: uint16; inline;
             Function read_uint32: uint32; inline;

             Function read_int8: int8; inline;
             Function read_int16: int16; inline;
             Function read_int32: int32; inline;
             Function read_int64: int64; inline;

             Function read_float: Extended; inline;
             Function read_string: String; inline;

             Function read_param: TMixedValue; inline;

             // -- stack operations -- //
             Procedure StackPush(E: TStackElement);
             Function StackPop: TStackElement;

             // -- exception handling -- //
             Procedure ThrowException(Exception: TExceptionBlock);
             Procedure ThrowExceptionByMessage(Msg: PChar);

             // -- object handling -- //
             Function CheckObject(const Address: Pointer): Pointer; inline;
            End;

 Function CopyStringToPChar(const S: String): PChar;

 // -------------------- //
 Procedure VM_Create(VM: Pointer; FileName: PChar); stdcall;
 Procedure VM_Run(VM: Pointer; EntryPoint: uint32); stdcall;
 Procedure VM_AddInternalCall(VM: Pointer; PackageName, FunctionName: PChar; ParamCount: uint8; Handler: TCallHandler); stdcall;
 Procedure VM_StackPush(VM: Pointer; Element: TMixedValue); stdcall;
 Function VM_StackPop(VM: Pointer): TMixedValue; stdcall;
 Procedure VM_ThrowException(VM: Pointer; Exception: TExceptionBlock); stdcall;
 Function VM_GetException(VM: Pointer): TExceptionBlock; stdcall;
 Function VM_GetStopReason(VM: Pointer): TStopReason; stdcall;

 Implementation
Uses SysUtils, Loader, Opcodes, Objects;

// CopyStringToPChar
Function CopyStringToPChar(const S: String): PChar;
Var I: uint32;
Begin
 Result := AllocMem(Length(S)+1);

 For I := 1 To Length(S) Do
  Result[I-1] := S[I];
End;

// -------------------------------------------------------------------------- //
(* VM_Create *)
{
 Sets fields to their default values, allocates memory etc. and loads the specified bytecode file.
 `VM` must points at already allocated memory area!
}
Procedure VM_Create(VM: Pointer; FileName: PChar); stdcall;
Var LoaderClass: TLoader;
Begin
 With PVM(VM)^ do
 Begin
  InternalCallList := TCallList.Create;

  // try to load and parse file
  LoaderClass := TLoader.Create(VM, FileName);
  Try
   LoaderClass.Load;
  Finally
   LoaderClass.Free;
  End;

  if (CodeData = nil) Then
   raise Exception.Create('Couldn''t load the specified bytecode file!');

  SetLength(Stack, StackElementCount);
  ExceptionStack := GetMem(ExceptionStackSize);
 End;
End;

(* VM_Run *)
{
 Runs the program from specified point ('EntryPoint').
}
Procedure VM_Run(VM: Pointer; EntryPoint: uint32); stdcall;
Begin
 With PVM(VM)^ do
 Begin
  if (CodeData = nil) Then
   raise Exception.Create('Couldn''t run program: no code has been loaded!');

 // if (not Loader.isRunnable) Then
 //  raise Exception.Create('Cannot directly run a library!');

  ExceptionHandler  := -1;
  LastException.Typ := etNone;

  SetPosition(EntryPoint);

  StopReason := srNormal;

  StackPos  := @Regs.i[5];
  StackPos^ := 0;

  Stop := False;

  While (not Stop) Do
  Begin
   CurrentOpcode := Position;

  // Writeln('0x', IntToHex(Int64(CurrentOpcode), 2*sizeof(int64)), ' -> ', IntToHex(CurrentOpcode^, 2), ' -> ', TOpcode_E(CurrentOpcode^));
   OpcodeTable[TOpcode_E(read_uint8)](VM);
  // Case read_uint8 of
  // End;
  End;
 End;
End;

(* VM_AddInternalCall *)
{
 Adds new internal call onto the list.
}
Procedure VM_AddInternalCall(VM: Pointer; PackageName, FunctionName: PChar; ParamCount: uint8; Handler: TCallHandler); stdcall;
Var icall: PCall;
Begin
 New(icall);
 icall^.PackageName  := PackageName;
 icall^.FunctionName := FunctionName;
 icall^.FullName     := PackageName+'.'+FunctionName;
 icall^.ParamCount   := ParamCount;
 icall^.Handler      := Handler;

 With PVM(VM)^ do
  InternalCallList.Add(icall);
End;

(* VM_StackPush *)
{
 Pushes new value onto the stack.
}
Procedure VM_StackPush(VM: Pointer; Element: TMixedValue); stdcall;
Begin
 With PVM(VM)^ do
 Begin
  Stack[StackPos^] := Element;
  Inc(StackPos^);
 End;
End;

(* VM_StackPop *)
{
 'Pops' a value from the stack.
}
Function VM_StackPop(VM: Pointer): TMixedValue; stdcall;
Begin
 With PVM(VM)^ do
 Begin
  Dec(StackPos^);
  Result := Stack[StackPos^];
 End;
End;

(* VM_ThrowException *)
{
 Throws a catchable VM exception.
}
Procedure VM_ThrowException(VM: Pointer; Exception: TExceptionBlock); stdcall;
Begin
 With PVM(VM)^ do
  ThrowException(Exception);
End;

(* VM_GetException *)
{
 Returns the latest user code exception.
}
Function VM_GetException(VM: Pointer): TExceptionBlock; stdcall;
Begin
 With PVM(VM)^ do
  Exit(LastException);
End;

(* VM_GetStopReason *)
{
 Returns VM stop reason.
}
Function VM_GetStopReason(VM: Pointer): TStopReason; stdcall;
Begin
 With PVM(VM)^ do
  Exit(StopReason);
End;

// ========================================================================== //
(* TVM.SetPosition *)
{
 Sets the bytecode position at `CodeData+Pos`
}
Procedure TVM.SetPosition(const Pos: uint32);
Begin
 Position := PByte(uint64(CodeData)+Pos);
End;

(* TVM.GetPosition *)
{
 Gets the relative current position: `Position-CodeData`
}
Function TVM.GetPosition: uint32;
Begin
 Result := uint32(uint64(Position)-uint64(CodeData));
End;

(* TVM.StackPush *)
{
 Pushes an element onto the stack.
}
Procedure TVM.StackPush(E: TStackElement);
Begin
 Stack[StackPos^] := E;
 Inc(StackPos^);
End;

(* TVM.StackPop *)
{
 'Pops' an element from the stack.
}
Function TVM.StackPop: TStackElement;
Begin
 Dec(StackPos^);
 Result := Stack[StackPos^];
End;

(* TVM.read_uint8 *)
{
 Reads an uint8
}
Function TVM.read_uint8: uint8;
Begin
 Result := Position^;
 Inc(Position, sizeof(Result));
End;

(* TVM.read_uint16 *)
{
 Reads an uint16
}
Function TVM.read_uint16: uint16;
Begin
 Result := BEtoN(Puint16(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVM.read_uint32 *)
{
 Reads an uint32
}
Function TVM.read_uint32: uint32;
Begin
 Result := BEtoN(Puint32(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVM.read_int8 *)
{
 Reads an int8
}
Function TVM.read_int8: int8;
Begin
 Result := Position^;
 Inc(Position, sizeof(Result));
End;

(* TVM.read_int16 *)
{
 Reads an int16
}
Function TVM.read_int16: int16;
Begin
 Result := BEtoN(Pint16(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVM.read_int32 *)
{
 Reads an int32
}
Function TVM.read_int32: int32;
Begin
 Result := BEtoN(Pint32(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVM.read_int64 *)
{
 Reads an int64
}
Function TVM.read_int64: int64;
Begin
 Result := BEtoN(Pint64(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVM.read_float *)
{
 Reads a float
}
Function TVM.read_float: Extended;
Begin
 Result := PExtended(Position)^;

 Inc(Position, 10);
End;

(* TVM.read_string *)
{
 Reads a string
}
Function TVM.read_string: String;
Begin
 Result := '';

 While (Position^ <> 0) Do
 Begin
  Result += chr(Position^);
  Inc(Position);
 End;

 Inc(Position); // skip the null termintor char
End;

(* TVM.read_param *)
{
 Reads an opcode's param
}
Function TVM.read_param: TMixedValue;
Type TBytecodeArgType = (btBoolReg=0, btCharReg, btIntReg, btFloatReg, btStringReg, btReferenceReg,
                         btBool, btChar, btInt, btFloat, btString, btStackVal);
Var Typ: TBytecodeArgType;
Begin
 Typ := TBytecodeArgType(read_uint8);

 if (Typ in [btBoolReg..btReferenceReg]) Then // register
 Begin
  Result.isReg    := True;
  Result.RegIndex := read_uint8;
 End Else
  Result.isReg := False;

 Case Typ of
  btBoolReg, btBool    : Result.Typ := mvBool;
  btCharReg, btChar    : Result.Typ := mvChar;
  btIntReg, btInt      : Result.Typ := mvInt;
  btFloatReg, btFloat  : Result.Typ := mvFloat;
  btStringReg, btString: Result.Typ := mvString;
  btReferenceReg       : Result.Typ := mvReference;

  else
   Result.Typ := mvInt;
 End;

 Case Typ of
  { register value }
  btBoolReg     : Result.Value.Bool  := Regs.b[Result.RegIndex];
  btCharReg     : Result.Value.Char  := Regs.c[Result.RegIndex];
  btIntReg      : Result.Value.Int   := Regs.i[Result.RegIndex];
  btFloatReg    : Result.Value.Float := Regs.f[Result.RegIndex];
  btStringReg   : Result.Value.Str   := CopyStringToPChar(Regs.s[Result.RegIndex]);
  btReferenceReg: Result.Value.Int   := uint64(Regs.r[Result.RegIndex]);

  { constant value }
  btBool  : Result.Value.Bool  := Boolean(read_uint8);
  btChar  : Result.Value.Char  := chr(read_uint8);
  btInt   : Result.Value.Int   := read_int64;
  btFloat : Result.Value.Float := read_float;
  btString: Result.Value.Str   := CopyStringToPChar(read_string);

  else
   Result.Value.Int := read_int32;
 End;

 if (Typ = btStackval) Then // stackval?
 Begin
  Result.Stackval   := @Stack[StackPos^+Result.Value.Int-1];
  Result.Typ        := Result.Stackval^.Typ;
  Result.Value      := Result.Stackval^.Value;
  Result.isStackval := True;
 End Else
  Result.isStackval := False;
End;

(* TVM.ThrowException *)
{
 Throws an exception.
}
Procedure TVM.ThrowException(Exception: TExceptionBlock);
Begin
 Case Exception.Typ of
  { throw (by) object }
  etByObject:
   raise SysUtils.Exception.Create('Exception-by-object throwing has not been implemented yet!');

  { throw (by) message (string) }
  etByMessage:
  Begin
   if (ExceptionHandler = -1) Then // no exception handler set
   Begin
    LastException := Exception;
    StopReason    := srException;
    Stop          := True;
    Exit;
   End;

   LastException := Exception;
   SetPosition(ExceptionHandler);
  End;
 End;
End;

(* TVM.ThrowExceptionByMessage *)
{
 Throws a message exception.
}
Procedure TVM.ThrowExceptionByMessage(Msg: PChar);
Var EB: TExceptionBlock;
Begin
 EB.Typ  := etByMessage;
 EB.Data := Msg;
 ThrowException(EB);
End;

(* TVM.CheckObject *)
{
 Checks if passed reference is a valid object.
}
Function TVM.CheckObject(const Address: Pointer): Pointer;
Begin
 Result := Address;

 if (Address = nil) Then
  ThrowExceptionByMessage('Null pointer reference');

 Try
  TMObject(Address).Test;
 Except
  ThrowExceptionByMessage(PChar('Not a valid object reference: 0x'+IntToHex(uint32(Address), 2*sizeof(uint32))));
 End;
End;
End.
