(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$DEFINE ENABLE_JIT} // when uncommenting, change also the compiler's unit path to point to "jit;jit/<selected architecture>"

{$MODESWITCH ADVANCEDRECORDS}
{$H+}

Unit VM;

 Interface
 Uses Stack, VMTypes, BCLoader, FGL;

 Const VMVersion = '0.3.4 nightly';

 Const ExceptionStackSize = 2*1024; // 2 KB
       StackElementCount  = 1000000; // maximum elements count on the stack; now it's about ~65 MB; @TODO: stack dynamic grow!

 Type TJITCompiledState = (csInvalidBytecode, csJITFailed, csJITUnsupported, csDisabled, csDone);

 Const TYPE_BOOL_id   = 0; // do not modify
       TYPE_CHAR_id   = 1;
       TYPE_INT_id    = 2;
       TYPE_FLOAT_id  = 3;
       TYPE_STRING_id = 4;

       TypeSizes: Array[TYPE_BOOL_id..TYPE_STRING_id] of Byte = (1, 1, 8, 10, 4);

 // -- internal calls -- //
 Type TCallHandler = Procedure (VM: Pointer; Params: PMixedValue; Result: PMixedValue); register; // single icall handler

 Type PCall = ^TCall; // @TODO: TInternalCall/PInternalCall?
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
      TVM = Record
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

             LoaderData: TBCLoaderData; // filled by TBCLoader

             ExceptionStack: Puint32; // exception stack elements
             Stack         : Array of TStackElement; // stack elements

             Position     : PByte; // current position in memory
             CurrentOpcode: PByte; // first byte of currently parsed opcode

             ExceptionHandler: Int64; // current bytecode exception handler (must be a signed type!); `-1` means no handler set
             LastException   : TExceptionBlock; // last exception block
             StackPos        : PInt64; // points at `Regs.i[5]` (current stack position)

             Stop      : Boolean; // used by the `stop()` opcode, if equal `true` - VM stops executing bytecode
             StopReason: TStopReason;

             GarbageCollector: Pointer;

             JITCode     : Pointer;
             JITCodeSize : uint32;
             JITCompiler : Pointer;
             LastJITError: PChar;

          (* -- procedures and functions for internal use -- *)
             Procedure SetPosition(const Pos: uint32);
             Function GetPosition: uint32;

             Function FindInternalCall(const Name: String): PCall;

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
             Procedure StackPush(E: TStackElement); inline;
             Function StackPop: TStackElement; inline;

             // -- exception handling -- //
             Procedure ThrowException(Exception: TExceptionBlock); inline;
             Procedure ThrowExceptionByMessage(Msg: PChar); inline;

             // -- object handling -- //
             Function CheckObject(const Address: Pointer): Pointer; inline;
             Function isValidObject(const Obj: Pointer): Boolean; inline;
            End;

 Procedure VM_Create(VM: Pointer; FileName: PChar; GCMemoryLimit: uint32); stdcall;
 Procedure VM_Run(VM: Pointer); stdcall;
 Function VM_JITCompile(VM: Pointer): TJITCompiledState; stdcall;
 Function VM_GetLastJITError(VM: Pointer): PChar; stdcall;
 Function VM_GetJITCode(VM: Pointer): Pointer; stdcall;
 Function VM_GetJITCodeSize(VM: Pointer): uint32; stdcall;
 Procedure VM_Free(VM: Pointer); stdcall;

 Procedure VM_AddInternalCall(VM: Pointer; PackageName, FunctionName: PChar; ParamCount: uint8; Handler: TCallHandler); stdcall;

 Procedure VM_StackPush(VM: Pointer; Element: TMixedValue); stdcall;
 Function VM_StackPop(VM: Pointer): TMixedValue; stdcall;

 Procedure VM_SetEB(VM: Pointer; RegNum: Byte; RegValue: Boolean); stdcall;
 Procedure VM_SetEC(VM: Pointer; RegNum: Byte; RegValue: Char); stdcall;
 Procedure VM_SetEI(VM: Pointer; RegNum: Byte; RegValue: Int64); stdcall;
 Procedure VM_SetEF(VM: Pointer; RegNum: Byte; RegValue: Extended); stdcall;
 Procedure VM_SetES(VM: Pointer; RegNum: Byte; RegValue: PChar); stdcall;
 Procedure VM_SetER(VM: Pointer; RegNum: Byte; RegValue: Pointer); stdcall;
 Function VM_GetEB(VM: Pointer; RegNum: Byte): Boolean; stdcall;
 Function VM_GetEC(VM: Pointer; RegNum: Byte): Char; stdcall;
 Function VM_GetEI(VM: Pointer; RegNum: Byte): Int64; stdcall;
 Function VM_GetEF(VM: Pointer; RegNum: Byte): Extended; stdcall;
 Function VM_GetES(VM: Pointer; RegNum: Byte): PChar; stdcall;
 Function VM_GetER(VM: Pointer; RegNum: Byte): Pointer; stdcall;

 Procedure VM_ThrowException(VM: Pointer; Exception: TExceptionBlock); stdcall;
 Function VM_GetException(VM: Pointer): TExceptionBlock; stdcall;
 Function VM_GetStopReason(VM: Pointer): TStopReason; stdcall;

 Implementation
Uses SysUtils, Opcodes, Objects, VMStrings,
     GC, OpcodeInterpreter
{$IFDEF ENABLE_JIT}
 , JITCompiler
{$ENDIF};

(* VM_Create *)
{
 Sets fields to their default values, allocates memory etc. and loads the specified bytecode file.
 `VM` must point at already allocated memory area!
}
Procedure VM_Create(VM: Pointer; FileName: PChar; GCMemoryLimit: uint32); stdcall;
Var LoaderClass: TBCLoader;
Begin
 With PVM(VM)^ do
 Begin
  InternalCallList := TCallList.Create;

  // try to load and parse the input file
  LoaderClass := TBCLoader.Create(FileName);
  Try
   LoaderData := LoaderClass.Load^;
  Finally
   CodeData := LoaderData.CodeData;
   LoaderClass.Free;
  End;

  if (CodeData = nil) Then
   raise Exception.Create('Couldn''t load the specified bytecode file!');

  // allocate memory
  SetLength(Stack, StackElementCount);
  ExceptionStack := GetMem(ExceptionStackSize);

  // create garbage collector
  GarbageCollector := TGarbageCollector.Create(VM, GCMemoryLimit);

  // reset variables
  JITCompiler  := nil;
  LastJITError := #0;
 End;
End;

(* VM_Run *)
{
 Executes the program.
}
Procedure VM_Run(VM: Pointer); stdcall;
Type TProcedure = Procedure;
Var I: uint32;
Begin
 With PVM(VM)^ do
 Begin
  Try
   ExceptionHandler   := -1;
   LastException.Typ  := etNone;
   LastException.Data := nil;

   if (CodeData = nil) Then
    raise Exception.Create('Couldn''t run program: no bytecode has been loaded!');

   if (not LoaderData.isRunnable) Then
    raise Exception.Create('File is not runnable!');

   setPosition(0);

   StopReason := srNormal;

   StackPos  := @Regs.i[5];
   StackPos^ := 0;

   For I := Low(Stack) To High(Stack) Do // clear stack
    Stack[I].isMemRef := False;

   if (JITCode <> nil) Then // if possible, execute the JIT compiled code instead of running slow opcode interpreter
   Begin
    TProcedure(JITCode)();
    Exit;
   End;

   Stop := False;

   While (not Stop) Do
   Begin
    CurrentOpcode := Position;
    OpcodeTable[TOpcode_E(read_uint8)](VM);
   End;
  Except
   On E: Exception Do
   Begin
    StopReason := srException;

    if (LastException.Data = nil) Then
    Begin
     LastException.Typ  := etByMessage;
     LastException.Data := CopyStringToPChar(E.Message);
    End;
   End;
  End;
 End;
End;

(* VM_JITCompile *)
{ Executes JIT compiler on already loaded bytecode in specified VM instance }
Function VM_JITCompile(VM: Pointer): TJITCompiledState; stdcall;
{$IFDEF ENABLE_JIT}
Var Compiler: TJITCompiler = nil;
{$ENDIF}
Begin
 {$IFDEF ENABLE_JIT}
  With PVM(VM)^ do
  Begin
   if (JITCompiler <> nil) Then
    TJITCompiler(JITCompiler).Free;

   JITCompiler  := nil;
   LastJITError := #0;

   Try
    Compiler := TJITCompiler.Create(VM); // create compiler instance, load bytecode...
    Result   := Compiler.Compile; // ...and compile it! :)
   Except
    On E: Exception Do
    Begin
     LastJITError := CopyStringToPChar(E.Message);
     Compiler.Free;
     Exit(csJITFailed);
    End;
   End;

   JITCode     := Compiler.getCPU.getCompiledData.Memory;
   JITCodeSize := Compiler.getCPU.getCompiledData.Size;
   JITCompiler := Compiler;
  End;
 {$ELSE}
  PVM(VM)^.LastJITError := CopyStringToPChar('This library has been compiled without a JIT compiler support.');
  Exit(csDisabled);
 {$ENDIF}
End;

(* VM_GetLastJITError *)
{ Returns last JIT compiler error }
Function VM_GetLastJITError(VM: Pointer): PChar; stdcall;
Begin
 Result := PVM(VM)^.LastJITError;
End;

(* VM_GetJITCode *)
{ Return JIT-compiled code pointer }
Function VM_GetJITCode(VM: Pointer): Pointer; stdcall;
Begin
 Result := PVM(VM)^.JITCode;
End;

(* VM_GetJITCodeSize *)
{ Returns JIT-compiled code size }
Function VM_GetJITCodeSize(VM: Pointer): uint32; stdcall;
Begin
 Result := PVM(VM)^.JITCodeSize;
End;

(* VM_Free *)
{
 Releases memory allocated to the VM instance, and the VM instance itself.
}
Procedure VM_Free(VM: Pointer); stdcall;
Var Call: PCall;
Begin
 With PVM(VM)^ do
 Begin
  TGarbageCollector(GarbageCollector).Free;

  For Call in InternalCallList Do
   Dispose(Call);
  InternalCallList.Free;

  Dispose(CodeData);
  Dispose(ExceptionStack);
  SetLength(Stack, 0);

  {$IFDEF ENABLE_JIT}
  if (JITCompiler <> nil) Then
   TJITCompiler(JITCompiler).Free;
  {$ENDIF}
 End;

 Dispose(PVM(VM));
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
  Inc(StackPos^);
  Stack[StackPos^] := Element;
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
  Result := Stack[StackPos^];
  Dec(StackPos^);
 End;
End;

(* VM_SetEB *)
{
 Sets `eb#` or `if` register value.
}
Procedure VM_SetEB(VM: Pointer; RegNum: Byte; RegValue: Boolean); stdcall;
Begin
 PVM(VM)^.Regs.b[RegNum] := RegValue;
End;

(* VM_SetEC *)
{
 Sets `ec#` register value.
}
Procedure VM_SetEC(VM: Pointer; RegNum: Byte; RegValue: Char); stdcall;
Begin
 PVM(VM)^.Regs.c[RegNum] := RegValue;
End;

(* VM_SetEI *)
{
 Sets `ei#` or `stp` register value.
}
Procedure VM_SetEI(VM: Pointer; RegNum: Byte; RegValue: Int64); stdcall;
Begin
 PVM(VM)^.Regs.i[RegNum] := RegValue;
End;

(* VM_SetEF *)
{
 Sets `ef#` register value.
}
Procedure VM_SetEF(VM: Pointer; RegNum: Byte; RegValue: Extended); stdcall;
Begin
 PVM(VM)^.Regs.f[RegNum] := RegValue;
End;

(* VM_SetES *)
{
 Sets `es#` register value.
}
Procedure VM_SetES(VM: Pointer; RegNum: Byte; RegValue: PChar); stdcall;
Begin
 PVM(VM)^.Regs.s[RegNum] := CopyStringToPChar(RegValue);
End;

(* VM_SetER *)
{
 Sets `er#` register value.
}
Procedure VM_SetER(VM: Pointer; RegNum: Byte; RegValue: Pointer); stdcall;
Begin
 PVM(VM)^.Regs.r[RegNum] := RegValue;
End;

(* VM_GetEB *)
{
 Returns `eb#` or `if` register value.
}
Function VM_GetEB(VM: Pointer; RegNum: Byte): Boolean; stdcall;
Begin
 Result := PVM(VM)^.Regs.b[RegNum];
End;

(* VM_GetEC *)
{
 Returns `ec#` register value.
}
Function VM_GetEC(VM: Pointer; RegNum: Byte): Char; stdcall;
Begin
 Result := PVM(VM)^.Regs.c[RegNum];
End;

(* VM_GetEI *)
{
 Returns `ei#` or `stp` register value.
}
Function VM_GetEI(VM: Pointer; RegNum: Byte): Int64; stdcall;
Begin
 Result := PVM(VM)^.Regs.i[RegNum];
End;

(* VM_GetEF *)
{
 Returns `ef#` register value.
}
Function VM_GetEF(VM: Pointer; RegNum: Byte): Extended; stdcall;
Begin
 Result := PVM(VM)^.Regs.f[RegNum];
End;

(* VM_GetES *)
{
 Returns `es#` register value.
}
Function VM_GetES(VM: Pointer; RegNum: Byte): PChar; stdcall;
Begin
 Result := CopyStringToPChar(PVM(VM)^.Regs.s[RegNum]);
End;

(* VM_GetER *)
{
 Returns `er#` register value.
}
Function VM_GetER(VM: Pointer; RegNum: Byte): Pointer; stdcall;
Begin
 Result := PVM(VM)^.Regs.r[RegNum];
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
 Position := PByte(uint32(CodeData)+Pos);
End;

(* TVM.GetPosition *)
{
 Gets the relative current position: `Position-CodeData`
}
Function TVM.GetPosition: uint32;
Begin
 Result := uint32(uint32(Position)-uint32(CodeData));
End;

(* TVM.FindInternalCall *)
{
 Searches for internal call with specified full name and returns it (or "nil" if not found).
}
Function TVM.FindInternalCall(const Name: String): PCall;
Begin
 For Result in InternalCallList Do
  if (Result^.FullName = Name) Then
   Exit;

 Exit(nil);
End;

(* TVM.StackPush *)
{
 Pushes an element onto the stack.
}
Procedure TVM.StackPush(E: TStackElement);
Begin
 Inc(StackPos^);
 Stack[StackPos^] := E;
End;

(* TVM.StackPop *)
{
 'Pops' an element from the stack.
}
Function TVM.StackPop: TStackElement;
Begin
 Result := Stack[StackPos^];
 Dec(StackPos^);
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
Var Typ: TOpcodeArgType;
Begin
 Result.Reset;

 Typ := TOpcodeArgType(read_uint8);

 Result.isReg := (Typ in [ptBoolReg..ptReferenceReg]);

 if (Result.isReg) Then // if it's a register, read this register's ID
  Result.RegIndex := read_uint8;

 Case Typ of
  ptBoolReg, ptBool    : Result.Typ := mvBool;
  ptCharReg, ptChar    : Result.Typ := mvChar;
  ptIntReg, ptInt      : Result.Typ := mvInt;
  ptFloatReg, ptFloat  : Result.Typ := mvFloat;
  ptStringReg, ptString: Result.Typ := mvString;
  ptReferenceReg       : Result.Typ := mvReference;
  ptConstantMemRef     : Result.Typ := mvReference;

  else
   Result.Typ := mvInt;
 End;

 Case Typ of
  { register value }
  ptBoolReg     : Result.Value.Bool  := Regs.b[Result.RegIndex];
  ptCharReg     : Result.Value.Char  := Regs.c[Result.RegIndex];
  ptIntReg      : Result.Value.Int   := Regs.i[Result.RegIndex];
  ptFloatReg    : Result.Value.Float := Regs.f[Result.RegIndex];
  ptStringReg   : Result.Value.Str   := CopyStringToPChar(Regs.s[Result.RegIndex]);
  ptReferenceReg: Result.Value.Int   := uint32(Regs.r[Result.RegIndex]);

  { constant value }
  ptBool          : Result.Value.Bool  := Boolean(read_uint8);
  ptChar          : Result.Value.Char  := chr(read_uint8);
  ptInt           : Result.Value.Int   := read_int64;
  ptFloat         : Result.Value.Float := read_float;
  ptString        : Result.Value.Str   := CopyStringToPChar(read_string);
  ptConstantMemRef: Result.MemAddr     := read_int64;

  else
   Result.Value.Int := read_int32;
 End;

 Result.isStackval := (Typ = ptStackval);
 Result.isMemRef   := (Typ = ptConstantMemRef);

 if (Result.isStackval) Then // if stackval
 Begin
  Result.Stackval := @Stack[StackPos^+Result.Value.Int];
  Result.Typ      := Result.Stackval^.Typ;
  Result.Value    := Result.Stackval^.Value;
 End;

 if (Result.isMemRef) Then // if memory reference
 Begin
  Result.MemAddr += uint32(@CodeData[0]); // make absolute address
 End;
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
   raise SysUtils.Exception.Create('Throwing exceptions using objects has not been implemented yet!');

  { throw (by) message (string) }
  etByMessage:
  Begin
   if (ExceptionHandler = -1) Then // no exception handler set
   Begin
    LastException := Exception;
    StopReason    := srException;
    Stop          := True;

    raise SysUtils.Exception.Create('Bytecode has raised an exception!');
   End;

   LastException := Exception;

   if (JITCode = nil) Then
    setPosition(ExceptionHandler);

   if (ExceptionHandler = -1) Then
    raise SysUtils.Exception.Create('Bytecode has raised an exception!');
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
 Checks if passed reference is a valid object and throws exception if it's not.
 Returns passed object.
}
Function TVM.CheckObject(const Address: Pointer): Pointer;
Begin
 Result := Address;

 if (Address = nil) Then
  ThrowExceptionByMessage('Null pointer reference');

 if (not TGarbageCollector(GarbageCollector).findObject(TMObject(Address))) Then
  ThrowExceptionByMessage(PChar('Not a valid object reference: 0x'+IntToHex(uint32(Address), 2*sizeof(uint32))));
End;

(* TVM.isValidObject *)
{
 Similar to @TVM.CheckObject but doesn't throw any exception, just returns true/false.
}
Function TVM.isValidObject(const Obj: Pointer): Boolean;
Begin
 Result := TGarbageCollector(GarbageCollector).findObject(TMObject(Obj));
End;
End.
