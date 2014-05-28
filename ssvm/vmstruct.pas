(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
{$MACRO ON}
Unit VMStruct;

 Interface
 Uses VMStack, VMTypes, VMStrings, VMICall, VMExceptions, VMBytecode, BCLoader, SysUtils;

 Const VMVersion = '0.3.4 nightly';

 { EBackToMainException }
 Type EBackToMainException = Class(Exception);

 { TStopReason }
 Type TStopReason = (srFinished, srException);

 { TLogMode }
 Type TLogMode = (lmDisabled, lmConsole, lmFile);

 { TVM }
 Type PVM = ^TVM;
      TVM =
      Record
       Regs: // registers
       Record
        b: Array[0..5] of VMBool;      // bool     : unused, eb1, eb2, eb3, eb4, if
        c: Array[0..4] of VMChar;      // char     : unused, ec1, ec2, ec3, ec4
        i: Array[0..5] of VMInt;       // int      : unused, ei1, ei2, ei3, ei4, stp
        f: Array[0..4] of VMFloat;     // float    : unused, ef1, ef1, ef3, ef4
        s: Array[0..4] of PVMString;   // string   : unused, es1, es2, es3, es4
        r: Array[0..4] of VMReference; // reference: unused, er1, er2, er3, er4
       End;

       LoaderData: TBCLoaderData; // filled by TBCLoader
       Bytecode  : TVMBytecode;

       VMStringList: TVMStringList; // list of allocated VMStrings; used to prevent internal memory leaks

       InternalCallList: TInternalCallList; // list of registered icall-s

       Stack           : TVMStack; // default bytecode stack
       ExceptionStack  : PVMInt; // pointer to the beginning of the exception stack
       ExceptionStackID: uint32; // index of last element lying on the exception stack

       ExceptionHandler: VMInt; // current bytecode exception handler (must be a signed type!); `-1` means no handler set
       LatestException : TExceptionBlock; // last exception block
       StackPos        : puint32; // points at low bytes of Regs.i[5] (current stack position)

       Stop      : Boolean; // used by the `stop` opcode - if equal `true`, VM stops executing further bytecode; refers only to the opcode interpreter.
       StopReason: TStopReason;

       GarbageCollector: TObject; // GC object instance

       JITCode    : Pointer; // pointer to the JIT-ted code
       JITCodeSize: uint32;
       JITCompiler: Pointer;
       JITError   : PChar; // either null or a pointer to valid PChar string

       LogMode  : TLogMode;
       LogFile  : String;
       LogHandle: TextFile; // if LogMode == lmFile
       // LastLogFlush: uint32; @TODO (?)

       { -- procedures and functions reserved for internal use -- }
       Function FindInternalCall(const FullName: String): PInternalCall;

       Procedure BackToTheMain;

       Procedure CheckMemory;

       // -- TMixedValue handling -- //
       Function getBool(const MV: TMixedValue): VMBool; //inline;
       Function getChar(const MV: TMixedValue): VMChar; //inline;
       Function getInt(const MV: TMixedValue): VMInt; //inline;
       Function getFloat(const MV: TMixedValue): VMFloat; //inline;
       Function getString(const MV: TMixedValue): PVMString; // inline;
       Function getReference(const MV: TMixedValue): VMReference; //inline;
       Function getPChar(const MV: TMixedValue; const UnbindAndFree: Boolean=False): PChar; //inline;

       // -- string handling -- //
       Procedure StringConcat(A, B: PVMString);
       Procedure StringConcat(A: PVMString; const B: String);

       // -- exception handling -- //
       Procedure ThrowException(const Exception: TExceptionBlock);
       Procedure ThrowException(const Message: String);
       Procedure ThrowException(const Format: String; const Args: Array of Const);

       // -- object handling -- //
       Function CheckObject(const Address: Pointer): Pointer; inline;
       Function isValidObject(const Obj: Pointer): Boolean; inline;

       // -- log handling -- //
       Procedure FlushLog;
       Procedure CloseLog;

       Procedure WriteLog(const Message: String='');
       Procedure WriteLog(const Format: String; const Args: Array of Const);
       Procedure WriteLogExceptionStacktrace;
       Procedure DumpExceptionData;
      End;

 Implementation
Uses VMObjects, GarbageCollector, os_functions;

(* TVM.FindInternalCall *)
{
 Searches for internal call with specified full name and returns it (or "nil" if not found).
}
Function TVM.FindInternalCall(const FullName: String): PInternalCall;
Begin
 For Result in InternalCallList Do
  if (Result^.FullName = FullName) Then
   Exit;

 Exit(nil);
End;

(* TVM.BackToTheMain *)
Procedure TVM.BackToTheMain;
Begin
 raise EBackToMainException.Create(''); // should return to ssvm.lpr::Run() function
End;

(* TVM.CheckMemory *)
{
 Checks amount of free memory and - if too low - runs garbage collector.
}
Procedure TVM.CheckMemory;
Var Write     : Boolean;
    TotalFreed: uint32;
Begin
 {$DEFINE Used := GetFPCHeapStatus.CurrHeapUsed}
 {$DEFINE MemLimit := TGarbageCollector(GarbageCollector).getMemoryLimit}

 TotalFreed := Used;

 if (Used > MemLimit) Then
 Begin
  WriteLog('Used (%d MB) > MemLimit (%d MB); need to clear memory...', [Used div 1024 div 1024, MemLimit div 1024 div 1024]);
  Write := True;
 End Else
  Write := False;

 if (Used > MemLimit) Then // at first try to purge the VM unused memory rather than run bytecode GC
  VMStringList.PurgeUnused;

 if (Used > MemLimit) Then // but if the above isn't enough, eventually run bytecode GC
 Begin
  WriteLog('Still not enough memory left - running GC...');
  TGarbageCollector(GarbageCollector).DoGarbageCollection;
 End Else
 Begin
  if (Write) Then
   WriteLog('Enough memory has been cleaned - skipping GC phase.');
 End;

 if (Used > MemLimit) Then // if everything failes, give up
 Begin
  WriteLog('Still not enough memory left (need %d MB more) - giving up.', [(Used - MemLimit) div 1024 div 1024]);
  ThrowException('Not enough memory left - cannot continue execution (GC couldn''t free anything). Try to increase memory limit or create less objects on the heap.');
 End;

 TotalFreed := TotalFreed - Used;

 if (Write) Then
  WriteLog('Memory sweeping done - freed %d MB.', [TotalFreed div 1024 div 1024]);
End;

(* TVM.getBool *)
Function TVM.getBool(const MV: TMixedValue): VMBool;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PBoolean(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(Value.Bool);

   { int }
   mvInt: Exit(Boolean(Value.Int));
  End;
 End;

 ThrowException('Invalid casting: %s -> bool', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getChar *)
Function TVM.getChar(const MV: TMixedValue): VMChar;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PChar(MemAddr)^);

  Case Typ of
   { char }
   mvChar: Exit(Value.Char);

   { int }
   mvInt: Exit(Char(Value.Int));
  End;
 End;

 ThrowException('Invalid casting: %s -> char', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getInt *)
Function TVM.getInt(const MV: TMixedValue): VMInt;
Var Float: VMFloat;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PVMInt(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(VMInt(Value.Bool));

   { char }
   mvChar: Exit(ord(Value.Char));

   { int, reference, callstack ref }
   mvInt, mvReference, mvCallstackRef: Exit(Value.Int);

   { float }
   mvFloat:
   Begin
    Float := Value.Float;

    if (Float > High(VMInt)) Then
     Exit(High(VMInt)) Else

    if (Float < Low(VMInt)) Then
     Exit(Low(VMInt)) Else

     Exit(Round(Float));
   End;
  End;
 End;

 ThrowException('Invalid casting: %s -> int', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getFloat *)
Function TVM.getFloat(const MV: TMixedValue): VMFloat;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PVMFloat(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(uint8(Value.Bool));

   { int, reference, callstack ref }
   mvInt, mvReference, mvCallstackRef: Exit(Value.Int);

   { float }
   mvFloat: Exit(Value.Float);
  End;
 End;

 ThrowException('Invalid casting: %s -> float', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getString *)
Function TVM.getString(const MV: TMixedValue): PVMString;
Var VMStr: PVMString;
    Str  : String;
    Ch   : PChar;
Begin
 With MV do
 Begin
  if (isMemRef) Then
  Begin
   if (PByte(MemAddr)^ = $FF) Then
   Begin
    {
     @Note:

     This is related to the global variables mechanism.

     Since compiler cannot allocate VMStrings by itself (even in the bytecode)
     and global variables must be handled somehow, following data is generated:
     > 0xFF + first string char + second string char + (...) + 0x00
     This sequence indicates that VM shouldn't take the address of the string
     (because this would normally be: > VMString pointer) but the string itself.
    }

    Str := '';
    Ch  := MemAddr+1;

    While (Ch^ <> #0) Do
    Begin
     Str += Ch^;
     Inc(Ch);
    End;

    New(VMStr);
    VMStr^.Data   := StringToPChar(Str, False);
    VMStr^.Length := Length(Str);

    // assign the pointer so that we don't have to create a new VMString each time
    if (Length(Str)+1 >= sizeof(Pointer)) Then
     PPointer(MemAddr)^ := VMStr;
   End;

   // return address
   Exit(PPointer(MemAddr)^);
  End;

  Case Typ of
   { char }
   mvChar: Exit(VMStringList.CharToVMString(Value.Char));

   { string }
   mvString: Exit(Value.Str);
  End;
 End;

 ThrowException('Invalid casting: %s -> string', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getReference *)
Function TVM.getReference(const MV: TMixedValue): VMReference;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PVMReference(MemAddr)^);

  Case Typ of
   { int, reference, callstack ref }
   mvInt, mvReference, mvCallstackRef: Exit(VMReference(Value.Int));
  End;
 End;

 ThrowException('Invalid casting: %s -> reference', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getPChar *)
{
 Converts MixedValue to string and fetches the PChar from the string.

 If "UnbindAndFree" equals 'true', disposes the VMString instance itself but leaves the "Data" pointer valid.
}
Function TVM.getPChar(const MV: TMixedValue; const UnbindAndFree: Boolean=False): PChar;
Var Str: PVMString;
Begin
 Str := getString(MV); // fetch string

 if (UnbindAndFree) Then
 Begin
  VMStringList.Unbind(Str); // unbind it, so we can dispose it by ourselves in a moment
  Result := Str^.Data; // get data
  Dispose(Str); // dispose string - notice that the "Str^.Data" is left untouched.
 End;
End;

(* TVM.StringConcat *)
{
 Concatenates two VMStrings: A := A+B;
}
Procedure TVM.StringConcat(A, B: PVMString);
Var OldLen: uint32;

  { SpecialCase } // see TVM.getString()
  Function SpecialCase(const Pnt: Pointer): PVMString;
  Var MV: TMixedValue;
  Begin
   if (Puint8(Pnt)^ = $FF) Then
   Begin
    MV.Reset;
    MV.Typ      := mvString;
    MV.MemAddr  := Pnt;
    MV.isMemRef := True;

    Result := getString(MV);
   End Else
   Begin
    Result := PVMString(Pnt);
   End;
  End;
Begin
 A := SpecialCase(A);
 B := SpecialCase(B);

 // is there anything to do?
 if (B^.Length = 0) Then
  Exit;

 // save current length
 OldLen := A^.Length;

 // increase length
 A^.Length += B^.Length;

 // reallocate memory block
 A^.Data := ReallocMem(A^.Data, A^.Length);

 // copy data
 Move(B^.Data[0], A^.Data[OldLen], B^.Length);
End;

(* StringConcat *)
{ Concatenates VMString and a regular String: A := A+B; }
Procedure TVM.StringConcat(A: PVMString; const B: String);
Var OldLen: uint32;
    MV    : TMixedValue;
Begin
 // is there anything to do?
 if (Length(B) = 0) Then
  Exit;

 // special case; see TVM.getString()
 if (PByte(A)^ = $FF) Then
 Begin
  MV.Reset;
  MV.Typ      := mvString;
  MV.MemAddr  := A;
  MV.isMemRef := True;

  A := TVM.getString(MV);
 End;

 // save current length
 OldLen := A^.Length;

 // increase length
 A^.Length += Length(B);

 // reallocate memory block
 ReallocMem(A^.Data, A^.Length);

 // copy data
 Move(B[1], A^.Data[OldLen], Length(B));
End;

(* TVM.ThrowException *)
{
 Throws an exception.
}
Procedure TVM.ThrowException(const Exception: TExceptionBlock);
Begin
 Stop := True;

 Case Exception.Typ of
  { throw (by) object }
  etByObject:
  Begin
   ThrowException('Throwing object exceptions has not been implemented yet.');
  End;

  { throw (by) message (string) }
  etByMessage:
  Begin
   LatestException := Exception;

   if (ExceptionHandler = -1) Then // no exception handler set - halt VM and return control back to the user
   Begin
    StopReason := srException;
    BackToTheMain;
   End;

   if (JITCode = nil) Then
   Begin
    Stop := False;
    Bytecode.setRelativePosition(ExceptionHandler);
   End Else
   Begin
    ThrowException('Unimplemented: bytecode exception handling with JIT compiler.');
   End;
  End;
 End;
End;

(* TVM.ThrowException *)
{
 Throws a message exception.
}
Procedure TVM.ThrowException(const Message: String);
Var EB: TExceptionBlock;
Begin
 EB.Typ  := etByMessage;
 EB.Data := StringToPChar(Message);
 ThrowException(EB);
End;

(* TVM.ThrowException *)
{
 Throws a formatted message exception.
}
Procedure TVM.ThrowException(const Format: String; const Args: Array of const);
Begin
 ThrowException(SysUtils.Format(Format, Args));
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
  ThrowException('Null pointer reference');

 if (not TGarbageCollector(GarbageCollector).findObject(TMObject(Address))) Then
  ThrowException('Not a valid object reference: 0x'+IntToHex(VMIReference(Address), 2*sizeof(VMIReference)));
End;

(* TVM.isValidObject *)
{
 Similar to @TVM.CheckObject but doesn't throw any exception, just returns true/false.
}
Function TVM.isValidObject(const Obj: Pointer): Boolean;
Begin
 Result := TGarbageCollector(GarbageCollector).findObject(TMObject(Obj));
End;

(* TVM.FlushLog *)
{
 If LogMode equals lmFile, flushes the log file; otherwise does nothing.
}
Procedure TVM.FlushLog;
Begin
 if (LogMode = lmFile) Then
  Flush(LogHandle);
End;

(* TVM.CloseLog *)
{
 If LogMode equals lmFile, flushes and closes the log file; otherwise does nothing.
}
Procedure TVM.CloseLog;
Begin
 WriteLog('-- log end --');

 if (LogMode = lmFile) Then
 Begin
  Flush(LogHandle);
  CloseFile(LogHandle);
 End;
End;

(* TVM.WriteLog *)
{
 Writes a message to the log.
}
Procedure TVM.WriteLog(const Message: String);
Var Msg: String;
Begin
 if (LogMode = lmDisabled) Then
  Exit;

 Msg := '['+IntToStr(GetMilliseconds)+'] '+Message;

 Case LogMode of
  // console log
  lmConsole:
  Begin
   Writeln(Msg);
  End;

  // file log
  lmFile:
  Begin
   Writeln(LogHandle, Msg);
  End;
 End;
End;

(* TVM.WriteLog *)
{
 Writes a formatted message to the log.
}
Procedure TVM.WriteLog(const Format: String; const Args: Array of Const);
Begin
 if (LogMode = lmDisabled) Then
  Exit;

 WriteLog(SysUtils.Format(Format, Args));
End;

(* TVM.WriteLogExceptionStacktrace *)
{
 Writes an exception stacktrace to the log.
}
Procedure TVM.WriteLogExceptionStacktrace;
Var Frames: PPointer;
    I     : uint32;
Begin
 WriteLog('%s', [BacktraceStrFunc(ExceptAddr)]);

 Frames := ExceptFrames;

 if (ExceptFrameCount > 0) Then
 Begin
  For I := 0 To ExceptFrameCount-1 Do
   WriteLog('%s', [BacktraceStrFunc(Frames[I])]);
 End;
End;

(* TVM.DumpExceptionData *)
{
 Dumps VM exception data to the log (register values etc.).
}
Procedure TVM.DumpExceptionData;
Var I  : uint8;
    Str: String;
    Obj: TMObject;
Begin
 // eb
 For I := 1 To 4 Do
  WriteLog('eb%d = %s', [I, BoolToStr(Regs.b[I], 'true', 'false')]);

 // ec
 WriteLog;
 For I := 1 To 4 Do
  WriteLog('ec%d = #%d', [I, ord(Regs.c[I])]);

 // ei
 WriteLog;
 For I := 1 To 4 Do
  WriteLog('ei%d = %d', [I, Regs.i[I]]);

 // ef
 WriteLog;
 For I := 1 To 4 Do
  WriteLog('ef%d = %f', [I, Regs.f[I]]);

 // es
 WriteLog;
 For I := 1 To 4 Do
 Begin
  Str := SysUtils.Format('es%d = PVMString(0x%x)', [I, VMIReference(Regs.s[I])]);

  if (Regs.s[I] <> nil) Then
  Begin
   Try
    Str += SysUtils.Format(' (len: %d) ', [Regs.s[I]^.Length]);

    if (Regs.s[I]^.Length > 30) Then
     Str += '<too long to display>' Else
     Str += '= '+Regs.s[I]^.asString;
   Except
    Str += ' <invalid address?>';
   End;
  End;

  WriteLog(Str);
 End;

 // er
 WriteLog;
 For I := 1 To 4 Do
 Begin
  Obj := TMObject(Regs.r[I]);

  Str := SysUtils.Format('er%d = 0x%x', [I, VMIReference(Obj)]);

  if (Obj = nil) Then
  Begin
   Str += ' (null)';
  End Else

  if (TGarbageCollector(GarbageCollector).FindObject(Obj)) Then
  Begin
   Str += SysUtils.Format(' (valid ''%s'' class instance)', [TObject(Obj).ClassName]);
  End Else

  Begin
   Str += ' (invalid/unknown)';
  End;

  WriteLog(Str);
 End;

 // special regs
 WriteLog;
 WriteLog('if  = %s', [BoolToStr(Regs.b[5], 'true', 'false')]);
 WriteLog('stp = %d', [Regs.i[5]]);
End;
End.
