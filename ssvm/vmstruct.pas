(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMStruct;

 Interface
 Uses VMStack, VMTypes, VMICall, VMExceptions, VMBytecode, BCLoader, SysUtils;

 Const VMVersion = '0.3.4 nightly';

 { EBackToMainException }
 Type EBackToMainException = Class(Exception);

 { TStopReason }
 Type TStopReason = (srFinished, srException);

 { TVM }
 Type PVM = ^TVM;
      TVM =
      Record
       Regs: // registers
       Packed Record
        b: Array[1..5] of VMBool;      // bool     : eb1, eb2, eb3, eb4, if
        c: Array[1..4] of VMChar;      // char     : ec1, ec2, ec3, ec4
        i: Array[1..5] of VMInt;       // int      : ei1, ei2, ei3, ei4, stp
        f: Array[1..4] of VMFloat;     // float    : ef1, ef1, ef3, ef4
        s: Array[1..4] of VMString;    // string   : es1, es2, es3, es4
        r: Array[1..4] of VMReference; // reference: er1, er2, er3, er4
       End;

       LoaderData: TBCLoaderData; // filled by TBCLoader
       Bytecode  : TVMBytecode;

       InternalCallList: TInternalCallList; // list of registered icall-s

       Stack         : TVMStack; // default bytecode stack
       ExceptionStack: PVMInt; // pointer to the latest exception stack element

       ExceptionHandler: VMInt; // current bytecode exception handler (must be a signed type!); `-1` means no handler set
       LatestException : TExceptionBlock; // last exception block
       StackPos        : PVMInt; // points at `Regs.i[5]` (current stack position)

       Stop      : Boolean; // used by the `stop` opcode, if equal `true` - VM stops executing bytecode
       StopReason: TStopReason;

       GarbageCollector: TObject; // GC object instance

       JITCode     : Pointer; // pointer to the JIT-ted code
       JITCodeSize : uint32;
       JITCompiler : Pointer;
       LastJITError: PChar;

       { -- procedures and functions reserved for internal use -- }
       Function FindInternalCall(const FullName: String): PInternalCall;

       Procedure BackToTheMain;

       // -- TMixedValue handling -- //
       Function getBool(const MV: TMixedValue): VMBool; inline;
       Function getChar(const MV: TMixedValue): VMChar; inline;
       Function getInt(const MV: TMixedValue): VMInt; inline;
       Function getFloat(const MV: TMixedValue): VMFloat; inline;
       Function getString(const MV: TMixedValue): VMString; inline;
       Function getReference(const MV: TMixedValue): VMReference; inline;

       // -- exception handling -- //
       Procedure ThrowException(const Exception: TExceptionBlock);
       Procedure ThrowException(const Message: String);
       Procedure ThrowException(const Format: String; const Args: Array of Const);

       // -- object handling -- //
       Function CheckObject(const Address: Pointer): Pointer; inline;
       Function isValidObject(const Obj: Pointer): Boolean; inline;
      End;

 Implementation
Uses VMObjects, VMStrings, GarbageCollector;

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

   { string }
   mvString: Exit(Value.Str.Data[1]);
  End;
 End;

 ThrowException('Invalid casting: %s -> char', [MixedValueTypeNames[MV.Typ]]);
End;

(* TVM.getInt *)
Function TVM.getInt(const MV: TMixedValue): VMInt;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PVMInt(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(Int64(Value.Bool));

   { char }
   mvChar: Exit(ord(Value.Char));

   { int, reference, callstack ref }
   mvInt, mvReference, mvCallstackRef: Exit(Value.Int);

   { float }
   mvFloat: Exit(Round(Value.Float)); // @TODO: inf, NaN?
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
Function TVM.getString(const MV: TMixedValue): VMString;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PVMString(MemAddr)^.Clone);

  Case Typ of
   { char }
   mvChar: Exit(Value.Char);

   { string }
   mvString: Exit(Value.Str.Clone);
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
    Stop       := True;

    BackToTheMain;
   End;

   if (JITCode = nil) Then
   Begin
    Bytecode.setPosition(VMReference(ExceptionHandler));
   End Else
    ThrowException('Unimplemented: bytecode exception handling with JIT compiler.');
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
End.
