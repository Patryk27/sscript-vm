(*
 SScript Virtual Machine Library
 Copyright © by Patryk Wychowaniec, 2013-2014

 -------------------------------------------------------------------------------
 SScript Compiler is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation; either version 2.1 of the License, or
 (at your option) any later version.

 SScript Compiler is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with SScript Compiler; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

{$IFDEF CPU64}
 {$FATAL 64-bit architecture is not supported!}
{$ENDIF}

{$H+}
Library SSVM;
Uses SysUtils, VMStruct, VMStack, VMICall, VMBytecode, VMExceptions, VMStrings, BCLoader, JITCompiler, GarbageCollector;

(* CreateAndLoad *)
{
 Creates a new VM istance, sets its fields to their default values and loads specified bytecode file.
 Returns 'nil' if failed or a valid VM instance pointer otherwise.
}
Function CreateAndLoad(const FileName: PChar; const GCMemoryLimit: uint32): PVM; stdcall;
Var LoaderClass: TBCLoader;
Begin
 New(Result);

 With Result^ do
 Begin
  // try to load and parse the input file
  LoaderClass := TBCLoader.Create(FileName);

  LoaderData.CodeData := nil;

  Try
   if (LoaderClass.Load) Then
    LoaderData := LoaderClass.getLoaderData^;
  Finally
   LoaderClass.Free;
  End;

  if (LoaderData.CodeData = nil) Then
  Begin
   Dispose(Result);
   Exit(nil);
  End;

  // allocate classes
  Bytecode := TVMBytecode.Create(Result);
  Stack    := TVMStack.Create(Result);

  InternalCallList := TInternalCallList.Create;
  GarbageCollector := TGarbageCollector.Create(Result, GCMemoryLimit);

  // allocate memory
  ExceptionStack := GetMem(ExceptionStackSize);

  // set variables
  StackPos := @Regs.i[5];

  // reset variables
  JITCompiler  := nil;
  LastJITError := #0;
 End;
End;

(* ExecuteVM *)
{
 Executes the VM.
 Returns 'false' if failed or 'true' if script finished executing.
}
Function ExecuteVM(const VM: PVM): Boolean; stdcall;
Begin
 Result := True;

 With VM^ do
 Begin
  Try
   ExceptionHandler     := -1;
   LatestException.Typ  := etNone;
   LatestException.Data := nil;

   if (Bytecode.getData = nil) or (not LoaderData.isRunnable) Then // error: no code to execute or code is not runnable
    Exit(False);

   Bytecode.setPosition(Bytecode.getData); // move instruction pointer at the beginning

   Stack.Clear; // clear stack

   StopReason := srFinished;
   Stop       := False;

   if (JITCode <> nil) Then // if possible, execute the JIT compiled code instead of running slow opcode interpreter
   Begin
    TProcedure(JITCode)();
    Exit;
   End;

   Bytecode.Execute; // execute bytecode
  Except
   On E: EBackToMainException Do
   Begin
    Writeln('> EBackToMainExeption < !');
   End;

   On E: Exception Do
   Begin
    StopReason := srException;

    if (LatestException.Data = nil) Then
    Begin
     LatestException.Typ  := etByMessage;
     LatestException.Data := StringToPChar(E.Message);
    End;
   End;
  End;

  // @TODO: trzeba jeszcze zwolnić wszystkie zaalokowane TMixedValue-y (oraz VMStringi!)
 End;
End;

(* FreeVM *)
{
 Releases memory allocated to the VM instance and the VM instance itself.
}
Procedure FreeVM(VM: PVM); stdcall;
Var Call: PInternalCall;
Begin
 With VM^ do
 Begin
  For Call in InternalCallList Do
   Dispose(Call);

  Dispose(ExceptionStack);

  Stack.Free;
  Bytecode.Free;
  InternalCallList.Free;
  TGarbageCollector(GarbageCollector).Free;

  if (JITCompiler <> nil) Then
   TJITCompiler(JITCompiler).Free;

  // @TODO: release all VMStrings
 End;

 Dispose(VM);
End;

(* JITCompile *)
{
 Executes JIT compiler on already loaded bytecode in specified VM instance
}
Function JITCompile(const VM: PVM): Boolean; stdcall;
Var Compiler: TJITCompiler = nil;
Begin
 With VM^ do
 Begin
  if (JITCompiler <> nil) Then
   TJITCompiler(JITCompiler).Free;

  JITCompiler  := nil;
  JITCode      := nil;
  JITCodeSize  := 0;
  LastJITError := #0;

  Try
   Compiler    := TJITCompiler.Create(VM); // create compiler instance, load bytecode...
   JITCode     := Compiler.Compile(); // ...and compile it! :)
   JITCodeSize := MemSize(JITCode);
  Except
   On E: Exception Do
   Begin
    LastJITError := StringToPChar(E.Message);
    Compiler.Free;
    Exit(False);
   End;
  End;

  JITCompiler := Compiler;
  Exit(True);
 End;
End;

(* GetJITError *)
{
 Returns last JIT compiler error
}
Function GetJITError(VM: PVM): PChar; stdcall;
Begin
 Result := VM^.LastJITError;
End;

(* GetJITCodeData *)
{
 Return pointer to JIT compiled code
}
Function GetJITCodeData(VM: PVM): Pointer; stdcall;
Begin
 Result := VM^.JITCode;
End;

(* GetJITCodeSize *)
{
 Returns JIT-compiled code size in bytes
}
Function GetJITCodeSize(VM: PVM): uint32; stdcall;
Begin
 Result := VM^.JITCodeSize;
End;

(* AddInternalCall *)
{
 Adds new internal call onto the list.
}
Function AddInternalCall(const VM: PVM; const PackageName, FunctionName: PChar; const ParamCount: uint8; const Handler: TInternalCallHandler): Boolean; stdcall;
Var call    : PInternalCall;
    FullName: String;
Begin
 FullName := PackageName+'.'+FunctionName;

 if (PackageName = 'vm') or // "vm" calls are reserved and set by the VM itself
    (VM^.FindInternalCall(FullName) <> nil) { also, icalls cannot be doubled } Then
 Begin
  Exit(False);
 End;

 New(call);
 call^.PackageName  := PackageName;
 call^.FunctionName := FunctionName;
 call^.FullName     := FullName;
 call^.ParamCount   := ParamCount;
 call^.Handler      := Handler;

 VM^.InternalCallList.Add(call);

 Exit(True);
End;

(* StackPush *)
{
 Pushes new value onto the stack.
}
Procedure StackPush(const VM: PVM; const Value: TMixedValue); stdcall;
Begin
 With VM^ do
  Stack.Push(Value);
End;

(* StackPop *)
{
 'Pops' a value from the stack.
}
Function StackPop(const VM: PVM): TMixedValue; stdcall;
Begin
 With VM^ do
  Result := Stack.Pop;
End;

(* ThrowException *)
{
 Throws a catchable VM exception.
}
Procedure ThrowException(const VM: PVM; const Exception: TExceptionBlock); stdcall;
Begin
 With VM^ do
  ThrowException(Exception);
End;

(* GetException *)
{
 Returns the latest code exception.
}
Function GetException(const VM: PVM): TExceptionBlock; stdcall;
Begin
 With VM^ do
  Exit(LatestException);
End;

(* GetStopReason *)
{
 Returns VM stop reason.
}
Function GetStopReason(const VM: PVM): TStopReason; stdcall;
Begin
 With VM^ do
  Exit(StopReason);
End;

(* GetVMVersion *)
{
 Returns VM version.
}
Function GetVMVersion: PChar; stdcall;
Begin
 Result := VMVersion;
End;

// exports
Exports
 CreateAndLoad,
 ExecuteVM,
 FreeVM,

 JITCompile,
 GetJITError,
 GetJITCodeData,
 GetJITCodeSize,

 AddInternalCall,

 StackPush,
 StackPop,

 ThrowException,
 GetException,
 GetStopReason,

 GetVMVersion;

Begin
 DefaultFormatSettings.DecimalSeparator := '.';
End.
