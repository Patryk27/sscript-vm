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
Uses SysUtils, VMStruct, VMStack, VMICall, VMBytecode, VMExceptions, VMTypes, VMStrings, BCLoader, JITCompiler, GarbageCollector;

{ TVMDataStructure }
Type TVMDataStructure =
     Packed Record
      FileName : PChar;
      GCMemSize: uint32;

      LogMode: TLogMode;
      LogFile: PChar;
     End;

(* CreateAndLoad *)
{
 Creates a new VM istance, sets its fields to their default values and loads specified bytecode file.
 Returns 'nil', if failed, or a valid VM instance pointer otherwise.
}
Function CreateAndLoad(const Struct: TVMDataStructure): PVM; stdcall;
Var LoaderClass: TBCLoader;
Begin
 New(Result);

 With Result^ do
 Begin
  // try to load and parse the input file
  LoaderClass := TBCLoader.Create(Struct.FileName);

  LoaderData.CodeData := nil;

  Try
   if (LoaderClass.Load) Then
    LoaderData := LoaderClass.getLoaderData;
  Finally
   LoaderClass.Free;
  End;

  if (LoaderData.CodeData = nil) Then
  Begin
   Dispose(Result);
   Exit(nil);
  End;

  // allocate classes
  Bytecode     := TVMBytecode.Create(Result);
  Stack        := TVMStack.Create(Result);
  VMStringList := TVMStringList.Create(Result);

  InternalCallList := TInternalCallList.Create;
  GarbageCollector := TGarbageCollector.Create(Result, Struct.GCMemSize);

  // allocate memory
  ExceptionStack   := AllocMem(ExceptionStackSize);
  ExceptionStackID := 0;

  // set variables
  StackPos := Pointer(VMIReference(@Regs.i[5])); // point at high bytes of the 'stp' register
  LogMode  := Struct.LogMode;

  if (Struct.LogMode = lmFile) Then
  Begin
   LogFile := Struct.LogFile;

   AssignFile(LogHandle, LogFile);
   Rewrite(LogHandle);
  End;

  WriteLog('-- log begin --');

  // reset variables
  JITCompiler := nil;
  JITCode     := nil;
  JITError    := nil;
 End;
End;

(* ExecuteVM *)
{
 Executes the VM.
 Returns 'false' if failed or 'true' if script finished executing.
}
Function ExecuteVM(const VM: PVM): Boolean; stdcall;
Var I: uint8;
Begin
 Result := True;

 With VM^ do
 Begin
  Try
   WriteLog('Preparing to execute VM...');

   ExceptionHandler     := -1;
   LatestException.Typ  := etNone;
   LatestException.Data := nil;

   if (Bytecode.getData = nil) Then
   Begin
    WriteLog('Failed: no bytecode data to execute!');
    Exit(False);
   End;

   if (not LoaderData.isRunnable) Then
   Begin
    WriteLog('Failed: code is not runnable!');
    Exit(False);
   End;

   Bytecode.setPosition(Bytecode.getData); // move instruction pointer at the beginning

   Stack.Clear; // clear stack

   StopReason := srFinished;
   Stop       := False;

   For I := Low(Regs.s) To High(Regs.s) Do // clear string registers
    Regs.s[I] := VMStringList.StringToVMString('');

   if (JITCode <> nil) Then // if possible, execute the JIT compiled code instead of running slow opcode interpreter
   Begin
    WriteLog('Executing JITted code...');
    TProcedure(JITCode)();
    Exit;
   End;

   WriteLog('Executing opcode interpreter...');
   Bytecode.Execute; // execute bytecode
  Except
   On E: EBackToMainException Do
   Begin
    VM^.WriteLog('>> EBackToMainException <<');
    VM^.WriteLog('');
    VM^.WriteLog('Exception data:');
    VM^.DumpExceptionData;

    VM^.FlushLog;
   End;

   On E: Exception Do
   Begin
    VM^.WriteLog('>> Unexpected exception was raised! <<');
    VM^.WriteLog('Message = %s', [E.Message]);
    VM^.WriteLog('ClassName = %s', [E.ClassName]);
    VM^.WriteLog('');

    VM^.WriteLog('Exception stacktrace:');
    VM^.WriteLogExceptionStacktrace;
    VM^.WriteLog('');

    VM^.WriteLog('Exception data:');
    VM^.DumpExceptionData;

    VM^.FlushLog;

    StopReason := srException;

    if (LatestException.Data = nil) Then
    Begin
     LatestException.Typ  := etByMessage;
     LatestException.Data := StringToPChar(E.Message);
    End;
   End;
  End;
 End;
End;

(* FreeVM *)
{
 Releases memory allocated to the VM instance and the VM instance itself.
}
Procedure FreeVM(const VM: PVM); stdcall;
Var Call: PInternalCall;
Begin
 With VM^ do
 Begin
  WriteLog('Freeing virtual machine...');

  Dispose(ExceptionStack);

  For Call in InternalCallList Do
   Dispose(Call);

  Stack.Free;
  Bytecode.Free;
  InternalCallList.Free;
  TGarbageCollector(GarbageCollector).Free;

  if (JITCompiler <> nil) Then
   TJITCompiler(JITCompiler).Free;

  VMStringList.Free;

  CloseLog;
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
  WriteLog('JITCompile()');

  if (JITCompiler <> nil) Then
  Begin
   WriteLog('Freeing previous JIT compiler instance...');
   TJITCompiler(JITCompiler).Free;
  End;

  JITCompiler := nil;
  JITCode     := nil;
  JITCodeSize := 0;
  JITError    := #0;

  Try
   WriteLog('Running JIT compiler...');
   Compiler    := TJITCompiler.Create(VM); // create compiler instance, load bytecode...
   JITCode     := Compiler.Compile(); // ...and compile it! :)
   JITCodeSize := MemSize(JITCode);
  Except
   On E: Exception Do
   Begin
    WriteLog('JIT compiler raised an exception: %s', [E.Message]);
    JITError := StringToPChar(E.Message);
    Compiler.Free;
    Exit(False);
   End;
  End;

  WriteLog('JIT compilation done successfully!');
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
 Result := VM^.JITError;
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

 VM^.WriteLog('Registering new internal call: ''%s'' (handler: 0x%x)', [FullName, VMIReference(Handler)]);

 if (PackageName = 'vm') or // "vm" calls are reserved and set by the VM itself
    (VM^.FindInternalCall(FullName) <> nil) { also, icalls cannot be doubled } Then
 Begin
  if (PackageName = 'vm') Then
   VM^.WriteLog('Failed - "vm." calls are registered only for internal VM use.') Else
   VM^.WriteLog('Failed - call with such name has been already registered.');

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
 Returns VM stop reason (either exception or 'stop' opcode).
}
Function GetStopReason(const VM: PVM): TStopReason; stdcall;
Begin
 With VM^ do
  Exit(StopReason);
End;

(* AllocateString *)
{
 Allocates a VMString and returns its pointer.
}
Function AllocateString(const VM: PVM; const Content: PChar): PVMString; stdcall;
Begin
 Result := VM^.VMStringList.StringToVMString(Content);
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

 AllocateString,

 GetVMVersion;

Begin
 DefaultFormatSettings.DecimalSeparator := '.';
End.
