(*
 SScript Virtual Machine Library
 Copyright © by Patryk Wychowaniec, 2013

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
Uses SysUtils, VM, Stack, Loader;
Const NO_ERROR            = 0;
      ERR_FILE_NOT_FOUND  = 1;
      ERR_INVALID_PROGRAM = 2;

Var LastError: uint8 = NO_ERROR;
    LastMsg  : PChar;

// SetError
Procedure SetError(const ID: uint8; const Msg: String='');
Begin
 LastError := ID;
 LastMsg   := PChar(Msg);
End;

// -------------------------------------------------------------------------- //

{ GetErrorID }
Function GetErrorID: uint8; stdcall;
Begin
 Result := LastError;
End;

{ GetErrorMsg }
Function GetErrorMsg: PChar; stdcall;
Begin
 Result := LastMsg;
End;

{ LoadProgram }
Function LoadProgram(FileName: PChar; GCMemoryLimit: uint32): Pointer; stdcall;
Begin
 SetError(NO_ERROR);

 if (not FileExists(FileName)) Then // file not found
 Begin
  SetError(ERR_FILE_NOT_FOUND, 'File "'+FileName+'" cannot be found.');
  Exit(nil);
 End;

 Try
  Result := AllocMem(sizeof(TVM));
  VM_Create(Result, FileName, GCMemoryLimit);
 Except
  On E: Exception Do // exception raised during program load
  Begin
   SetError(ERR_INVALID_PROGRAM, E.Message);
   Exit(nil);
  End;
 End;
End;

{ GetVersion }
Function GetVersion: PChar; stdcall;
Begin
 Result := VM.VMVersion;
End;

// exports
Exports
 GetErrorID,
 GetErrorMsg,
 GetVersion,

 LoadProgram,

 VM_Run name 'Run',
 VM_JITCompile name 'JITCompile',
 VM_GetLastJITError name 'GetLastJITError',
 VM_GetJITCode name 'GetJITCode',
 VM_GetJITCodeSize name 'GetJITCodeSize',
 VM_Free name 'Free',

 VM_AddInternalCall name 'AddInternalCall',

 VM_StackPush name 'StackPush',
 VM_StackPop name 'StackPop',

 VM_SetEB name 'SetEB',
 VM_SetEC name 'SetEC',
 VM_SetEI name 'SetEI',
 VM_SetEF name 'SetEF',
 VM_SetES name 'SetES',
 VM_SetER name 'SetER',

 VM_GetEB name 'GetEB',
 VM_GetEC name 'GetEC',
 VM_GetEI name 'GetEI',
 VM_GetEF name 'GetEF',
 VM_GetES name 'GetES',
 VM_GetER name 'GetER',

 VM_ThrowException name 'ThrowException',
 VM_GetException name 'GetException',
 VM_GetStopReason name 'GetStopReason';

Begin
End.
