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
 {$FATAL 64-bit architectures are not supported!}
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
Function LoadProgram(FileName: PChar): Pointer; stdcall;
Begin
 SetError(NO_ERROR);

 if (not FileExists(FileName)) Then // file not found
 Begin
  SetError(ERR_FILE_NOT_FOUND, 'File "'+FileName+'" cannot be found.');
  Exit(nil);
 End;

 Try
  Result := AllocMem(sizeof(TVM));
  VM_Create(Result, FileName);
 Except
  On E: Exception Do // exception raised during program load
  Begin
   SetError(ERR_INVALID_PROGRAM, E.Message);
   Exit(nil);
  End;
 End;
End;

{ FreeVM }
Procedure FreeVM(VM: Pointer); stdcall;
Begin
 if (VM <> nil) Then
  FreeMem(PVM(VM));
End;

{ getVersion }
Function getVersion: PChar; stdcall;
Begin
 Result := VM.VMVersion;
End;

// exports
Exports
 GetErrorID,
 GetErrorMsg,
 LoadProgram,
 FreeVM,
 getVersion,
 VM_Run name 'Run',
 VM_AddInternalCall name 'AddInternalCall',
 VM_StackPush name 'StackPush',
 VM_StackPop name 'StackPop';

Begin
End.
