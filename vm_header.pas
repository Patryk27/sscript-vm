(*
 SScript Virtual Machine Library Header
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
Unit vm_header;

 Interface
 Uses SysUtils;

 (* ========== constants ========== *)
 Const NO_ERROR            = 0;
       ERR_FILE_NOT_FOUND  = 1;
       ERR_INVALID_PROGRAM = 2;

 (* ========== types ========== *)
 { TMixedValue }
 Type TMixedValueType = (mvNone=-1, mvBool, mvChar, mvInt, mvFloat, mvString, mvReference);
 Type PMixedValue = ^TMixedValue;
      TMixedValue = Record
                     Typ  : TMixedValueType;
                     Value: Record
                             Bool : Boolean;
                             Char : Char;
                             Int  : Int64;
                             Float: Extended;
                             Str  : PChar;
                            End;

                     VMInternalUse: Array[1..11] of uint8;
                    End;
      TMixedValueArray = Array of TMixedValue;

 {$IF sizeof(TMixedValue) <> 64}
  {$FATAL Size of TMixedValue structure must be exactly 64 bytes!}
 {$ENDIF}

 { TExceptionBlock }
 Type TExceptionType = (etByObject, etByMessage);
 Type PExceptionBlock = ^TExceptionBlock;
      TExceptionBlock = Record
                         Typ : TExceptionType; // exception type ("throw object;" or "throw string;")
                         Data: Pointer; 
                        End;

 { TCallHandler }
 Type TCallHandler = Procedure (VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;

 { TStopReason }
 Type TStopReason = (srNormal, srException);

 { TJITCompiledState }
 Type TJITCompiledState = (csInvalidBytecode, csJITFailed, csJITUnsupported, csDisabled, csDone);

 (* ========== constants ========== *)
 Const MixedValueTypeNames: Array[TMixedValueType] of String = ('none', 'bool', 'char', 'int', 'float', 'string', 'reference');

 (* ========== functions imported from the DLL ========== *)
 Function GetErrorID: uint8;                                                                                         stdcall external 'ssvm.dll';
 Function GetErrorMsg: PChar;                                                                                        stdcall external 'ssvm.dll';
 Function GetVersion: PChar;                                                                                         stdcall external 'ssvm.dll';

 Function LoadProgram(FileName: PChar; GCMemoryLimit: uint32): Pointer;                                              stdcall external 'ssvm.dll';

 Procedure Run(VM: Pointer);                                                                                         stdcall external 'ssvm.dll';
 Function JITCompile(VM: Pointer): TJITCompiledState;                                                                stdcall external 'ssvm.dll';
 Function GetLastJITError(VM: Pointer): PChar;                                                                       stdcall external 'ssvm.dll';
 Function GetJITCode(VM: Pointer): Pointer;                                                                          stdcall external 'ssvm.dll';
 Function GetJITCodeSize(VM: Pointer): uint32;                                                                       stdcall external 'ssvm.dll';
 Procedure Free(VM: Pointer);                                                                                        stdcall external 'ssvm.dll';

 Procedure AddInternalCall(VM: Pointer; PackageName, FunctionName: PChar; ParamCount: uint8; Handler: TCallHandler); stdcall external 'ssvm.dll';

 Procedure StackPush(VM: Pointer; Element: TMixedValue);                                                             stdcall external 'ssvm.dll';
 Function StackPop(VM: Pointer): TMixedValue;                                                                        stdcall external 'ssvm.dll';

 Procedure SetEB(VM: Pointer; RegNum: Byte; RegValue: Boolean);                                                      stdcall external 'ssvm.dll';
 Procedure SetEC(VM: Pointer; RegNum: Byte; RegValue: Char);                                                         stdcall external 'ssvm.dll';
 Procedure SetEI(VM: Pointer; RegNum: Byte; RegValue: Int64);                                                        stdcall external 'ssvm.dll';
 Procedure SetEF(VM: Pointer; RegNum: Byte; RegValue: Extended);                                                     stdcall external 'ssvm.dll';
 Procedure SetES(VM: Pointer; RegNum: Byte; RegValue: PChar);                                                        stdcall external 'ssvm.dll';
 Procedure SetER(VM: Pointer; RegNum: Byte; RegValue: Pointer);                                                      stdcall external 'ssvm.dll';
 Function GetEB(VM: Pointer; RegNum: Byte): Boolean;                                                                 stdcall external 'ssvm.dll';
 Function GetEC(VM: Pointer; RegNum: Byte): Char;                                                                    stdcall external 'ssvm.dll';
 Function GetEI(VM: Pointer; RegNum: Byte): Int64;                                                                   stdcall external 'ssvm.dll';
 Function GetEF(VM: Pointer; RegNum: Byte): Extended;                                                                stdcall external 'ssvm.dll';
 Function GetES(VM: Pointer; RegNum: Byte): PChar;                                                                   stdcall external 'ssvm.dll';
 Function GetER(VM: Pointer; RegNum: Byte): Pointer;                                                                 stdcall external 'ssvm.dll';

 Procedure ThrowException(VM: Pointer; Exception: TExceptionBlock);                                                  stdcall external 'ssvm.dll';
 Function GetStopReason(VM: Pointer): TStopReason;                                                                   stdcall external 'ssvm.dll';
 Function GetException(VM: Pointer): TExceptionBlock;                                                                stdcall external 'ssvm.dll';

 (* ========== auxiliary types ========== *)
 Type EScriptError = Class(Exception);

 (* ========== auxiliary functions ========== *)
 Operator := (Value: Boolean): TMixedValue;
 Operator := (Value: Char): TMixedValue;
 Operator := (Value: Int64): TMixedValue;
 Operator := (Value: Extended): TMixedValue;
 Operator := (Value: String): TMixedValue;

 Procedure StackPush(VM: Pointer; Value: Boolean);
 Procedure StackPush(VM: Pointer; Value: Char);
 Procedure StackPush(VM: Pointer; Value: Int64);
 Procedure StackPush(VM: Pointer; Value: Extended);
 Procedure StackPush(VM: Pointer; Value: String);

 Function getBool(MV: TMixedValue): Boolean;
 Function getChar(MV: TMixedValue): Char;
 Function getInt(MV: TMixedValue): Int64;
 Function getFloat(MV: TMixedValue): Extended;
 Function getString(MV: TMixedValue): String;

 Implementation

{ CopyStringToPChar }
Function CopyStringToPChar(const S: String): PChar;
Var I: uint32;
Begin
 Result := AllocMem(Length(S)+1);

 For I := 1 To Length(S) Do
  Result[I-1] := S[I];
End;

// -------------------------------------------------------------------------- //
(* TMixedValue := Boolean *)
Operator := (Value: Boolean): TMixedValue;
Begin
 Result.Typ        := mvBool;
 Result.Value.Bool := Value;
End;

(* TMixedValue := Char *)
Operator := (Value: Char): TMixedValue;
Begin
 Result.Typ        := mvChar;
 Result.Value.Char := Value;
End;

(* TMixedValue := Int64 *)
Operator := (Value: Int64): TMixedValue;
Begin
 Result.Typ       := mvInt;
 Result.Value.Int := Value;
End;

(* TMixedValue := Extended *)
Operator := (Value: Extended): TMixedValue;
Begin
 Result.Typ         := mvFloat;
 Result.Value.Float := Value;
End;

(* TMixedValue := String *)
Operator := (Value: String): TMixedValue;
Begin
 Result.Typ       := mvString;
 Result.Value.Str := CopyStringToPChar(Value);
End;

(* StackPush (Boolean) *)
Procedure StackPush(VM: Pointer; Value: Boolean);
Var E: TMixedValue;
Begin
 E.Typ        := mvBool;
 E.Value.Bool := Value;

 StackPush(VM, E);
End;

(* StackPush (Char) *)
Procedure StackPush(VM: Pointer; Value: Char);
Var E: TMixedValue;
Begin
 E.Typ        := mvChar;
 E.Value.Char := Value;

 StackPush(VM, E);
End;

(* StackPush (Int64) *)
Procedure StackPush(VM: Pointer; Value: Int64);
Var E: TMixedValue;
Begin
 E.Typ       := mvInt;
 E.Value.Int := Value;

 StackPush(VM, E);
End;

(* StackPush (Extended) *)
Procedure StackPush(VM: Pointer; Value: Extended);
Var E: TMixedValue;
Begin
 E.Typ         := mvFloat;
 E.Value.Float := Value;

 StackPush(VM, E);
End;

(* StackPush (String) *)
Procedure StackPush(VM: Pointer; Value: String);
Var E: TMixedValue;
Begin
 E.Typ       := mvBool;
 E.Value.Str := CopyStringToPChar(Value);

 StackPush(VM, E);
End;

(* getBool *)
Function getBool(MV: TMixedValue): Boolean;
Begin
 if (MV.Typ <> mvBool) Then
  raise EScriptError.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'bool']);

 Result := MV.Value.Bool;
End;

(* getChar *)
Function getChar(MV: TMixedValue): Char;
Begin
 if (MV.Typ <> mvChar) Then
  raise EScriptError.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'char']);

 Result := MV.Value.Char;
End;

(* getInt *)
Function getInt(MV: TMixedValue): Int64;
Begin
 if (MV.Typ <> mvInt) Then
  raise EScriptError.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'int']);

 Result := MV.Value.Int;
End;

(* getFloat *)
Function getFloat(MV: TMixedValue): Extended;
Begin
 Case MV.Typ of
  mvInt  : Result := MV.Value.Int;
  mvFloat: Result := MV.Value.Float;

  else
   raise EScriptError.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'float']);
 End;
End;

(* getString *)
Function getString(MV: TMixedValue): String;
Begin
 if (MV.Typ <> mvString) Then
  raise EScriptError.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'string']);

 Result := MV.Value.Str;
End;
End.
