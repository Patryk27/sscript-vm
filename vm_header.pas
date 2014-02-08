(*
 SScript Virtual Machine Library Header
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
Unit vm_header;

 Interface
 Uses SysUtils;

 (* ========== types ========== *)
 { primary }
 Type VMBool      = Boolean;
      VMChar      = Char;
      VMInt       = Int64;
      VMFloat     = Extended;
      VMReference = Pointer;

 { VMString }
 Type VMString =
     Packed Record
      Length: uint32;
      Data  : PChar;
     End;

 { TMixedValue }
 Type TMixedValueType = (mvNone=-1, mvBool, mvChar, mvInt, mvFloat, mvString, mvReference);
 Type PMixedValue = ^TMixedValue;
      TMixedValue =
      Packed Record
       Typ  : TMixedValueType;
       Value:
       Record
        Bool : VMBool;
        Char : VMChar;
        Int  : VMInt;
        Float: VMFloat;
        Str  : VMString;
       End;

       VMInternalUse: Array[1..12] of uint8;
      End;

 { TMixedValueArray }
 Type TMixedValueArray = Array of TMixedValue;

 { TExceptionBlock }
 Type TExceptionType = (etByObject, etByMessage);
 Type PExceptionBlock = ^TExceptionBlock;
      TExceptionBlock =
      Record
       Typ : TExceptionType; // exception type ("throw object;" or "throw string;")
       Data: Pointer;
      End;

 { TCallHandler }
 Type TCallHandler = Procedure (VM: Pointer; Params: PMixedValue; Result: PMixedValue) register;

 { TStopReason }
 Type TStopReason = (srFinished, srException);

 (* ========== constants ========== *)
 Const MixedValueTypeNames: Array[TMixedValueType] of String = ('none', 'bool', 'char', 'int', 'float', 'string', 'reference');

 (* ========== functions imported from the DLL ========== *)
 Function SSCreateAndLoad(const FileName: PChar; const GCMemoryLimit: uint32): Pointer;                                                                stdcall external 'ssvm.dll' name 'CreateAndLoad';
 Function SSExecuteVM(const VM: Pointer): Boolean;                                                                                                     stdcall external 'ssvm.dll' name 'ExecuteVM';
 Procedure SSFreeVM(const VM: Pointer);                                                                                                                stdcall external 'ssvm.dll' name 'FreeVM';

 Function SSJITCompile(const VM: Pointer): Boolean;                                                                                                    stdcall external 'ssvm.dll' name 'JITCompile';
 Function SSGetJITError(const VM: Pointer): PChar;                                                                                                     stdcall external 'ssvm.dll' name 'GetJITError';
 Function SSGetJITCodeData(const VM: Pointer): Pointer;                                                                                                stdcall external 'ssvm.dll' name 'GetJITCodeData';
 Function SSGetJITCodeSize(const VM: Pointer): uint32;                                                                                                 stdcall external 'ssvm.dll' name 'GetJITCodeSize';

 Function SSAddInternalCall(const VM: Pointer; const PackageName, FunctionName: PChar; const ParamCount: uint8; const Handler: TCallHandler): Boolean; stdcall external 'ssvm.dll' name 'AddInternalCall';

 Procedure SSStackPush(const VM: Pointer; const Value: TMixedValue);                                                                                   stdcall external 'ssvm.dll' name 'StackPush';
 Function SSStackPop(const VM: Pointer): TMixedValue;                                                                                                  stdcall external 'ssvm.dll' name 'StackPop';

 Procedure SSThrowException(const VM: Pointer; const Exception: TExceptionBlock);                                                                      stdcall external 'ssvm.dll' name 'ThrowException';
 Function SSGetException(const VM: Pointer): TExceptionBlock;                                                                                          stdcall external 'ssvm.dll' name 'GetException';
 Function SSGetStopReason(const VM: Pointer): TStopReason;                                                                                             stdcall external 'ssvm.dll' name 'GetStopReason';

 Function SSGetVMVersion: PChar;                                                                                                                       stdcall external 'ssvm.dll' name 'GetVMVersion';

 (* ========== auxiliary types ========== *)
 Type ESScriptException = Class(Exception);

 (* ========== auxiliary functions ========== *)
 Operator := (const Value: VMBool): TMixedValue;
 Operator := (const Value: VMChar): TMixedValue;
 Operator := (const Value: VMInt): TMixedValue;
 Operator := (const Value: VMFloat): TMixedValue;
 Operator := (const Value: String): TMixedValue;

 Procedure SSStackPush(const VM: Pointer; const Value: VMBool);
 Procedure SSStackPush(const VM: Pointer; const Value: VMChar);
 Procedure SSStackPush(const VM: Pointer; const Value: VMInt);
 Procedure SSStackPush(const VM: Pointer; const Value: VMFloat);
 Procedure SSStackPush(const VM: Pointer; const Value: VMString);

 Function SSGetBool(const MV: TMixedValue): VMBool;
 Function SSGetChar(const MV: TMixedValue): VMChar;
 Function SSGetInt(const MV: TMixedValue): VMInt;
 Function SSGetFloat(const MV: TMixedValue): VMFloat;
 Function SSGetString(const MV: TMixedValue): VMString;
 Function SSGetString(const VMStr: VMString): String;

 Implementation

(* TMixedValue := VMBool *)
Operator := (const Value: VMBool): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ        := mvBool;
 Result.Value.Bool := Value;
End;

(* TMixedValue := VMChar *)
Operator := (const Value: VMChar): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ        := mvChar;
 Result.Value.Char := Value;
End;

(* TMixedValue := VMInt *)
Operator := (const Value: VMInt): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ       := mvInt;
 Result.Value.Int := Value;
End;

(* TMixedValue := VMFloat *)
Operator := (const Value: VMFloat): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ         := mvFloat;
 Result.Value.Float := Value;
End;

(* TMixedValue := String *)
Operator := (const Value: String): TMixedValue;
Var Str: VMString;
    I  : uint32;
Begin
 FillByte(Result.VMInternalUse[0], Length(TMixedValue.VMInternalUse), 0);

 Str.Length := Length(Value);
 Str.Data   := GetMem(Str.Length);

 if (Str.Length > 0) Then
 Begin
  For I := 0 To Str.Length-1 Do
   Str.Data[I] := Value[I+1];
 End;

 Result.Typ       := mvString;
 Result.Value.Str := Str;
End;

(* SSStackPush (VMBool) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMBool);
Var E: TMixedValue;
Begin
 E.Typ        := mvBool;
 E.Value.Bool := Value;

 SSStackPush(VM, E);
End;

(* SSStackPush (VMChar) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMChar);
Var E: TMixedValue;
Begin
 E.Typ        := mvChar;
 E.Value.Char := Value;

 SSStackPush(VM, E);
End;

(* SSStackPush (VMInt) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMInt);
Var E: TMixedValue;
Begin
 E.Typ       := mvInt;
 E.Value.Int := Value;

 SSStackPush(VM, E);
End;

(* SSStackPush (VMFloat) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMFloat);
Var E: TMixedValue;
Begin
 E.Typ         := mvFloat;
 E.Value.Float := Value;

 SSStackPush(VM, E);
End;

(* SSStackPush (VMString) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMString);
Var E: TMixedValue;
Begin
 E.Typ       := mvBool;
 E.Value.Str := Value;

 SSStackPush(VM, E);
End;

(* SSGetBool *)
Function SSGetBool(const MV: TMixedValue): VMBool;
Begin
 if (MV.Typ <> mvBool) Then
  raise ESScriptException.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'bool']);

 Result := MV.Value.Bool;
End;

(* SSGetChar *)
Function SSGetChar(const MV: TMixedValue): VMChar;
Begin
 if (MV.Typ <> mvChar) Then
  raise ESScriptException.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'char']);

 Result := MV.Value.Char;
End;

(* SSGetInt *)
Function SSGetInt(const MV: TMixedValue): VMInt;
Begin
 if (MV.Typ <> mvInt) Then
  raise ESScriptException.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'int']);

 Result := MV.Value.Int;
End;

(* SSGetFloat *)
Function SSGetFloat(const MV: TMixedValue): VMFloat;
Begin
 Case MV.Typ of
  mvInt  : Result := MV.Value.Int;
  mvFloat: Result := MV.Value.Float;

  else
   raise ESScriptException.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'float']);
 End;
End;

(* SSGetString *)
Function SSGetString(const MV: TMixedValue): VMString;
Begin
 if (MV.Typ <> mvString) Then
  raise ESScriptException.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'string']);

 Result := MV.Value.Str;
End;

(* SSGetString *)
Function SSGetString(const VMStr: VMString): String;
Var I: uint32;
Begin
 Result := '';

 if (VMStr.Length > 0) Then
  For I := 0 To VMStr.Length-1 Do
   Result += VMStr.Data[I];
End;
End.
