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
{$H+}
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
 Type PVMString = ^VMString;
      VMString =
      Record
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
        Str  : PVMString;
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

 { TLogMode }
 Type TLogMode = (lmDisabled, lmConsole, lmFile);

 { TVMDataStructure }
 Type TVMDataStructure =
      Packed Record
       FileName : PChar;
       GCMemSize: uint32;

       LogMode: TLogMode;
       LogFile: PChar;
      End;

 { TCallHandler }
 Type TCallHandler = Procedure (VM: Pointer; Params: PMixedValue; Result: PMixedValue) register;

 { TStopReason }
 Type TStopReason = (srFinished, srException);

 (* ========== constants ========== *)
 Const MixedValueTypeNames: Array[TMixedValueType] of String = ('none', 'bool', 'char', 'int', 'float', 'string', 'reference');

 (* ========== functions imported from the DLL ========== *)
 Function SSCreateAndLoad(const Struct: TVMDataStructure): Pointer;                                                                                    stdcall external 'ssvm.dll' name 'CreateAndLoad';
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
 Function SSGetExceptionAddress(const VM: Pointer): uint32;                                                                                            stdcall external 'ssvm.dll' name 'GetExceptionAddress';
 Function SSGetStopReason(const VM: Pointer): TStopReason;                                                                                             stdcall external 'ssvm.dll' name 'GetStopReason';

 Function SSGetLineData(const VM: Pointer; const Address: uint32; out FileName, FunctionName: PChar; out FunctionLine: uint32): Boolean;               stdcall external 'ssvm.dll' name 'GetLineData';

 Function SSAllocateString(const VM: Pointer; const Content: PChar): PVMString                                                                         stdcall external 'ssvm.dll' name 'AllocateString';

 Function SSGetVMVersion: PChar;                                                                                                                       stdcall external 'ssvm.dll' name 'GetVMVersion';

 (* ========== auxiliary types ========== *)
 Type ESScriptException = Class(Exception);

 (* ========== auxiliary functions ========== *)
 Function SSConvertBool(const Value: VMBool): TMixedValue;
 Function SSConvertChar(const Value: VMChar): TMixedValue;
 Function SSConvertInt(const Value: VMInt): TMixedValue;
 Function SSConvertFloat(const Value: VMFloat): TMixedValue;
 Function SSConvertString(const VM: Pointer; const Value: String): TMixedValue;

 Operator := (const Value: VMBool): TMixedValue;
 Operator := (const Value: VMChar): TMixedValue;
 Operator := (const Value: VMInt): TMixedValue;
 Operator := (const Value: VMFloat): TMixedValue;

 Procedure SSStackPush(const VM: Pointer; const Value: VMBool);
 Procedure SSStackPush(const VM: Pointer; const Value: VMChar);
 Procedure SSStackPush(const VM: Pointer; const Value: VMInt);
 Procedure SSStackPush(const VM: Pointer; const Value: VMFloat);
 Procedure SSStackPush(const VM: Pointer; const Value: PVMString);

 Function SSGetBool(const MV: TMixedValue): VMBool;
 Function SSGetChar(const MV: TMixedValue): VMChar;
 Function SSGetInt(const MV: TMixedValue): VMInt;
 Function SSGetFloat(const MV: TMixedValue): VMFloat;
 Function SSGetString(const MV: TMixedValue): PVMString;
 Function SSGetString(const VMStr: PVMString): String;

 Implementation

(* SSConvertBool *)
{
 Converts a boolean value to mixed-value.
}
Function SSConvertBool(const Value: VMBool): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ        := mvBool;
 Result.Value.Bool := Value;
End;

(* SSConvertChar *)
{
 Converts a char to mixed-value.
}
Function SSConvertChar(const Value: VMChar): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ        := mvChar;
 Result.Value.Char := Value;
End;

(* SSConvertInt *)
{
 Converts an integer number to mixed-value.
}
Function SSConvertInt(const Value: VMInt): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ       := mvInt;
 Result.Value.Int := Value;
End;

(* SSConvertFloat *)
{
 Converts a float number to mixed-value.
}
Function SSConvertFloat(const Value: VMFloat): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ         := mvFloat;
 Result.Value.Float := Value;
end;

(* SSConvertString *)
{
 Converts a string to mixed-value.
}
Function SSConvertString(const VM: Pointer; const Value: String): TMixedValue;
Begin
 FillByte(Result, sizeof(Result), 0);
 Result.Typ       := mvString;
 Result.Value.Str := SSAllocateString(VM, PChar(Value));
End;

(* TMixedValue := VMBool *)
Operator := (const Value: VMBool): TMixedValue;
Begin
 Result := SSConvertBool(Value);
End;

(* TMixedValue := VMChar *)
Operator := (const Value: VMChar): TMixedValue;
Begin
 Result := SSConvertChar(Value);
End;

(* TMixedValue := VMInt *)
Operator := (const Value: VMInt): TMixedValue;
Begin
 Result := SSConvertInt(Value);
End;

(* TMixedValue := VMFloat *)
Operator := (const Value: VMFloat): TMixedValue;
Begin
 Result := SSConvertFloat(Value);
End;

(* SSStackPush (VMBool) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMBool);
Begin
 SSStackPush(VM, SSConvertBool(Value));
End;

(* SSStackPush (VMChar) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMChar);
Begin
 SSStackPush(VM, SSConvertChar(Value));
End;

(* SSStackPush (VMInt) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMInt);
Begin
 SSStackPush(VM, SSConvertInt(Value));
End;

(* SSStackPush (VMFloat) *)
Procedure SSStackPush(const VM: Pointer; const Value: VMFloat);
Begin
 SSStackPush(VM, SSConvertFloat(Value));
End;

(* SSStackPush (VMString) *)
Procedure SSStackPush(const VM: Pointer; const Value: PVMString);
Begin
 SSStackPush(VM, Value);
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
Function SSGetString(const MV: TMixedValue): PVMString;
Begin
 if (MV.Typ <> mvString) Then
  raise ESScriptException.CreateFmt('Invalid type! Got `%s`, expected `%s`!', [MixedValueTypeNames[MV.Typ], 'string']);

 Result := MV.Value.Str;
End;

(* SSGetString *)
Function SSGetString(const VMStr: PVMString): String;
Var I: uint32;
Begin
 Result := '';

 if (VMStr^.Length > 0) Then
  For I := 0 To VMStr^.Length-1 Do
   Result += VMStr^.Data[I];
End;
End.
