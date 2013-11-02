(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 Part of the JIT compiler for x86 architecture.
*)
{$ASMMODE INTEL}

(* ------------------ PUSH ------------------ *)
{ r__push_bool }
Procedure r__push_bool(const VM: PVM; const Value: Boolean); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ        := mvBool;
 MV.Value.Bool := Value;
 VM^.StackPush(MV);
End;

{ r__push_char }
Procedure r__push_char(const VM: PVM; const Value: Char); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ        := mvChar;
 MV.Value.Char := Value;
 VM^.StackPush(MV);
End;

{ r__push_int }
Procedure r__push_int(const VM: PVM; const Value_lo, Value_hi: int32); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvint;
 MV.Value.Int := (int64(Value_hi) << 32) + Value_lo;
 VM^.StackPush(MV);
End;

{ r__push_float_mem }
Procedure r__push_float_mem(const VM: PVM; const Addr: PExtended); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ         := mvFloat;
 MV.Value.Float := Addr^;
 VM^.StackPush(MV);
End;

{ r__push_string }
Procedure r__push_string(const VM: PVM; const Value: PChar); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvString;
 MV.Value.Str := Value;
 VM^.StackPush(MV);
End;

{ r__push_reference }
Procedure r__push_reference(const VM: PVM; const Value: uint32); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvReference;
 MV.Value.Int := Value;
 VM^.StackPush(MV);
End;

(* ------------------ POP (functions) ------------------ *)
{ PopCheck }
Procedure PopCheck(const VM: PVM); register;
Begin
 if (VM^.Regs.i[5] <= 0) Then
  raise Exception.Create('Cannot do ''pop'' - there''s nothing on the stack!');
End;

{ r__pop_reference }
Function r__pop_reference(const VM: PVM): Pointer; register;
Begin
 PopCheck(VM);

 Result := getReference(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

(* ------------------ POP (procedures) ------------------ *)

{ r__pop_bool_reg }
Procedure r__pop_bool_reg(const VM: PVM; const RegAddr: PBoolean); register;
Begin
 RegAddr^ := getBool(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_char_reg }
Procedure r__pop_char_reg(const VM: PVM; const RegAddr: PChar); register;
Begin
 RegAddr^ := getChar(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_int_reg }
Procedure r__pop_int_reg(const VM: PVM; const RegAddr: pint64); register;
Begin
 RegAddr^ := getInt(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_float_reg }
Procedure r__pop_float_reg(const VM: PVM; const RegAddr: PExtended); register;
Begin
 RegAddr^ := getFloat(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_string_reg }
Procedure r__pop_string_reg(const VM: PVM; const RegAddr: PString); register;
Begin
 RegAddr^ := getString(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_reference_reg }
Procedure r__pop_reference_reg(const VM: PVM; const RegAddr: PPointer); register;
Begin
 RegAddr^ := r__pop_reference(VM);
End;

(* ------------------ internal calls ------------------ *)
{ r__create_icall_parameter_list }
Procedure r__create_icall_parameter_list(const VM: PVM; const icall: PCall; const ParamsMV: PMixedValue); register;
Var I: int8;
Begin
 For I := 0 To int8(icall^.ParamCount)-1 Do
 Begin
  ParamsMV[I] := VM^.Stack[VM^.Regs.i[5]];
  Dec(VM^.Regs.i[5]);
 End;
End;

(* ------------------ mixed-values ------------------ *)
{ r__clean_mixedvalue }
Procedure r__clean_mixedvalue(const MV: PMixedValue); register;
Begin
 MV^.Reset;
End;

{ r__apply_mixedvalue }
Procedure r__apply_mixedvalue(const VM: PVM; const MV: PMixedValue); register;
Begin
 if (MV^.Typ <> mvNone) Then
  VM^.StackPush(MV^);
End;

(* ------------------ int operations ------------------ *)
{ r__div_memint_immint }
Procedure r__div_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ div (int64(Value_hi) << 32 + Value_lo);
End;

{ r__mod_memint_immint }
Procedure r__mod_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ mod (int64(Value_hi) << 32 + Value_lo);
End;

{ r__shl_memint_immint }
Procedure r__shl_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ << (int64(Value_hi) << 32 + Value_lo);
End;

{ r__shr_memint_immint }
Procedure r__shr_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ >> (int64(Value_hi) << 32 + Value_lo);
End;

(* ------------------ string operations ------------------ *)
{ r__concat_string }
Function r__concat_string(const A, B: PChar): PChar; register;
Var LenA, LenB: uint32;
Begin
 LenA := Length(A);
 LenB := Length(B);

 Result := AllocMem(LenA + LenB + 1);

 Move(A[0], Result[0], LenA);
 Move(B[0], Result[LenA], LenB);

 // AllocMem automatically zeroes allocated area so we don't have to do any "Result[LenA+LenB+1] := #0;"
End;

(* ------------------ stack operations ------------------ *)
{ getStackvalElement }
Function getStackvalElement(const VM: PVM; const StackvalPos: int32): PMixedValue; inline; // helper function!
Begin
 Result := @VM^.Stack[VM^.Regs.i[5] + StackvalPos];
End;

{ r__add_stackval_int }
Procedure r__add_stackval_int(const VM: PVM; const StackvalPos: int32; const Value: int64); register;
Var MV: PMixedValue;
Begin
 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int += Value;
  mvFloat: MV^.Value.Float += Value;

  else
   raise Exception.CreateFmt('r__add_stackval_int() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__sub_stackval_int }
Procedure r__sub_stackval_int(const VM: PVM; const StackvalPos: int32; const Value: int64); register;
Var MV: PMixedValue;
Begin
 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int -= Value;
  mvFloat: MV^.Value.Float -= Value;

  else
   raise Exception.CreateFmt('r__sub_stackval_int() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__mul_stackval_int }
Procedure r__mul_stackval_int(const VM: PVM; const StackvalPos: int32; const Value: int64); register;
Var MV: PMixedValue;
Begin
 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int *= Value;
  mvFloat: MV^.Value.Float *= Value;

  else
   raise Exception.CreateFmt('r__mul_stackval_int() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__div_stackval_int }
Procedure r__div_stackval_int(const VM: PVM; const StackvalPos: int32; const Value: int64); register;
Var MV: PMixedValue;
Begin
 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int := MV^.Value.Int div Value;
  mvFloat: MV^.Value.Float /= Value;

  else
   raise Exception.CreateFmt('r__div_stackval_int() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__add_stackval_float }
Procedure r__add_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int += Round(Value);
  mvFloat: MV^.Value.Float += Value;

  else
   raise Exception.CreateFmt('r__add_stackval_float() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__sub_stackval_float }
Procedure r__sub_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int -= Round(Value);
  mvFloat: MV^.Value.Float -= Value;

  else
   raise Exception.CreateFmt('r__sub_stackval_float() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__mul_stackval_float }
Procedure r__mul_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int *= Round(Value);
  mvFloat: MV^.Value.Float *= Value;

  else
   raise Exception.CreateFmt('r__mul_stackval_float() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__div_stackval_float }
Procedure r__div_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int := MV^.Value.Int div Round(Value);
  mvFloat: MV^.Value.Float /= Value;

  else
   raise Exception.CreateFmt('r__div_stackval_float() cannot be executed on a non-numeric type `%d`', [ord(MV^.Typ)]);
 End;
End;

{ r__push_stackval }
Procedure r__push_stackval(const VM: PVM; const StackvalPos: int32); register;
Begin
 VM^.StackPush(getStackvalElement(VM, StackvalPos)^);
End;

{ r__move_stackval_bool }
Procedure r__move_stackval_bool(const VM: PVM; const StackvalPos: int32; const NewValue: Boolean); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  ReleaseData;

  Typ        := mvBool;
  Value.Bool := NewValue;
 End;
End;

{ r__move_stackval_char }
Procedure r__move_stackval_char(const VM: PVM; const StackvalPos: int32; const NewValue: Char); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  ReleaseData;

  Typ        := mvChar;
  Value.Char := NewValue;
 End;
End;

{ r__move_stackval_int }
Procedure r__move_stackval_int(const VM: PVM; const StackvalPos: int32; const NewValue: int64); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  ReleaseData;

  Typ       := mvInt;
  Value.Int := NewValue;
 End;
End;

{ r__move_stackval_float }
Procedure r__move_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var NewValue: Extended;
Begin
 asm
  fstp Extended NewValue
 end;

 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  ReleaseData;

  Typ         := mvFloat;
  Value.Float := NewValue;
 End;
End;

{ r__move_stackval_string }
Procedure r__move_stackval_string(const VM: PVM; const StackvalPos: int32; const NewValue: PChar); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  ReleaseData;

  Typ       := mvString;
  Value.Str := CopyStringToPChar(NewValue);
 End;
End;

{ r__stackval_fetch_bool }
Function r__stackval_fetch_bool(const VM: PVM; const StackvalPos: int32): Boolean; register;
Begin
 Result := getBool(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_char }
Function r__stackval_fetch_char(const VM: PVM; const StackvalPos: int32): Char; register;
Begin
 Result := getChar(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_int }
Function r__stackval_fetch_int(const VM: PVM; const StackvalPos: int32): int64; register;
Begin
 Result := getInt(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_float }
Function r__stackval_fetch_float(const VM: PVM; const StackvalPos: int32): Float; register;
Begin
 Result := getFloat(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_string }
Function r__stackval_fetch_string(const VM: PVM; const StackvalPos: int32): PChar; register;
Begin
 Result := getString(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_reference }
Function r__stackval_fetch_reference(const VM: PVM; const StackvalPos: int32): Pointer; register;
Begin
 Result := getReference(getStackvalElement(VM, StackvalPos)^);
End;
