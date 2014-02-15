(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

{$ASMMODE INTEL}

{ getStackvalElement }
Function getStackvalElement(const VM: PVM; const StackvalPos: int32): PMixedValue; inline; // helper function!
Begin
 Result := VM^.Stack.getPointer(VM^.Regs.i[5] + StackvalPos);
End;

(* ------------------ string operations ------------------ *)
{ r__clone_string }
Function r__clone_string(const Arg: PString): PString;
Begin
 New(Result);
 Result^ := Arg^;
End;

(* ------------------ integer operations ------------------ *)
{ r__div_imem_iconst }
Procedure r__div_imem_iconst(const NumA: Pint64; const NumB_lo, NumB_hi: uint32); register;
Begin
 NumA^ := NumA^ div ((int64(NumB_hi) << 32) + NumB_lo);
End;

{ r__mod_imem_iconst }
Procedure r__mod_imem_iconst(const NumA: Pint64; const NumB_lo, NumB_hi: uint32); register;
Begin
 NumA^ := NumA^ mod ((int64(NumB_hi) << 32) + NumB_lo);
End;

(* ------------------ push ------------------ *)
{ r__push_bool }
Procedure r__push_bool(const VM: PVM; const Value: VMBool); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ        := mvBool;
 MV.Value.Bool := Value;
 VM^.Stack.Push(MV);
End;

{ r__push_char }
Procedure r__push_char(const VM: PVM; const Value: VMChar); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ        := mvChar;
 MV.Value.Char := Value;
 VM^.Stack.Push(MV);
End;

{ r__push_int }
Procedure r__push_int(const VM: PVM; const Value_lo, Value_hi: int32); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvint;
 MV.Value.Int := (int64(Value_hi) << 32) + Value_lo;
 VM^.Stack.Push(MV);
End;

{ r__push_float }
Procedure r__push_float(const VM: PVM); register;
Var MV   : TMixedValue;
    Float: VMFloat;
Begin
 asm
  fstp VMFloat Float
 end;

 MV.Reset;
 MV.Typ         := mvFloat;
 MV.Value.Float := Float;
 VM^.Stack.Push(MV);
End;

{ r__push_string }
Procedure r__push_string(const VM: PVM; const Value: PVMString); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvString;
 MV.Value.Str := Value;
 VM^.Stack.Push(MV);
End;

{ r__push_reference }
Procedure r__push_reference(const VM: PVM; const Value: uint32); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvReference;
 MV.Value.Int := Value;
 VM^.Stack.Push(MV);
End;

{ r__push_stackval }
Procedure r__push_stackval(const VM: PVM; const StackvalPos: int32); register;
Begin
 VM^.Stack.Push(getStackvalElement(VM, StackvalPos)^);
End;

(* ------------------ pop ------------------ *)
{ PopCheck }
Procedure PopCheck(const VM: PVM); register;
Begin
 if (VM^.Regs.i[5] <= 0) Then
  VM^.ThrowException('Cannot do ''pop'' - there''s nothing on the stack!');
End;

{ r__pop_reference }
Function r__pop_reference(const VM: PVM): Pointer; register;
Begin
 PopCheck(VM);

 Result := VM^.getReference(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_bool_reg }
Procedure r__pop_bool_reg(const VM: PVM; const RegAddr: PBoolean); register;
Begin
 RegAddr^ := VM^.getBool(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_char_reg }
Procedure r__pop_char_reg(const VM: PVM; const RegAddr: PChar); register;
Begin
 RegAddr^ := VM^.getChar(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_int_reg }
Procedure r__pop_int_reg(const VM: PVM; const RegAddr: pint64); register;
Begin
 RegAddr^ := VM^.getInt(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_float_reg }
Procedure r__pop_float_reg(const VM: PVM; const RegAddr: PVMFloat); register;
Begin
 RegAddr^ := VM^.getFloat(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_string_reg }
Procedure r__pop_string_reg(const VM: PVM; const RegAddr: PPVMString); register;
Begin
 RegAddr^ := VM^.getString(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

{ r__pop_reference_reg }
Procedure r__pop_reference_reg(const VM: PVM; const RegAddr: PPointer); register;
Begin
 RegAddr^ := r__pop_reference(VM);
End;

(* ------------------ internal calls ------------------ *)
{ r__create_icall_parameter_list }
Procedure r__create_icall_parameter_list(const VM: PVM; const call: PInternalCall; const ParamsMV: PMixedValue); register;
Var I: int8;
Begin
 For I := 0 To int8(call^.ParamCount)-1 Do
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
  VM^.Stack.Push(MV^);
End;

(* ------------------ stackvals ------------------ *)
{ r__stackval_fetch_bool }
Function r__stackval_fetch_bool(const VM: PVM; const StackvalPos: int32): Boolean; register;
Begin
 Result := VM^.getBool(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_char }
Function r__stackval_fetch_char(const VM: PVM; const StackvalPos: int32): Char; register;
Begin
 Result := VM^.getChar(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_int }
Function r__stackval_fetch_int(const VM: PVM; const StackvalPos: int32): int64; register;
Begin
 Result := VM^.getInt(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_float }
Function r__stackval_fetch_float(const VM: PVM; const StackvalPos: int32): VMFloat; register;
Begin
 Result := VM^.getFloat(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_string }
Function r__stackval_fetch_string(const VM: PVM; const StackvalPos: int32): PVMString; register;
Begin
 Result := VM^.getString(getStackvalElement(VM, StackvalPos)^);
End;

{ r__stackval_fetch_reference }
Function r__stackval_fetch_reference(const VM: PVM; const StackvalPos: int32): Pointer; register;
Begin
 Result := VM^.getReference(getStackvalElement(VM, StackvalPos)^);
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
   VM^.ThrowException('r__add_stackval_int() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
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
   VM^.ThrowException('r__sub_stackval_int() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
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
   VM^.ThrowException('r__mul_stackval_int() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
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
   VM^.ThrowException('r__div_stackval_int() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
 End;
End;

{ r__mod_stackval_int }
Procedure r__mod_stackval_int(const VM: PVM; const StackvalPos: int32; const Value: int64); register;
Var MV: PMixedValue;
Begin
 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt: MV^.Value.Int := MV^.Value.Int mod Value;

  else
   VM^.ThrowException('r__mod_stackval_int() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
 End;
End;

{ r__add_stackval_float }
Procedure r__add_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: VMFloat;
Begin
 asm
  fstp VMFloat Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int += Round(Value);
  mvFloat: MV^.Value.Float += Value;

  else
   VM^.ThrowException('r__add_stackval_float() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
 End;
End;

{ r__sub_stackval_float }
Procedure r__sub_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: VMFloat;
Begin
 asm
  fstp VMFloat Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int -= Round(Value);
  mvFloat: MV^.Value.Float -= Value;

  else
   VM^.ThrowException('r__sub_stackval_float() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
 End;
End;

{ r__mul_stackval_float }
Procedure r__mul_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: VMFloat;
Begin
 asm
  fstp VMFloat Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int *= Round(Value);
  mvFloat: MV^.Value.Float *= Value;

  else
   VM^.ThrowException('r__mul_stackval_float() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
 End;
End;

{ r__div_stackval_float }
Procedure r__div_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var MV   : PMixedValue;
    Value: VMFloat;
Begin
 asm
  fstp VMFloat Value
 end;

 MV := getStackvalElement(VM, StackvalPos);

 Case MV^.Typ of
  mvInt  : MV^.Value.Int := MV^.Value.Int div Round(Value);
  mvFloat: MV^.Value.Float /= Value;

  else
   VM^.ThrowException('r__div_stackval_float() cannot be executed on a non-numeric type #%d', [ord(MV^.Typ)]);
 End;
End;

{ r__set_stackval_bool }
Procedure r__set_stackval_bool(const VM: PVM; const StackvalPos: int32; const NewValue: Boolean); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  Typ        := mvBool;
  Value.Bool := NewValue;
 End;
End;

{ r__set_stackval_char }
Procedure r__set_stackval_char(const VM: PVM; const StackvalPos: int32; const NewValue: Char); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  Typ        := mvChar;
  Value.Char := NewValue;
 End;
End;

{ r__set_stackval_int }
Procedure r__set_stackval_int(const VM: PVM; const StackvalPos: int32; const NewValue: int64); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  Typ       := mvInt;
  Value.Int := NewValue;
 End;
End;

{ r__set_stackval_float }
Procedure r__set_stackval_float(const VM: PVM; const StackvalPos: int32); register;
Var NewValue: VMFloat;
Begin
 asm
  fstp VMFloat NewValue
 end;

 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  Typ         := mvFloat;
  Value.Float := NewValue;
 End;
End;

{ r__set_stackval_string }
Procedure r__set_stackval_string(const VM: PVM; const StackvalPos: int32; const NewValue: PVMString); register;
Begin
 With getStackvalElement(VM, StackvalPos)^ do
 Begin
  Typ       := mvString;
  Value.Str := NewValue; // @TODO: is it safe?
 End;
End;
