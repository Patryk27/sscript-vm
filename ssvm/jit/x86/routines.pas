(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

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
 VM^.StackPush(MV);
End;

{ r__push_char }
Procedure r__push_char(const VM: PVM; const Value: VMChar); register;
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
Procedure r__push_float_mem(const VM: PVM; const Addr: PVMFloat); register;
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

(* ------------------ pop ------------------ *)
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
