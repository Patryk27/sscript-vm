(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 JIT Compiler for the x86 architecture.
*)
{$ASMMODE INTEL}

{ ConstructMixedValue }
Function ConstructMixedValue(const Typ: TOpcodeArgType; const DataPointer: Pointer): TMixedValue;
Var Float: Extended;
Begin
 Case Typ of
  ptBool:
  Begin
   Result.Typ        := mvBool;
   Result.Value.Bool := PBoolean(DataPointer)^;
  End;

  ptChar:
  Begin
   Result.Typ        := mvChar;
   Result.Value.Char := PChar(DataPointer)^;
  End;

  ptInt:
  Begin
   Result.Typ       := mvInt;
   Result.Value.Int := pint64(DataPointer)^;
  End;

  ptFloat:
  Begin
   asm
    fstp Float
   end;

   Result.Typ         := mvFloat;
   Result.Value.Float := Float;
  End;

  ptString:
  Begin
   Result.Typ       := mvString;
   Result.Value.Str := PPChar(DataPointer)^;
  End;

  else
   raise Exception.CreateFmt('ConstructMixedValue -> unknown type %d', [ord(Typ)]);
 End;
End;

// -------------------------------------------------------------------------- //
(* __stack_push_bool *)
Procedure __stack_push_bool(const StackPnt: PStack; const reg_stp: pint32; const Value: Boolean); stdcall;
Begin
 Inc(reg_stp^);
 StackPnt^[reg_stp^].Typ        := mvBool;
 StackPnt^[reg_stp^].Value.Bool := Value;
End;

(* __stack_push_char *)
Procedure __stack_push_char(const StackPnt: PStack; const reg_stp: pint32; const Value: Char); stdcall;
Begin
 Inc(reg_stp^);
 StackPnt^[reg_stp^].Typ        := mvChar;
 StackPnt^[reg_stp^].Value.Char := Value;
End;

(* __stack_push_int *)
Procedure __stack_push_int(const StackPnt: PStack; const reg_stp: pint32; const Value: int64); stdcall;
Begin
 Inc(reg_stp^);
 StackPnt^[reg_stp^].Typ       := mvInt;
 StackPnt^[reg_stp^].Value.Int := Value;
End;

(* __stack_push_float *)
Procedure __stack_push_float(const StackPnt: PStack; const reg_stp: pint32); stdcall;
Var Value: Extended;
Begin
 asm
  fstp Value
 end;

 Inc(reg_stp^);
 StackPnt^[reg_stp^].Typ         := mvFloat;
 StackPnt^[reg_stp^].Value.Float := Value;
End;

(* __stack_push_string *)
Procedure __stack_push_string(const StackPnt: PStack; const reg_stp: pint32; const Value: PChar); stdcall;
Begin
 Inc(reg_stp^);
 StackPnt^[reg_stp^].Typ       := mvString;
 StackPnt^[reg_stp^].Value.Str := Value;
End;

(* __stack_push_reference *)
Procedure __stack_push_reference(const StackPnt: PStack; const reg_stp: pint32; const Value: Pointer); stdcall;
Begin
 Inc(reg_stp^);
 StackPnt^[reg_stp^].Typ       := mvReference;
 StackPnt^[reg_stp^].Value.Int := uint32(Value);
End;

(* __stack_push_stackval *)
Procedure __stack_push_stackval(const StackPnt: PStack; const reg_stp: pint32; const StackvalPos: int32); stdcall;
Begin
 Inc(reg_stp^);
 StackPnt^[reg_stp^] := StackPnt^[reg_stp^+StackvalPos-1];
End;

(* __stack_pop_bool_reg *)
Procedure __stack_pop_bool_reg(const StackPnt: PStack; const reg_stp: pint32; const RegMem: PBoolean); stdcall;
Begin
 RegMem^ := getBool(StackPnt^[reg_stp^]);
 Dec(reg_stp^);
End;

(* __stack_pop_char_reg *)
Procedure __stack_pop_char_reg(const StackPnt: PStack; const reg_stp: pint32; const RegMem: PChar); stdcall;
Begin
 RegMem^ := getChar(StackPnt^[reg_stp^]);
 Dec(reg_stp^);
End;

(* __stack_pop_int_reg *)
Procedure __stack_pop_int_reg(const StackPnt: PStack; const reg_stp: pint32; const RegMem: pint64); stdcall;
Begin
 RegMem^ := getInt(StackPnt^[reg_stp^]);
 Dec(reg_stp^);
End;

(* __stack_pop_float_reg *)
Procedure __stack_pop_float_reg(const StackPnt: PStack; const reg_stp: pint32; const RegMem: PExtended); stdcall;
Begin
 RegMem^ := getFloat(StackPnt^[reg_stp^]);
 Dec(reg_stp^);
End;

(* __stack_pop_string_reg *)
Procedure __stack_pop_string_reg(const StackPnt: PStack; const reg_stp: pint32; const RegMem: PPChar); stdcall;
Begin
 RegMem^ := getString(StackPnt^[reg_stp^]);
 Dec(reg_stp^);
End;

(* __stack_pop_reference_reg *)
Procedure __stack_pop_reference_reg(const StackPnt: PStack; const reg_stp: pint32; const RegMem: PPointer); stdcall;
Begin
 RegMem^ := getReference(StackPnt^[reg_stp^]);
 Dec(reg_stp^);
End;

(* __boolreg_stackval_assign *)
Procedure __boolreg_stackval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const dest_reg: PBoolean); stdcall;
Begin
 dest_reg^ := getBool(Stack^[reg_stp^+StackvalPos]);
End;

(* __charreg_stackval_assign *)
Procedure __charreg_stackval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const dest_reg: PChar); stdcall;
Begin
 dest_reg^ := getChar(Stack^[reg_stp^+StackvalPos]);
End;

(* __intreg_stackval_assign *)
Procedure __intreg_stackval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const dest_reg: pint64); stdcall;
Begin
 dest_reg^ := getInt(Stack^[reg_stp^+StackvalPos]);
End;

(* __floatreg_stackval_assign *)
Procedure __floatreg_stackval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const dest_reg: PExtended); stdcall;
Begin
 dest_reg^ := getFloat(Stack^[reg_stp^+StackvalPos]);
End;

(* __stringreg_stackval_assign *)
Procedure __stringreg_stackval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const dest_reg: PPChar); stdcall;
Begin
 dest_reg^ := getString(Stack^[reg_stp^+StackvalPos]);
End;

(* __referencereg_stackval_assign *)
Procedure __referencereg_stackval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const dest_reg: PPointer); stdcall;
Begin
 dest_reg^ := getReference(Stack^[reg_stp^+StackvalPos]);
End;

(* __stackval_boolval_assign *)
Procedure __stackval_boolval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: Boolean); stdcall;
Begin
 Stack^[reg_stp^+StackvalPos].Typ        := mvBool;
 Stack^[reg_stp^+StackvalPos].Value.Bool := Value;
End;

(* __stackval_charval_assign *)
Procedure __stackval_charval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: Char); stdcall;
Begin
 Stack^[reg_stp^+StackvalPos].Typ        := mvChar;
 Stack^[reg_stp^+StackvalPos].Value.Char := Value;
End;

(* __stackval_intval_assign *)
Procedure __stackval_intval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Begin
 Stack^[reg_stp^+StackvalPos].Typ       := mvInt;
 Stack^[reg_stp^+StackvalPos].Value.Int := Value;
End;

(* __stackval_floatval_assign *)
Procedure __stackval_floatval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32); stdcall;
Var Value: Extended;
Begin
 asm
  fstp Value
 end;

 Stack^[reg_stp^+StackvalPos].Typ         := mvFloat;
 Stack^[reg_stp^+StackvalPos].Value.Float := Value;
End;

(* __stackval_stringval_assign *)
Procedure __stackval_stringval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: PChar); stdcall;
Begin
 Stack^[reg_stp^+StackvalPos].Typ       := mvString;
 Stack^[reg_stp^+StackvalPos].Value.Str := Value;
End;

(* __stackval_referenceval_assign *)
Procedure __stackval_referenceval_assign(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: Pointer); stdcall;
Begin
 Stack^[reg_stp^+StackvalPos].Typ       := mvReference;
 Stack^[reg_stp^+StackvalPos].Value.Int := uint32(Value);
End;

(* __intadd_int_int *)
Function __intadd_int_int(const A, B: int64): int64; stdcall;
Begin
 Result := A+B;
End;

(* __intsub_int_int *)
Function __intsub_int_int(const A, B: int64): int64; stdcall;
Begin
 Result := A-B;
End;

(* __intmul_int_int *)
Function __intmul_int_int(const A, B: int64): int64; stdcall;
Begin
 Result := A*B;
End;

(* __intdiv_int_int *)
Function __intdiv_int_int(const A, B: int64): int64; stdcall;
Begin
 Result := A div B;
End;

(* __intshl_int_int *)
Function __intshl_int_int(const A, B: int64): int64; stdcall;
Begin
 Result := A << B;
End;

(* __intshr_int_int *)
Function __intshr_int_int(const A, B: int64): int64; stdcall;
Begin
 Result := A >> B;
End;

(* __create_icall_parameter_list *)
Procedure __create_icall_parameter_list(const Params: PMixedValue; const ParamCount: uint32; const Stack: PStack; const reg_stp: pint32); stdcall;
Var I: Integer;
Begin
 For I := 0 To ParamCount-1 Do
 Begin
  Params[I] := Stack^[reg_stp^];
  Dec(reg_stp^);
 End;
End;

(* __clean_mixedvalue *)
Procedure __clean_mixedvalue(const MVal: PMixedValue); stdcall;
Begin
 MVal^.Typ := mvNone;
End;

(* __apply_mixedvalue *)
Procedure __apply_mixedvalue(const MVal: PMixedValue; const Stack: PStack; const reg_stp: pint32); stdcall;
Begin
 if (MVal^.Typ <> mvNone) Then
 Begin
  Inc(reg_stp^);
  Stack^[reg_stp^] := MVal^;
 End;
End;

(* __stringconcat_reg_string *)
Procedure __stringconcat_reg_string(const RegAddr: uint32; const StrPnt: PChar); stdcall;
Begin
 PPChar(RegAddr)^ := CopyStringToPChar(AnsiString(PPChar(RegAddr)^) + AnsiString(StrPnt));
End;

(* __ret *)
Function __ret(const Stack: PStack; const reg_stp: pint32): uint32; stdcall;
Begin
 Result := uint32(getReference(Stack^[reg_stp^]));
 Dec(reg_stp^);
End;

(* __stackval_add_int *)
Procedure __stackval_add_int(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Var SVal: PStackElement;
Begin
 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt  : SVal^.Value.Int += Value;
  mvFloat: SVal^.Value.Float += Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_add_int'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_sub_int *)
Procedure __stackval_sub_int(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Var SVal: PStackElement;
Begin
 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt  : SVal^.Value.Int -= Value;
  mvFloat: SVal^.Value.Float -= Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_sub_int'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_mul_int *)
Procedure __stackval_mul_int(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Var SVal: PStackElement;
Begin
 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt  : SVal^.Value.Int *= Value;
  mvFloat: SVal^.Value.Float *= Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_mul_int'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_div_int *)
Procedure __stackval_div_int(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Var SVal: PStackElement;
Begin
 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt  : SVal^.Value.Int := SVal^.Value.Int div Value;
  mvFloat: SVal^.Value.Float /= Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_div_int'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_shl_int *)
Procedure __stackval_shl_int(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Var SVal: PStackElement;
Begin
 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt: SVal^.Value.Int := SVal^.Value.Int << Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_shl_int'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_shr_int *)
Procedure __stackval_shr_int(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32; const Value: int64); stdcall;
Var SVal: PStackElement;
Begin
 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt: SVal^.Value.Int := SVal^.Value.Int >> Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_shr_int'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_add_float *)
Procedure __stackval_add_float(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32); stdcall;
Var SVal : PStackElement;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt:
  Begin
   SVal^.Typ         := mvFloat;
   SVal^.Value.Float := SVal^.Value.Int+Value;
  End;

  mvFloat: SVal^.Value.Float += Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_add_float'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_sub_float *)
Procedure __stackval_sub_float(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32); stdcall;
Var SVal : PStackElement;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt:
  Begin
   SVal^.Typ         := mvFloat;
   SVal^.Value.Float := SVal^.Value.Int-Value;
  End;

  mvFloat: SVal^.Value.Float -= Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_sub_float'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_mul_float *)
Procedure __stackval_mul_float(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32); stdcall;
Var SVal : PStackElement;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt:
  Begin
   SVal^.Typ         := mvFloat;
   SVal^.Value.Float := SVal^.Value.Int*Value;
  End;

  mvFloat: SVal^.Value.Float *= Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_mul_float'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_div_float *)
Procedure __stackval_div_float(const Stack: PStack; const reg_stp: pint32; const StackvalPos: int32); stdcall;
Var SVal : PStackElement;
    Value: Extended;
Begin
 asm
  fstp Extended Value
 end;

 SVal := @Stack^[reg_stp^+StackvalPos];

 Case SVal^.Typ of
  mvInt:
  Begin
   SVal^.Typ         := mvFloat;
   SVal^.Value.Float := SVal^.Value.Int/Value;
  End;

  mvFloat: SVal^.Value.Float /= Value;
  else
   raise Exception.CreateFmt('Cannot execute ''__stackval_div_float'' on type `%d`', [ord(SVal^.Typ)]);
 End;
End;

(* __stackval_opcode_stackval *)
Procedure __stackval_opcode_stackval(const Stack: PStack; const reg_stp: pint32; const Stackval1Pos, Stackval2Pos: int32; const Opcode: TOpcode_E); stdcall;
Var S1, S2, Result: TMixedValue;
    FVal1, FVal2  : Extended;
    IVal1, IVal2  : Int64;
Begin
 S1 := Stack^[reg_stp^+Stackval1Pos];
 S2 := Stack^[reg_stp^+Stackval2Pos];

 // int, int
 if (S1.Typ = mvInt) and (S2.Typ = mvInt) Then
 Begin
  IVal1      := S1.Value.Int;
  IVal2      := S2.Value.Int;
  Result.Typ := mvInt;
 End Else

 // int, float
 if (S1.Typ = mvInt) and (S2.Typ = mvFloat) Then
 Begin
  FVal1      := S1.Value.Int;
  FVal2      := S2.Value.Float;
  Result.Typ := mvFloat;
 End Else

 // float, int
 if (S1.Typ = mvFloat) and (S2.Typ = mvInt) Then
 Begin
  FVal1      := S1.Value.Float;
  FVal2      := S2.Value.Int;
  Result.Typ := mvFloat;
 End Else

 // float, float
 if (S1.Typ = mvFloat) and (S2.Typ = mvFloat) Then
 Begin
  FVal1      := S1.Value.Float;
  FVal2      := S2.Value.Float;
  Result.Typ := mvFloat;
 End Else

 // invalid call
  raise Exception.CreateFmt('''__stackval_opcode_stackval'' called with invalid args: %d, %d', [ord(S1.Typ), ord(S2.Typ)]);

 if (Result.Typ = mvFloat) Then
 Begin
  Case Opcode of
   o_add: Result.Value.Float := FVal1 + FVal2;
   o_sub: Result.Value.Float := FVal1 - FVal2;
   o_mul: Result.Value.Float := FVal1 * FVal2;
   o_div: Result.Value.Float := FVal1 / FVal2;

   else
    raise Exception.CreateFmt('''__stackval_opcode_stackval'' called with invalid opcode: %d', [ord(Opcode)]);
  End;
 End Else
 Begin
  Case Opcode of
   o_add: Result.Value.Int := IVal1 - IVal2;
   o_sub: Result.Value.Int := IVal1 - IVal2;
   o_mul: Result.Value.Int := IVal1 - IVal2;
   o_div: Result.Value.Int := IVal1 - IVal2;

   else
    raise Exception.CreateFmt('''__stackval_opcode_stackval'' called with invalid opcode: %d', [ord(Opcode)]);
  End;
 End;

 Stack^[reg_stp^+Stackval1Pos] := Result;
End;

(* __compare_stackval_stackval *)
Procedure __compare_stackval_stackval(const Stack: PStack; const reg_stp: pint32; const reg_if: PBoolean; const Stackval1Pos, Stackval2Pos: int32; const Opcode: TOpcode_E); stdcall;
Var S1, S2: TMixedValue;
Begin
 S1 := Stack^[reg_stp^+Stackval1Pos];
 S2 := Stack^[reg_stp^+Stackval2Pos];

 Case Opcode of
  o_if_le: reg_if^ := (S1 <= S2);
  o_if_l : reg_if^ := (S1 < S2);
  o_if_e : reg_if^ := (S1 = S2);
  o_if_ne: reg_if^ := (S1 <> S2);
  o_if_g : reg_if^ := (S1 > S2);
  o_if_ge: reg_if^ := (S1 >= S2);
 End;
End;

(* __compare_stackval_value *)
Procedure __compare_stackval_value(const Stack: PStack; const reg_stp: pint32; const reg_if: PBoolean; const Opcode: TOpcode_E; const Stackval1Pos: int32; const ValueType: TOpcodeArgType; const Value: Pointer); stdcall;
Var S1, S2: TMixedValue;
Begin
 S1 := Stack^[reg_stp^+Stackval1Pos];
 S2 := ConstructMixedValue(ValueType, @Value);

 Case Opcode of
  o_if_le: reg_if^ := (S1 <= S2);
  o_if_l : reg_if^ := (S1 < S2);
  o_if_e : reg_if^ := (S1 = S2);
  o_if_ne: reg_if^ := (S1 <> S2);
  o_if_g : reg_if^ := (S1 > S2);
  o_if_ge: reg_if^ := (S1 >= S2);
 End;
End;

(* __compare_value_stackval *)
Procedure __compare_value_stackval(const Stack: PStack; const reg_stp: pint32; const reg_if: PBoolean; const Opcode: TOpcode_E; const Stackval2Pos: int32; const ValueType: TOpcodeArgType; const Value: Pointer); stdcall;
Var S1, S2: TMixedValue;
Begin
 S1 := ConstructMixedValue(ValueType, @Value);
 S2 := Stack^[reg_stp^+Stackval2Pos];

 Case Opcode of
  o_if_le: reg_if^ := (S1 <= S2);
  o_if_l : reg_if^ := (S1 < S2);
  o_if_e : reg_if^ := (S1 = S2);
  o_if_ne: reg_if^ := (S1 <> S2);
  o_if_g : reg_if^ := (S1 > S2);
  o_if_ge: reg_if^ := (S1 >= S2);
 End;
End;

(* __compare_string_string *)
Procedure __compare_string_string(const reg_if: PBoolean; const Opcode: TOpcode_E; const Str1, Str2: PChar); stdcall;
Var S1, S2: String;
Begin
 S1 := Str1;
 S2 := Str2;

 Case Opcode of
  o_if_le: reg_if^ := (S1 <= S2);
  o_if_l : reg_if^ := (S1 < S2);
  o_if_e : reg_if^ := (S1 = S2);
  o_if_ne: reg_if^ := (S1 <> S2);
  o_if_g : reg_if^ := (S1 > S2);
  o_if_ge: reg_if^ := (S1 >= S2);
 End;
End;

(* __release_memory *)
Procedure __release_memory(const Pnt: Pointer); stdcall;
Begin
 FreeMem(Pnt);
End;
