(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 Part of the JIT compiler for x86 architecture.
*)
{$ASMMODE INTEL}

(* r__push_bool *)
Procedure r__push_bool(const VM: PVM; const Value: Boolean); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ        := mvBool;
 MV.Value.Bool := Value;
 VM^.StackPush(MV);
End;

(* r__push_int *)
Procedure r__push_int(const VM: PVM; const Value_lo, Value_hi: int32); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ       := mvint;
 MV.Value.Int := (int64(Value_hi) << 32) + Value_lo;
 VM^.StackPush(MV);
End;

(* r__push_float_mem *)
Procedure r__push_float_mem(const VM: PVM; const Addr: PExtended); register;
Var MV: TMixedValue;
Begin
 MV.Reset;
 MV.Typ         := mvFloat;
 MV.Value.Float := Addr^;
 VM^.StackPush(MV);
End;

(* r__pop_int_reg *)
Procedure r__pop_int_reg(const VM: PVM; const RegAddr: pint64); register;
Begin
 RegAddr^ := getInt(VM^.Stack[VM^.Regs.i[5]]);
 Dec(VM^.Regs.i[5]);
End;

(* r__create_icall_parameter_list *)
Procedure r__create_icall_parameter_list(const VM: PVM; const icall: PCall; const ParamsMV: PMixedValue); register;
Var I: int8;
Begin
 For I := 0 To int8(icall^.ParamCount)-1 Do
 Begin
  ParamsMV[I] := VM^.Stack[VM^.Regs.i[5]];
  Dec(VM^.Regs.i[5]);
 End;
End;

(* r__clean_mixedvalue *)
Procedure r__clean_mixedvalue(const MV: PMixedValue); register;
Begin
 MV^.Reset;
End;

(* r__apply_mixedvalue *)
Procedure r__apply_mixedvalue(const VM: PVM; const MV: PMixedValue); register;
Begin
 if (MV^.Typ <> mvNone) Then
  VM^.StackPush(MV^);
End;

(* r__div_memint_immint *)
Procedure r__div_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ div (int64(Value_hi) << 32 + Value_lo);
End;

(* r__shl_memint_immint *)
Procedure r__shl_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ << (int64(Value_hi) << 32 + Value_lo);
End;

(* r__shr_memint_immint *)
Procedure r__shr_memint_immint(const Pnt: pint64; const Value_lo, Value_hi: int32); register;
Begin
 Pnt^ := Pnt^ >> (int64(Value_hi) << 32 + Value_lo);
End;
