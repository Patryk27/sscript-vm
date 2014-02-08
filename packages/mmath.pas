(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit mMath;

 Interface
 Uses vm_header;

 Procedure Init(VM: Pointer);

 Implementation
Uses Math;

{ math.cos }
Procedure _cos(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := cos(SSGetFloat(Params[0]));
End;

{ math.sin }
Procedure _sin(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := sin(SSGetFloat(Params[0]));
End;

{ math.tan }
Procedure _tan(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := tan(SSGetFloat(Params[0]));
End;

{ math.acos }
Procedure _acos(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arccos(SSGetFloat(Params[0]));
End;

{ math.asin }
Procedure _asin(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arcsin(SSGetFloat(Params[0]));
End;

{ math.atan }
Procedure _atan(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arctan(SSGetFloat(Params[0]));
End;

{ math.atan2 }
Procedure _atan2(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arctan2(SSGetFloat(Params[0]), SSGetFloat(Params[1]));
End;

{ math.cosh }
Procedure _cosh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := cosh(SSGetFloat(Params[0]));
End;

{ math.sinh }
Procedure _sinh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := sinh(SSGetFloat(Params[0]));
End;

{ math.tanh }
Procedure _tanh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := tanh(SSGetFloat(Params[0]));
End;

{ math.acosh }
Procedure _acosh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arccosh(SSGetFloat(Params[0]));
End;

{ math.asinh }
Procedure _asinh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arcsinh(SSGetFloat(Params[0]));
End;

{ math.atanh }
Procedure _atanh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := arctanh(SSGetFloat(Params[0]));
End;

{ math.exp }
Procedure _exp(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := exp(SSGetFloat(Params[0]));
End;

{ math.log }
Procedure _log(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := logn(SSGetFloat(Params[0]), SSGetFloat(Params[1]));
End;

{ math.ln }
Procedure _ln(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := ln(SSGetFloat(Params[0]));
End;

{ math.sqrt }
Procedure _sqrt(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := sqrt(SSGetFloat(Params[0]));
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 SSAddInternalCall(VM, 'math', 'cos', 1, @_cos);
 SSAddInternalCall(VM, 'math', 'sin', 1, @_sin);
 SSAddInternalCall(VM, 'math', 'tan', 1, @_tan);

 SSAddInternalCall(VM, 'math', 'acos', 1, @_acos);
 SSAddInternalCall(VM, 'math', 'asin', 1, @_asin);
 SSAddInternalCall(VM, 'math', 'atan', 1, @_atan);
 SSAddInternalCall(VM, 'math', 'atan2', 2, @_atan2);

 SSAddInternalCall(VM, 'math', 'cosh', 1, @_cosh);
 SSAddInternalCall(VM, 'math', 'sinh', 1, @_sinh);
 SSAddInternalCall(VM, 'math', 'tanh', 1, @_tanh);

 SSAddInternalCall(VM, 'math', 'acosh', 1, @_acosh);
 SSAddInternalCall(VM, 'math', 'asinh', 1, @_asinh);
 SSAddInternalCall(VM, 'math', 'atanh', 1, @_atanh);

 SSAddInternalCall(VM, 'math', 'exp', 1, @_exp);
 SSAddInternalCall(VM, 'math', 'log', 2, @_log);
 SSAddInternalCall(VM, 'math', 'ln', 1, @_ln);
 SSAddInternalCall(VM, 'math', 'sqrt', 1, @_sqrt);
End;
End.
