(*
 Copyright © by Patryk Wychowaniec, 2013
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
Procedure _cos(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := cos(getFloat(Params[0]));
End;

{ math.sin }
Procedure _sin(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := sin(getFloat(Params[0]));
End;

{ math.tan }
Procedure _tan(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := tan(getFloat(Params[0]));
End;

{ math.acos }
Procedure _acos(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arccos(getFloat(Params[0]));
End;

{ math.asin }
Procedure _asin(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arcsin(getFloat(Params[0]));
End;

{ math.atan }
Procedure _atan(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arctan(getFloat(Params[0]));
End;

{ math.atan2 }
Procedure _atan2(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arctan2(getFloat(Params[0]), getFloat(Params[1]));
End;

{ math.cosh }
Procedure _cosh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := cosh(getFloat(Params[0]));
End;

{ math.sinh }
Procedure _sinh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := sinh(getFloat(Params[0]));
End;

{ math.tanh }
Procedure _tanh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := tanh(getFloat(Params[0]));
End;

{ math.acosh }
Procedure _acosh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arccosh(getFloat(Params[0]));
End;

{ math.asinh }
Procedure _asinh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arcsinh(getFloat(Params[0]));
End;

{ math.atanh }
Procedure _atanh(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := arctanh(getFloat(Params[0]));
End;

{ math.exp }
Procedure _exp(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := exp(getFloat(Params[0]));
End;

{ math.log }
Procedure _log(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := logn(getFloat(Params[0]), getFloat(Params[1]));
End;

{ math.ln }
Procedure _ln(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := ln(getFloat(Params[0]));
End;

{ math.sqrt }
Procedure _sqrt(VM: Pointer; Params: PMixedValue; Result: PMixedValue); stdcall;
Begin
 Result^ := sqrt(getFloat(Params[0]));
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 AddInternalCall(VM, 'math', 'cos', 1, @_cos);
 AddInternalCall(VM, 'math', 'sin', 1, @_sin);
 AddInternalCall(VM, 'math', 'tan', 1, @_tan);

 AddInternalCall(VM, 'math', 'acos', 1, @_acos);
 AddInternalCall(VM, 'math', 'asin', 1, @_asin);
 AddInternalCall(VM, 'math', 'atan', 1, @_atan);
 AddInternalCall(VM, 'math', 'atan2', 2, @_atan2);

 AddInternalCall(VM, 'math', 'cosh', 1, @_cosh);
 AddInternalCall(VM, 'math', 'sinh', 1, @_sinh);
 AddInternalCall(VM, 'math', 'tanh', 1, @_tanh);

 AddInternalCall(VM, 'math', 'acosh', 1, @_acosh);
 AddInternalCall(VM, 'math', 'asinh', 1, @_asinh);
 AddInternalCall(VM, 'math', 'atanh', 1, @_atanh);

 AddInternalCall(VM, 'math', 'exp', 1, @_exp);
 AddInternalCall(VM, 'math', 'log', 2, @_log);
 AddInternalCall(VM, 'math', 'ln', 1, @_ln);
 AddInternalCall(VM, 'math', 'sqrt', 1, @_sqrt);
End;
End.
