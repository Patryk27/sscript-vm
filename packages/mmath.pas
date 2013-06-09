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

{ math.sqrt }
Procedure _sqrt(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^.Typ         := mvFloat;
 Result^.Value.Float := sqrt(getFloat(Params[0]));
End;

{ math.sin }
Procedure _sin(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^.Typ         := mvFloat;
 Result^.Value.Float := sin(getFloat(Params[0]));
End;

{ math.cos }
Procedure _cos(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^.Typ         := mvFloat;
 Result^.Value.Float := cos(getFloat(Params[0]));
End;

{ math.log }
Procedure _log(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^.Typ         := mvFloat;
 Result^.Value.Float := logn(getFloat(Params[0]), getFloat(Params[1]));
End;

{ math.ln }
Procedure _ln(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^.Typ         := mvFloat;
 Result^.Value.Float := ln(getFloat(Params[0]));
End;

{ math.exp }
Procedure _exp(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^.Typ         := mvFloat;
 Result^.Value.Float := exp(getFloat(Params[0]));
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 AddInternalCall(VM, 'math', 'sqrt', 1, @_sqrt);
 AddInternalCall(VM, 'math', 'sin', 1, @_sin);
 AddInternalCall(VM, 'math', 'cos', 1, @_cos);
 AddInternalCall(VM, 'math', 'log', 2, @_log);
 AddInternalCall(VM, 'math', 'ln', 1, @_ln);
 AddInternalCall(VM, 'math', 'exp', 1, @_exp);
End;
End.
