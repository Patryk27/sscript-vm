(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mMath;

 Interface
 Uses Machine;

 Procedure Init(M: TMachine);

 Implementation
Uses Math;

{ math.sqrt }
Procedure _sqrt(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpFloat;
 Result.Value := sqrt(Params[0].Value);
End;

{ math.sin }
Procedure _sin(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpFloat;
 Result.Value := sin(Params[0].Value);
End;

{ math.cos }
Procedure _cos(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpFloat;
 Result.Value := cos(Params[0].Value);
End;

{ math.log }
Procedure _log(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpFloat;
 Result.Value := logn(Params[0].Value, Params[1].Value);
End;

{ math.ln }
Procedure _ln(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpFloat;
 Result.Value := ln(Params[0].Value);
End;

{ math.exp }
Procedure _exp(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpFloat;
 Result.Value := exp(Params[0].Value);
End;

// -------------------------------------------------------------------------- //
Procedure Init(M: TMachine);
Begin
 M.AddInternalCall('math', 'sqrt', 1, @_sqrt);
 M.AddInternalCall('math', 'sin', 1, @_sin);
 M.AddInternalCall('math', 'cos', 1, @_cos);
 M.AddInternalCall('math', 'log', 2, @_log);
 M.AddInternalCall('math', 'ln', 1, @_ln);
 M.AddInternalCall('math', 'exp', 1, @_exp);
End;
End.
