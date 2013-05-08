(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mMath;

 Interface

 Implementation
Uses Machine, Math;

{ math.sqrt }
Procedure _sqrt(M: TMachine);
Begin
 M.StackPush(System.sqrt(M.StackPop.getFloat));
End;

{ math.sin }
Procedure _sin(M: TMachine);
Begin
 M.StackPush(System.sin(M.StackPop.getFloat));
End;

{ math.cos }
Procedure _cos(M: TMachine);
Begin
 M.StackPush(System.cos(M.StackPop.getFloat));
End;

{ math.log }
Procedure _log(M: TMachine);
Begin
 M.StackPush(Math.logn(M.StackPop.getFloat, M.StackPop.getFloat));
End;

{ math.ln }
Procedure _ln(M: TMachine);
Begin
 M.StackPush(System.ln(M.StackPop.getFloat));
End;

{ math.exp }
Procedure _exp(M: TMachine);
Begin
 M.StackPush(System.exp(M.StackPop.getFloat));
End;

initialization
 Add_icall('math', 'sqrt', @_sqrt);
 Add_icall('math', 'sin', @_sin);
 Add_icall('math', 'cos', @_cos);
 Add_icall('math', 'log', @_log);
 Add_icall('math', 'ln', @_ln);
 Add_icall('math', 'exp', @_exp);
End.
