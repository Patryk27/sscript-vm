(* TMachine.StackPush (boolean) *)
Procedure TMachine.StackPush(Value: Boolean);
Var E: TOpParam;
Begin
 E.Typ       := ptBool;
 E.Value.Int := ord(Value);
 StackPush(E);
End;

(* TMachine.StackPush (char) *)
Procedure TMachine.StackPush(Value: Char);
Var E: TOpParam;
Begin
 E.Typ       := ptChar;
 E.Value.Int := ord(Value);
 StackPush(E);
End;

(* TMachine.StackPush (Int64) *)
Procedure TMachine.StackPush(Value: Int64);
Var E: TOpParam;
Begin
 E.Typ       := ptInt;
 E.Value.Int := Value;
 StackPush(E);
End;

(* TMachine.StackPush (Extended) *)
Procedure TMachine.StackPush(Value: Extended);
Var E: TOpParam;
Begin
 E.Typ         := ptFloat;
 E.Value.Float := Value;
 StackPush(E);
End;

(* TMachine.StackPush (String) *)
Procedure TMachine.StackPush(Value: String);
Var E: TOpParam;
Begin
 E.Typ       := ptString;
 E.Value.Str := Value;
 StackPush(E);
End;

(* TMachine.StackPush (TOpParam) *)
Procedure TMachine.StackPush(Value: TOpParam);
Begin
 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''push'' - no space left on the stack.');

 Inc(StackPos^);
 Stack[StackPos^] := Value;
End;

(* TMachine.StackPop *)
Function TMachine.StackPop: TOpParam;
Begin
 if (StackPos^ = 0) Then
  raise eInternalError.Create('Cannot ''pop'' - stack is empty.');

 if (StackPos^ >= STACK_SIZE) Then
  raise eInternalError.Create('Cannot ''pop'' - pointer is outside of the stack.');

 Result := Stack[StackPos^];
 Dec(StackPos^);
End;

{ TMachine.CallstackPush }
Procedure TMachine.CallstackPush(Value: LongWord);
Var E: TOpParam;
Begin
 E.Typ       := ptCallstackRef;
 E.Value.Int := Value;
 StackPush(E);
End;

(* TMachine.CallstackPop *)
Function TMachine.CallstackPop: LongWord;
Begin
 Exit(StackPop.getReference);
End;
