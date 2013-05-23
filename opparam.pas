(* TOpParam.getBool *)
Function TOpParam.getBool: Boolean;
Begin
 Case Typ of
  { bool, int }
  ptBool, ptBoolReg, ptInt, ptIntReg:
   if (Value.Int = 0) Then
    Exit(False) { `0` is `false` } Else
    Exit(True); { anything else if `true` }

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getBool);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> bool');
End;

(* TOpParam.getChar *)
Function TOpParam.getChar: Char;
Begin
 Case Typ of
  { char, int }
  ptChar, ptCharReg, ptInt, ptIntReg: Exit(chr(Value.Int));

  { string }
  ptString, ptStringReg: Exit(Value.Str[1]);

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getChar);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> char');
End;

(* TOpParam.getInt *)
Function TOpParam.getInt: Int64;
Begin
 Case Typ of
  { bool, char, int, reference }
  ptBool, ptBoolReg, ptChar, ptCharReg, ptInt, ptIntReg, ptReferenceReg: Exit(Value.Int);

  { float }
  ptFloat, ptFloatReg: Exit(Round(Value.Float)); // @TODO: inf, nan?

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getInt);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int');
End;

(* TOpParam.getLongword *)
Function TOpParam.getLongword: LongWord;
Begin
 Case Typ of
  { int, reference }
  ptInt, ptIntReg, ptReferenceReg: Exit(Value.Int);

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getInt);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int (longword)');
End;

(* TOpParam.getFloat *)
Function TOpParam.getFloat: Extended;
Begin
 Case Typ of
  { float }
  ptFloat, ptFloatReg: Exit(Value.Float);

  { int, bool }
  ptInt, ptIntReg, ptBool, ptBoolReg: Exit(Value.Int);

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getFloat);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> float');
End;

(* TOpParam.getString *)
Function TOpParam.getString: String;
Begin
 Case Typ of
  { string }
  ptString, ptStringReg: Exit(Value.Str);

  { char }
  ptChar, ptCharReg: Exit(chr(Value.Int));

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getString);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> string');
End;

(* TOpParam.getReference *)
Function TOpParam.getReference: LongWord;
Begin
 Case Typ of
  { int, reference, callstack reference }
  ptInt, ptIntReg, ptReferenceReg, ptCallstackRef: Exit(Value.Int);

  { stackval }
  ptStackval: Exit(M.Stack[M.StackPos^+Value.Int].getReference);
 End;

 raise eInvalidCasting.Create('Invalid casting: '+PrimaryTypeNames[Typ]+' -> int');
End;

{ TOpParam.getTypeName }
Function TOpParam.getTypeName: String;
Begin
 Result := PrimaryTypeNames[Typ];

 if (Typ = ptStackVal) Then
  Result += ' ('+M.Stack[M.StackPos^+Value.Int].getTypeName+')';
End;
