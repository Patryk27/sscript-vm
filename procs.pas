(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Procs;

 Interface
 Uses SysUtils, Machine, Exceptions, Opcodes;

 Procedure op_(const M: TMachine);

 Procedure op_NOP(const M: TMachine);
 Procedure op_STOP(const M: TMachine);
 Procedure op_PUSH(const M: TMachine);
 Procedure op_POP(const M: TMachine);
 Procedure op_ADD(const M: TMachine);
 Procedure op_SUB(const M: TMachine);
 Procedure op_MUL(const M: TMachine);
 Procedure op_DIV(const M: TMachine);
 Procedure op_NEG(const M: TMachine);
 Procedure op_MOV(const M: TMachine);
 Procedure op_JMP(const M: TMachine);
 Procedure op_TJMP(const M: TMachine);
 Procedure op_FJMP(const M: TMachine);
 Procedure op_CALL(const M: TMachine);
 Procedure op_ICALL(const M: TMachine);
 Procedure op_ACALL(const M: TMachine);
 Procedure op_RET(const M: TMachine);
 Procedure op_IF_E(const M: TMachine);
 Procedure op_IF_NE(const M: TMachine);
 Procedure op_IF_G(const M: TMachine);
 Procedure op_IF_L(const M: TMachine);
 Procedure op_IF_GE(const M: TMachine);
 Procedure op_IF_LE(const M: TMachine);
 Procedure op_STRJOIN(const M: TMachine);
 Procedure op_NOT(const M: TMachine);
 Procedure op_OR(const M: TMachine);
 Procedure op_XOR(const M: TMachine);
 Procedure op_AND(const M: TMachine);
 Procedure op_SHL(const M: TMachine);
 Procedure op_SHR(const M: TMachine);
 Procedure op_MOD(const M: TMachine);
 Procedure op_ARSET(const M: TMachine);
 Procedure op_ARGET(const M: TMachine);
 Procedure op_ARCRT(const M: TMachine);
 Procedure op_ARLEN(const M: TMachine);
 Procedure op_OBJFREE(const M: TMachine);
 Procedure op_LOCATION(const M: TMachine);

 Type TOpcodeProc = Procedure(const M: TMachine);
 Const OpcodeTable: Array[TOpcode_E] of TOpcodeProc = // opcode list
 (
  @op_NOP,
  @op_STOP,
  @op_PUSH,
  @op_POP,
  @op_ADD,
  @op_SUB,
  @op_MUL,
  @op_DIV,
  @op_NEG,
  @op_MOV,
  @op_JMP,
  @op_TJMP,
  @op_FJMP,
  @op_CALL,
  @op_ICALL,
  @op_ACALL,
  @op_RET,
  @op_IF_E,
  @op_IF_NE,
  @op_IF_G,
  @op_IF_L,
  @op_IF_GE,
  @op_IF_LE,
  @op_STRJOIN,
  @op_NOT,
  @op_OR,
  @op_XOR,
  @op_AND,
  @op_SHL,
  @op_SHR,
  @op_MOD,
  @op_ARSET,
  @op_ARGET,
  @op_ARCRT,
  @op_ARLEN,
  @op_OBJFREE,
  @op_LOCATION,
  @op_LOCATION,
  @op_LOCATION
 );

 Implementation
Uses Objects;

{ CheckStringBounds }
Procedure CheckStringBounds(const M: TMachine; const Str: String; const Index: Integer);
Begin
 if (Index < 1) Then
  M.ThrowException('String out of bounds. Tried to access char #'+IntToStr(Index)+', while string starts at 1.');

 if (Index > Length(Str)) Then
  M.ThrowException('String out of bounds. Tried to access char #'+IntToStr(Index)+', while '+IntToStr(Length(Str))+' is the last one.');
End;

{ qmagic }
Function qmagic(Param: TOpParam): TOpParam; inline;
Begin
 With Result do
 Begin
  Case Param.Typ of
   ptBoolReg     : Typ := ptBool;
   ptCharReg     : Typ := ptChar;
   ptIntReg      : Typ := ptInt;
   ptFloatReg    : Typ := ptFloat;
   ptStringReg   : Typ := ptString;
   ptReferenceReg: Typ := ptInt;

   else
    Typ := Param.Typ;
  End;

  M     := Param.M;
  Value := Param.Value;
  Index := Param.Index;
 End;

 if (Param.Typ = ptStackVal) Then
  With Param.M do
   Result := Stack[StackPos^+Param.Value.Int];
End;

{ = }
Operator = (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) { float = int } Then
  Exit(P1.Value.Float = P2.Value.Int);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) { int = float } Then
  Exit(P1.Value.Int = P2.Value.Float);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Value.Int = P2.Value.Int);
  ptFloat: Exit(P1.Value.Float = P2.Value.Float);
 End;
End;

{ <> }
Operator <> (P1, P2: TOpParam): Boolean;
Begin
 Result := not (P1 = P2);
End;

{ > }
Operator > (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) { float > int } Then
  Exit(P1.Value.Float > P2.Value.Int);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) { int > float } Then
  Exit(P1.Value.Int > P2.Value.Float);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Value.Int > P2.Value.Int);
  ptFloat: Exit(P1.Value.Float > P2.Value.Float);
 End;
End;

{ < }
Operator < (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) { float < int } Then
  Exit(P1.Value.Float < P2.Value.Int);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) { int < float } Then
  Exit(P1.Value.Int < P2.Value.Float);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Value.Int < P2.Value.Int);
  ptFloat: Exit(P1.Value.Float < P2.Value.Float);
 End;
End;

{ >= }
Operator >= (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) { float >= int } Then
  Exit(P1.Value.Float >= P2.Value.Int);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) { int >= float } Then
  Exit(P1.Value.Int >= P2.Value.Float);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Value.Int >= P2.Value.Int);
  ptFloat: Exit(P1.Value.Float >= P2.Value.Float);
 End;
End;

{ <= }
Operator <= (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) { float <= int } Then
  Exit(P1.Value.Float <= P2.Value.Int);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) { int <= float } Then
  Exit(P1.Value.Int <= P2.Value.Float);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Value.Int <= P2.Value.Int);
  ptFloat: Exit(P1.Value.Float <= P2.Value.Float);
 End;
End;

{ _ }
Procedure op_(const M: TMachine); // an unimplemented opcode
Begin
 raise eInvalidOpcode.Create('Opcode '''+getOpcodeName(PByte(LongWord(M.Position)-sizeof(Byte))^)+''' unimplemented');
End;

{ NOP() }
Procedure op_NOP(const M: TMachine);
Begin
End;

{ STOP() }
Procedure op_STOP(const M: TMachine);
Begin
 raise Exception.Create('');
End;

{ PUSH (value) }
Procedure op_PUSH(const M: TMachine);
Var P: TOpParam;
Begin
 With M do
 Begin
  P := qmagic(read_param);
  Inc(StackPos^);
  Stack[StackPos^] := P;
 End;
End;

{ POP (register) }
Procedure op_POP(const M: TMachine);
Var reg, val: TOpParam;
Begin
 With M do
 Begin
  reg := read_param;
  val := StackPop;

  Case reg.Typ of
   ptBoolReg: breg[reg.Index] := val.getBool;
   ptCharReg: creg[reg.Index] := val.getChar;
   ptIntReg: ireg[reg.Index] := val.getInt;
   ptFloatReg: freg[reg.Index] := val.getFloat;
   ptStringReg: sreg[reg.Index] := val.getString;
   ptReferenceReg: rreg[reg.Index] := val.getReference;
  // ptStackval: Stack[StackPos^+reg.Value.Int] := val;
   else
    raise eInvalidOpcode.Create('''pop'' called with arguments: '+reg.getTypeName+' <- '+val.getTypeName);
  End;
 End;
End;

{ ADD (register, value) }
Procedure op_ADD(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptCharReg : creg[reg.Index] := chr(ord(creg[reg.Index])+param.getInt); { char }
   ptIntReg  : ireg[reg.Index] += param.getInt; { int }
   ptFloatReg: freg[reg.Index] += param.getFloat; { float }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptChar, ptInt: Value.Int   += param.getInt;
      ptFloat      : Value.Float += param.getFloat;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''add'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ SUB (register, value) }
Procedure op_SUB(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptCharReg : creg[reg.Index] := chr(ord(creg[reg.Index])-param.getInt); { char }
   ptIntReg  : ireg[reg.Index] -= param.getInt; { int }
   ptFloatReg: freg[reg.Index] -= param.getFloat; { float }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptChar, ptInt: Value.Int   -= param.getInt;
      ptFloat      : Value.Float -= param.getFloat;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''sub'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ MUL (register, value) }
Procedure op_MUL(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptCharReg : creg[reg.Index] := chr(ord(creg[reg.Index])*param.getInt); { char }
   ptIntReg  : ireg[reg.Index] *= param.getInt; { int }
   ptFloatReg: freg[reg.Index] *= param.getFloat; { float }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptChar, ptInt: Value.Int   *= param.getInt;
      ptFloat      : Value.Float *= param.getFloat;
      else goto Fail;
     End;
   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''mul'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ DIV (register, value) }
Procedure op_DIV(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  if (param.getFloat = 0) Then // div by zero
   ThrowException('Division by zero.');

  Case reg.Typ of
   ptCharReg : creg[reg.Index] := chr(ord(creg[reg.Index]) div param.getInt); { char }
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] div param.getInt; { int }
   ptFloatReg: freg[reg.Index] /= param.getFloat; { float }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptChar, ptInt: Value.Int   := Value.Int div param.getInt;
      ptFloat      : Value.Float /= param.getFloat;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''div'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ NEG (register) }
Procedure op_NEG(const M: TMachine);
Var reg: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := -ireg[reg.Index]; { int }
   ptFloatReg: freg[reg.Index] := -freg[reg.Index]; { float }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt  : Value.Int   := -Value.Int;
      ptFloat: Value.Float := -Value.Float;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''neg'' called with argument: '+reg.getTypeName);
 End;
End;

{ MOV (register, value) }
Procedure op_MOV(const M: TMachine);
Var reg, val: TOpParam;
Begin
 With M do
 Begin
  reg := read_param;
  val := read_param;

  Case reg.Typ of
   ptBoolReg     : breg[reg.Index] := val.getBool; { bool }
   ptCharReg     : creg[reg.Index] := val.getChar; { char }
   ptIntReg      : ireg[reg.Index] := val.getInt; { int }
   ptFloatReg    : freg[reg.Index] := val.getFloat; { float }
   ptStringReg   : sreg[reg.Index] := val.getString; { string }
   ptReferenceReg: rreg[reg.Index] := val.getReference; { reference }
   ptStackVal    : Stack[StackPos^+reg.Value.Int] := qmagic(val); { stackval}

   else raise eInvalidOpcode.Create('''mov'' called with arguments: '+reg.getTypeName+', '+val.getTypeName);
  End;
 End;
End;

{ JMP (int) }
Procedure op_JMP(const M: TMachine);
Var NewAddr: LongWord;
Begin
 With M do
 Begin
  NewAddr := getPosition;
  NewAddr += read_param.getInt-1;
  setPosition(NewAddr);
 End;
End;

{ TJMP (int) }
Procedure op_TJMP(const M: TMachine);
Var NewAddr: LongWord;
Begin
 With M do
 Begin
  NewAddr := getPosition;
  NewAddr += read_param.getInt-1;
  if (breg[5]) Then
   setPosition(NewAddr);
 End;
End;

{ FJMP (int) }
Procedure op_FJMP(const M: TMachine);
Var NewAddr: LongWord;
Begin
 With M do
 Begin
  NewAddr := getPosition;
  NewAddr += read_param.getInt-1;
  if (not breg[5]) Then
   setPosition(NewAddr);
 End;
End;

{ CALL (int) }
Procedure op_CALL(const M: TMachine);
Var NewAddr: LongWord;
Begin
With M do
Begin
 NewAddr := getPosition;
 NewAddr += read_param.getInt-1;
 CallstackPush(getPosition);
 setPosition(NewAddr);
End;
End;

{ ICALL (string) }
Procedure op_ICALL(const M: TMachine);
Var Name  : String;
    call  : PCall;
    I     : Integer;
    Param : TOpParam;
    Params: TCallValues;
    Result: TCallValue;
Begin
 With M do
 Begin
  Name := read_param.getString;
  For call in icall Do
   if (call^.Full = Name) Then
   Begin
    SetLength(Params, call^.ParamCount);
    For I := High(Params) Downto 0 Do
    Begin
     Param := StackPop;

     Case Param.Typ of
      { bool }
      ptBool:
      Begin
       Params[I].Typ   := cpBool;
       Params[I].Value := Param.getBool;
      End;

      { char }
      ptChar:
      Begin
       Params[I].Typ   := cpInt;
       Params[I].Value := Param.getChar;
      End;

      { int }
      ptInt:
      Begin
       Params[I].Typ   := cpInt;
       Params[I].Value := Param.getInt;
      End;

      { float }
      ptFloat:
      Begin
       Params[I].Typ   := cpFloat;
       Params[I].Value := Param.getFloat;
      End;

      { string }
      ptString:
      Begin
       Params[I].Typ   := cpString;
       Params[I].Value := Param.getString;
      End;
     End;
    End;

    Result.Typ   := cpNone;
    Result.Value := null;
    call^.Handler(M, Params, Result);

    Case Result.Typ of // any result?
     cpBool  : StackPush(Boolean(Result.Value));
     cpChar  : StackPush(Char(Result.Value));
     cpInt   : StackPush(Int64(Result.Value));
     cpFloat : StackPush(Extended(Result.Value));
     cpString: StackPush(String(Result.Value));
    End;

    Exit;
   End;
 End;
End;

{ ACALL (int) }
Procedure op_ACALL(const M: TMachine);
Var NewAddr: LongWord;
Begin
 With M do
 Begin
  NewAddr := read_param.getReference;
  CallstackPush(getPosition);
  setPosition(NewAddr);
 End;
End;

{ RET() }
Procedure op_RET(const M: TMachine);
Begin
 With M do
  setPosition(CallstackPop);
End;

{ IF_E (value, value) }
Procedure op_IF_E(const M: TMachine);
Var P1, P2: TOpParam;
Begin
 With M do
 Begin
  P1 := read_param;
  P2 := read_param;

  breg[5] := (P1 = P2); // set 'IF' register
 End;
End;

{ IF_NE (value, value) }
Procedure op_IF_NE(const M: TMachine);
Var P1, P2: TOpParam;
Begin
 With M do
 Begin
  P1 := read_param;
  P2 := read_param;

  breg[5] := (P1 <> P2); // set 'IF' register
 End;
End;

{ IF_G (value, value) }
Procedure op_IF_G(const M: TMachine);
Var P1, P2: TOpParam;
Begin
 With M do
 Begin
  P1 := read_param;
  P2 := read_param;

  breg[5] := (P1 > P2); // set 'IF' register
 End;
End;

{ IF_L (value, value) }
Procedure op_IF_L(const M: TMachine);
Var P1, P2: TOpParam;
Begin
 With M do
 Begin
  P1 := read_param;
  P2 := read_param;

  breg[5] := (P1 < P2); // set 'IF' register
 End;
End;

{ IF_GE (value, value) }
Procedure op_IF_GE(const M: TMachine);
Var P1, P2: TOpParam;
Begin
 With M do
 Begin
  P1 := read_param;
  P2 := read_param;

  breg[5] := (P1 >= P2); // set 'IF' register
 End;
End;

{ IF_LE (value, value) }
Procedure op_IF_LE(const M: TMachine);
Var P1, P2: TOpParam;
Begin
 With M do
 Begin
  P1 := read_param;
  P2 := read_param;

  breg[5] := (P1 <= P2); // set 'IF' register
 End;
End;

{ STRJOIN (register, string) }
Procedure op_STRJOIN(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptStringReg: sreg[reg.Index] += param.getString; { string }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptString: Value.Str += param.getString;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''strjoin'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ NOT (register) }
Procedure op_NOT(const M: TMachine);
Var reg: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := not ireg[reg.Index]; { int }
   ptBoolReg : breg[reg.Index] := not breg[reg.Index]; { bool }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptBool, ptInt: Value.Int := not Value.Int;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''not'' called with argument: '+reg.getTypeName);
 End;
End;

{ OR (register, value) }
Procedure op_OR(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] or param.getInt; { int }
   ptBoolReg : breg[reg.Index] := breg[reg.Index] or param.getBool; { bool }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt : Value.Int := Value.Int or param.getInt;
      ptBool: Value.Int := Integer(Boolean(Value.Int) or param.getBool);

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''or'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ XOR (register, value) }
Procedure op_XOR(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] xor param.getInt; { int }
   ptBoolReg : breg[reg.Index] := breg[reg.Index] xor param.getBool; { bool }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt : Value.Int := Value.Int xor param.getInt;
      ptBool: Value.Int := Integer(Boolean(Value.Int) xor param.getBool);

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''xor'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ AND (register, value) }
Procedure op_AND(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] and param.getInt; { int }
   ptBoolReg : breg[reg.Index] := breg[reg.Index] and param.getBool; { bool }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt : Value.Int := Value.Int and param.getInt;
      ptBool: Value.Int := Integer(Boolean(Value.Int) and param.getBool);

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''and'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ SHL (register, value) }
Procedure op_SHL(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] shl param.getInt; { int }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt: Value.Int := Value.Int shl param.getInt;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''shl'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ SHR (register, value) }
Procedure op_SHR(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] shr param.getInt; { int }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt: Value.Int := Value.Int shr param.getInt;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''shr'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ MOD (register, value) }
Procedure op_MOD(const M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
 With M do
 Begin
  reg   := read_param;
  param := read_param;

  if (param.getFloat = 0) Then // division by zero
   ThrowException('Division by zero.');

  Case reg.Typ of
   ptIntReg  : ireg[reg.Index] := ireg[reg.Index] mod param.getInt; { int }
   ptStackVal: { stackval }
    With Stack[StackPos^+reg.Value.Int] do
     Case Typ of
      ptInt: Value.Int := Value.Int div param.getInt;

      else goto Fail;
     End;

   else goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''mod'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
 End;
End;

{ ARSET (register, index count, value) }
Procedure op_ARSET(const M: TMachine);
Var refreg, index_count, new_value: TOpParam;
    PosArray                      : TLongWordArray;
    I                             : LongWord;
Label Fail;
Begin
 With M do
 Begin
  refreg      := read_param;
  index_count := read_param;
  new_value   := read_param;

  SetLength(PosArray, index_count.getInt);
  For I := 0 To index_count.getInt-1 Do
   PosArray[I] := StackPop.getInt;

  Case refreg.Typ of
   ptInt, ptIntReg, ptReferenceReg: getArray(refreg.getReference).setValue(PosArray, @new_value);
   ptStringReg: // string reg
   Begin
    CheckStringBounds(M, sreg[refreg.Index], PosArray[0]);
    sreg[refreg.Index][PosArray[0]] := new_value.getChar;
   End;

   ptCharReg: // char reg
   Begin
    CheckStringBounds(M, ' ', PosArray[0]);
    creg[refreg.Index] := new_value.getChar;
   End;

   ptStackVal: // stackval
    With Stack[StackPos^+refreg.Value.Int] do
     Case Typ of
      ptInt   : getArray(Value.Int).setValue(PosArray, @new_value);
      ptString: // string stackval
      Begin
       CheckStringBounds(M, Value.Str, PosArray[0]);
       Value.Str[PosArray[0]] := new_value.getChar;
      End;

      ptChar: // char stackval
      Begin
       CheckStringBounds(M, ' ', PosArray[0]);
       Value.Int := ord(new_value.getChar);
      End;

      else goto Fail;
     End;

   else
    goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''arset'' called with arguments: '+refreg.getTypeName+', '+index_count.getTypeName+', '+new_value.getTypeName);
 End;
End;

{ ARGET (register, index count, result register) }
Procedure op_ARGET(const M: TMachine);
Var refreg, index_count, out_reg: TOpParam;
    PosArray                    : TLongWordArray;
    I                           : LongWord;
    AValue                      : TOpParam;
Label Fail;
Begin
 With M do
 Begin
  refreg      := read_param;
  index_count := read_param;
  out_reg     := read_param;

  SetLength(PosArray, index_count.getInt);
  For I := 0 To index_count.getInt-1 Do
   PosArray[I] := StackPop.getInt;

  Case refreg.Typ of
   ptInt, ptIntReg, ptReferenceReg: AValue := POpParam(getArray(refreg.getReference).getValue(PosArray))^; { int, reference }
   ptString, ptStringReg: { string }
   Begin
    AValue.Typ       := ptChar;
    AValue.Value.Int := ord(refreg.getString[PosArray[0]]);
   End;

   ptChar, ptCharReg: // char
   Begin
    AValue.Typ       := ptChar;
    AValue.Value.Int := ord(refreg.getChar);
   End;

   ptStackVal: // stackval
    With Stack[StackPos^+refreg.Value.Int] do
     Case Typ of
      ptInt   : AValue := POpParam(getArray(Value.Int).getValue(PosArray))^;
      ptString: // string stackval
      Begin
       CheckStringBounds(M, Value.Str, PosArray[0]);

       AValue.Typ       := ptChar;
       AValue.Value.Int := ord(Value.Str[PosArray[0]]);
      End;

      ptChar: // char stackval
      Begin
       CheckStringBounds(M, ' ', PosArray[0]);

       AValue.Typ       := ptChar;
       AValue.Value.Int := Value.Int;
      End;

      else goto Fail;
     End;

   else
    goto Fail;
  End;

  Case out_reg.Typ of
   ptBoolReg     : breg[out_reg.Index] := AValue.getBool;
   ptCharReg     : creg[out_reg.Index] := AValue.getChar;
   ptIntReg      : ireg[out_reg.Index] := AValue.getInt;
   ptFloatReg    : freg[out_reg.Index] := AValue.getFloat;
   ptStringReg   : sreg[out_reg.Index] := AValue.getString;
   ptReferenceReg: rreg[out_reg.Index] := AValue.getReference;

   else
    goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''arget'' called with arguments: '+refreg.getTypeName+', '+index_count.getTypeName+', '+out_reg.getTypeName);
 End;
End;

{ ARCRT (register, array type, dimensions count) }
Procedure op_ARCRT(const M: TMachine);
Var refreg, typ, dimcount: TOpParam;
    ArrayObj             : TMArray;
    Sizes                : TLongWordArray;
    I                    : LongWord;
Begin
 With M do
 Begin
  refreg   := read_param;
  typ      := read_param;
  dimcount := read_param;

  SetLength(Sizes, dimcount.getInt);
  For I := 0 To dimcount.getInt-1 Do
   Sizes[I] := StackPop.getInt;

  ArrayObj := TMArray.Create(M, typ.getInt, Sizes);

  Case refreg.Typ of
   ptReferenceReg: rreg[refreg.Index] := ArrayObj.getAddress;

   else
    raise eInvalidOpcode.Create('''arcrt'' called with arguments: '+refreg.getTypeName+', '+typ.getTypeName+', '+dimcount.getTypeName);
  End;
 End;
End;

{ ARLEN (register, dimension ID, result register) }
Procedure op_ARLEN(const M: TMachine);
Var refreg, dimension, out_reg: TOpParam;
Begin
 With M do
 Begin
  refreg    := read_param;
  dimension := read_param;
  out_reg   := read_param;

  Case out_reg.Typ of
   ptIntReg: ireg[out_reg.Index] := getArray(refreg.getReference).getSize(dimension.getInt);

   else
    raise eInvalidOpcode.Create('''arlen'' called with arguments: '+refreg.getTypeName+', '+dimension.getTypeName+', '+out_reg.getTypeName);
  End;
 End;
End;

{ OBJFREE }
Procedure op_OBJFREE(const M: TMachine);
Begin
 With M do
  getObject(read_param.getReference).Free;
End;

{ LOCATION }
Procedure op_LOCATION(const M: TMachine);
Begin
 M.read_param; // do nothing, just skip param
End;
End.
