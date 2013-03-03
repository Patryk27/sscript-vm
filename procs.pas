(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Procs;

 Interface
 Uses SysUtils, Machine;

 Type eInvalidOpcode = Class(Exception);
      eDivByZero     = Class(Exception);

 Procedure op_(M: TMachine);

 Procedure op_NOP(M: TMachine);
 Procedure op_STOP(M: TMachine);
 Procedure op_PUSH(M: TMachine);
 Procedure op_POP(M: TMachine);
 Procedure op_ADD(M: TMachine);
 Procedure op_SUB(M: TMachine);
 Procedure op_MUL(M: TMachine);
 Procedure op_DIV(M: TMachine);
 Procedure op_NEG(M: TMachine);
 Procedure op_MOV(M: TMachine);
 Procedure op_JMP(M: TMachine);
 Procedure op_TJMP(M: TMachine);
 Procedure op_FJMP(M: TMachine);
 Procedure op_CALL(M: TMachine);
 Procedure op_ICALL(M: TMachine);
 Procedure op_ACALL(M: TMachine);
 Procedure op_RET(M: TMachine);
 Procedure op_IF_E(M: TMachine);
 Procedure op_IF_NE(M: TMachine);
 Procedure op_IF_G(M: TMachine);
 Procedure op_IF_L(M: TMachine);
 Procedure op_IF_GE(M: TMachine);
 Procedure op_IF_LE(M: TMachine);
 Procedure op_STRJOIN(M: TMachine);
 Procedure op_NOT(M: TMachine);
 Procedure op_OR(M: TMachine);
 Procedure op_XOR(M: TMachine);
 Procedure op_AND(M: TMachine);
 Procedure op_SHL(M: TMachine);
 Procedure op_SHR(M: TMachine);
 Procedure op_MOD(M: TMachine);
 Procedure op_ARSET(M: TMachine);
 Procedure op_ARGET(M: TMachine);
 Procedure op_ARCRT(M: TMachine);
 Procedure op_ARLEN(M: TMachine);
 Procedure op_OBJFREE(M: TMachine);

 Implementation
Uses Opcodes, Objects;

{ qmagic }
Function qmagic(Param: TOpParam): TOpParam; inline;
Begin
 With Result do
 Begin
  Case Param.Typ of
   ptBoolReg: Typ := ptBool;
   ptCharReg: Typ := ptChar;
   ptIntReg: Typ := ptInt;
   ptFloatReg: Typ := ptFloat;
   ptStringReg: Typ := ptString;
   ptReferenceReg: Typ := ptInt;
   else Typ := Param.Typ;
  End;

  M     := Param.M;
  Val   := Param.Val;
  fVal  := Param.fVal;
  sVal  := Param.sVal;
  Index := Param.Index;
 End;

 if (Param.Typ = ptStackVal) Then
  With Param.M do
   Result := Stack[StackPos^+Param.Val];
End;

{ = }
Operator = (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) Then
  Exit(P1.fVal = P2.Val);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) Then
  Exit(P1.Val = P2.fVal);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Val = P2.Val);
  ptFloat: Exit(P1.fVal = P2.fVal);
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

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) Then
  Exit(P1.fVal > P2.Val);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) Then
  Exit(P1.Val > P2.fVal);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Val > P2.Val);
  ptFloat: Exit(P1.fVal > P2.fVal);
 End;
End;

{ < }
Operator < (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) Then
  Exit(P1.fVal < P2.Val);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) Then
  Exit(P1.Val < P2.fVal);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Val < P2.Val);
  ptFloat: Exit(P1.fVal < P2.fVal);
 End;
End;

{ >= }
Operator >= (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) Then
  Exit(P1.fVal >= P2.Val);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) Then
  Exit(P1.Val >= P2.fVal);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Val >= P2.Val);
  ptFloat: Exit(P1.fVal >= P2.fVal);
 End;
End;

{ <= }
Operator <= (P1, P2: TOpParam): Boolean;
Begin
 Result := False;

 P1 := qmagic(P1);
 P2 := qmagic(P2);

 if (P1.Typ = ptFloat) and (P2.Typ = ptInt) Then
  Exit(P1.fVal <= P2.Val);

 if (P1.Typ = ptInt) and (P2.Typ = ptFloat) Then
  Exit(P1.Val <= P2.fVal);

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  ptBool, ptChar, ptInt: Exit(P1.Val <= P2.Val);
  ptFloat: Exit(P1.fVal <= P2.fVal);
 End;
End;

{ _ }
Procedure op_(M: TMachine);
Begin
 raise eInvalidOpcode.Create('Opcode '''+getOpcodeName(LongWord(M.Position)-sizeof(Byte))+''' unimplemented');
End;

{ NOP }
Procedure op_NOP(M: TMachine);
Begin
End;

{ STOP }
Procedure op_STOP(M: TMachine);
Begin
 Log('');
 Log('-- STOP (reason: `stop` opcode) --');
 raise Exception.Create('');
End;

{ PUSH }
Procedure op_PUSH(M: TMachine);
Var P: TOpParam;
Begin
With M do
Begin
 P := qmagic(read_param);
 Inc(StackPos^);
 Stack[StackPos^] := P;
End;
End;

{ POP }
Procedure op_POP(M: TMachine);
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
  else raise eInvalidOpcode.Create('''pop'' called with arguments: '+reg.getTypeName+', '+val.getTypeName);
 End;
End;
End;

{ ADD }
Procedure op_ADD(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptCharReg: creg[reg.Index] := chr(ord(creg[reg.Index])+param.getInt);
  ptIntReg: ireg[reg.Index] += param.getInt;
  ptFloatReg: freg[reg.Index] += param.getFloat;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptChar, ptInt: Val += param.getInt;
     ptFloat: fVal += param.getFloat;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''add'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ SUB }
Procedure op_SUB(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptCharReg: creg[reg.Index] := chr(ord(creg[reg.Index])-param.getInt);
  ptIntReg: ireg[reg.Index] -= param.getInt;
  ptFloatReg: freg[reg.Index] -= param.getFloat;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptChar, ptInt: Val -= param.getInt;
     ptFloat: fVal -= param.getFloat;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''sub'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ MUL }
Procedure op_MUL(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptCharReg: creg[reg.Index] := chr(ord(creg[reg.Index])*param.getInt);
  ptIntReg: ireg[reg.Index] *= param.getInt;
  ptFloatReg: freg[reg.Index] *= param.getFloat;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptChar, ptInt: Val *= param.getInt;
     ptFloat: fVal *= param.getFloat;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''mul'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ DIV }
Procedure op_DIV(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 if (param.getFloat = 0) Then
  raise eDivByZero.Create('Division by zero.');

 Case reg.Typ of
  ptCharReg: creg[reg.Index] := chr(ord(creg[reg.Index]) div param.getInt);
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] div param.getInt;
  ptFloatReg: freg[reg.Index] /= param.getFloat;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptChar, ptInt: Val := Val div param.getInt;
     ptFloat: fVal /= param.getFloat;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''div'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ NEG }
Procedure op_NEG(M: TMachine);
Var reg: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := -ireg[reg.Index];
  ptFloatReg: freg[reg.Index] := -freg[reg.Index];
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := -Val;
     ptFloat: fVal := -fVal;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''neg'' called with argument: '+reg.getTypeName);
End;
End;

{ MOV }
Procedure op_MOV(M: TMachine);
Var reg, val: TOpParam;
Begin
With M do
Begin
 reg := read_param;
 val := read_param;

 Case reg.Typ of
  ptBoolReg: breg[reg.Index] := val.getBool;
  ptCharReg: creg[reg.Index] := val.getChar;
  ptIntReg: ireg[reg.Index] := val.getInt;
  ptFloatReg: freg[reg.Index] := val.getFloat;
  ptStringReg: sreg[reg.Index] := val.getString;
  ptReferenceReg: rreg[reg.Index] := val.getReference;
  ptStackVal: Stack[StackPos^+reg.Val] := qmagic(val);
  else raise eInvalidOpcode.Create('''mov'' called with arguments: '+reg.getTypeName+', '+val.getTypeName);
 End;
End;
End;

{ JMP }
Procedure op_JMP(M: TMachine);
Var NewAddr: LongWord;
Begin
With M do
Begin
 NewAddr := getPosition;
 NewAddr += read_param.getInt-1;
 setPosition(NewAddr);
End;
End;

{ TJMP }
Procedure op_TJMP(M: TMachine);
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

{ FJMP }
Procedure op_FJMP(M: TMachine);
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

{ CALL }
Procedure op_CALL(M: TMachine);
Var NewAddr: Integer;
Begin
With M do
Begin
 Inc(CallstackPos);

 if (CallstackPos >= CALLSTACK_SIZE) Then
  raise eInvalidOpcode.Create('Cannot do ''call'' - no space left on callstack.');

 NewAddr                 := getPosition;
 NewAddr                 += read_param.getInt-1;
 Callstack[CallstackPos] := getPosition;
 setPosition(NewAddr);
End;
End;

{ ICALL }
Procedure op_ICALL(M: TMachine);
Var Str, PackageName, FunctionName: String;
Begin
With M do
Begin
 Str := read_param.getString;

 if (Str = 'breakpoint') Then
 Begin
  DebugMode := True;
  Exit;
 End;

 PackageName  := Copy(Str, 1, Pos('.', Str)-1);
 FunctionName := Copy(Str, Pos('.', Str)+1, Length(Str));

 RunFunction(PackageName, FunctionName);
End;
End;

{ ACALL }
Procedure op_ACALL(M: TMachine);
Var NewAddr: Integer;
Begin
With M do
Begin
 Inc(CallstackPos);

 if (CallstackPos >= CALLSTACK_SIZE) Then
  raise eInvalidOpcode.Create('Cannot do ''acall'' - no space left on callstack.');

 NewAddr                 := read_param.getReference;
 Callstack[CallstackPos] := getPosition;
 setPosition(NewAddr);
End;
End;

{ RET }
Procedure op_RET(M: TMachine);
Begin
With M do
Begin
 if (CallstackPos < 1) Then
  raise eInvalidOpcode.Create('Cannot do ''ret'' - callstack is empty.');

 setPosition(Callstack[CallstackPos]);
 Dec(CallstackPos);
End;
End;

{ IF_E }
Procedure op_IF_E(M: TMachine);
Var P1, P2: TOpParam;
Begin
With M do
Begin
 P1 := read_param;
 P2 := read_param;

 breg[5] := (P1 = P2); // set 'IF' register
End;
End;

{ IF_NE }
Procedure op_IF_NE(M: TMachine);
Var P1, P2: TOpParam;
Begin
With M do
Begin
 P1 := read_param;
 P2 := read_param;

 breg[5] := (P1 <> P2); // set 'IF' register
End;
End;

{ IF_G }
Procedure op_IF_G(M: TMachine);
Var P1, P2: TOpParam;
Begin
With M do
Begin
 P1 := read_param;
 P2 := read_param;

 breg[5] := (P1 > P2); // set 'IF' register
End;
End;

{ IF_L }
Procedure op_IF_L(M: TMachine);
Var P1, P2: TOpParam;
Begin
With M do
Begin
 P1 := read_param;
 P2 := read_param;

 breg[5] := (P1 < P2); // set 'IF' register
End;
End;

{ IF_GE }
Procedure op_IF_GE(M: TMachine);
Var P1, P2: TOpParam;
Begin
With M do
Begin
 P1 := read_param;
 P2 := read_param;

 breg[5] := (P1 >= P2); // set 'IF' register
End;
End;

{ IF_LE }
Procedure op_IF_LE(M: TMachine);
Var P1, P2: TOpParam;
Begin
With M do
Begin
 P1 := read_param;
 P2 := read_param;

 breg[5] := (P1 <= P2); // set 'IF' register
End;
End;

{ STRJOIN }
Procedure op_STRJOIN(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptStringReg: sreg[reg.Index] += param.getString;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptString: sVal += param.getString;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''strjoin'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ NOT }
Procedure op_NOT(M: TMachine);
Var reg: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := not ireg[reg.Index];
  ptBoolReg: breg[reg.Index] := not breg[reg.Index];
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := not Val;
     ptBool: Val := not Val;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''not'' called with argument: '+reg.getTypeName);
End;
End;

{ OR }
Procedure op_OR(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] or param.getInt;
  ptBoolReg: breg[reg.Index] := breg[reg.Index] or param.getBool;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := Val or param.getInt;
     ptBool: Val := Integer(Boolean(Val) or param.getBool);
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''or'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ XOR }
Procedure op_XOR(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] xor param.getInt;
  ptBoolReg: breg[reg.Index] := breg[reg.Index] xor param.getBool;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := Val xor param.getInt;
     ptBool: Val := Integer(Boolean(Val) xor param.getBool);
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''xor'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ AND }
Procedure op_AND(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] and param.getInt;
  ptBoolReg: breg[reg.Index] := breg[reg.Index] and param.getBool;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := Val and param.getInt;
     ptBool: Val := Integer(Boolean(Val) and param.getBool);
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''and'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ SHL }
Procedure op_SHL(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] shl param.getInt;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := Val shl param.getInt;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''shl'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ SHR }
Procedure op_SHR(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] shr param.getInt;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := Val shr param.getInt;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''shr'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ MOD }
Procedure op_MOD(M: TMachine);
Var reg, param: TOpParam;
Label Fail;
Begin
With M do
Begin
 reg   := read_param;
 param := read_param;

 if (param.getFloat = 0) Then
  raise eDivByZero.Create('Division by zero.');

 Case reg.Typ of
  ptIntReg: ireg[reg.Index] := ireg[reg.Index] mod param.getInt;
  ptStackVal:
   With Stack[StackPos^+reg.Val] do
    Case Typ of
     ptInt: Val := Val div param.getInt;
     else goto Fail;
    End;
  else goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''mod'' called with arguments: '+reg.getTypeName+', '+param.getTypeName);
End;
End;

{ ARSET }
Procedure op_ARSET(M: TMachine);
Var refreg, index_count, value: TOpParam;
    PosArray                  : TLongWordArray;
    I                         : LongWord;
Label Fail;
Begin
With M do
Begin
 refreg      := read_param;
 index_count := read_param;
 value       := read_param;

 SetLength(PosArray, index_count.getInt);
 For I := 0 To index_count.getInt-1 Do
  PosArray[I] := StackPop.getInt;

 Case refreg.Typ of
  ptInt, ptIntReg, ptReferenceReg: getArray(refreg.getReference).setValue(PosArray, @value);
  ptStringReg                    : sreg[refreg.Index][PosArray[0]] := value.getChar;

  ptStackVal:
   With Stack[StackPos^+refreg.Val] do
    Case Typ of
     ptInt   : getArray(Val).setValue(PosArray, @value);
     ptString: sval[PosArray[0]] := value.getChar;
     else goto Fail;
    End;

  else
   goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''arset'' called with arguments: '+refreg.getTypeName+', '+index_count.getTypeName+', '+value.getTypeName);
End;
End;

{ ARGET }
Procedure op_ARGET(M: TMachine);
Var refreg, index_count, out_reg: TOpParam;
    PosArray                    : TLongWordArray;
    I                           : LongWord;
    Value                       : TOpParam;
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
  ptInt, ptIntReg, ptReferenceReg: Value := POpParam(getArray(refreg.getReference).getValue(PosArray))^;
  ptString, ptStringReg:
  Begin
   Value.Typ := ptChar;
   Value.Val := ord(refreg.getString[PosArray[0]]);
  End;

  ptStackVal:
   With Stack[StackPos^+refreg.Val] do
    Case Typ of
     ptInt   : Value := POpParam(getArray(Val).getValue(PosArray))^;
     ptString:
     Begin
      Value.Typ := ptChar;
      Value.Val := ord(sVal[PosArray[0]]);
     End;

     else goto Fail;
    End;

  else
   goto Fail;
 End;

 Case out_reg.Typ of
  ptBoolReg     : breg[out_reg.Index] := Value.getBool;
  ptCharReg     : creg[out_reg.Index] := Value.getChar;
  ptIntReg      : ireg[out_reg.Index] := Value.getInt;
  ptFloatReg    : freg[out_reg.Index] := Value.getFloat;
  ptStringReg   : sreg[out_reg.Index] := Value.getString;
  ptReferenceReg: rreg[out_reg.Index] := Value.getReference;
  else
   goto Fail;
 End;

 Exit;

Fail:
 raise eInvalidOpcode.Create('''arget'' called with arguments: '+refreg.getTypeName+', '+index_count.getTypeName+', '+out_reg.getTypeName);
End;
End;

{ ARCRT }
Procedure op_ARCRT(M: TMachine);
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

 ArrayObj := TMArray.Create(typ.getInt, Sizes);

 Case refreg.Typ of
  ptReferenceReg: rreg[refreg.Index] := ArrayObj.getAddress;
  else
   raise eInvalidOpcode.Create('''arcrt'' called with arguments: '+refreg.getTypeName+', '+typ.getTypeName+', '+dimcount.getTypeName);
 End;
End;
End;

{ ARLEN }
Procedure op_ARLEN(M: TMachine);
Var refreg, index_count, out_reg: TOpParam;
    PosArray                    : TLongWordArray;
    I                           : Integer;
Begin
With M do
Begin
 refreg      := read_param;
 index_count := read_param;
 out_reg     := read_param;

 SetLength(PosArray, index_count.getInt);
 For I := 0 To index_count.getInt-1 Do
  PosArray[I] := StackPop.getInt;

 Case out_reg.Typ of
  ptIntReg: ireg[out_reg.Index] := getArray(refreg.getReference).getSize(PosArray);
  else
   raise eInvalidOpcode.Create('''arlen'' called with arguments: '+refreg.getTypeName+', '+index_count.getTypeName+', '+out_reg.getTypeName);
 End;
End;
End;

{ OBJFREE }
Procedure op_OBJFREE(M: TMachine);
Begin
 With M do
  getObject(read_param.getReference).Free;
End;
End.
