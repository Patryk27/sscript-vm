(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
{$H+}
Unit Interpreter;

 Interface
 Uses VMStruct, VMTypes, Opcodes, SysUtils;

 Procedure op_(const VM: PVM);

 Procedure op_NOP(const VM: PVM);
 Procedure op_STOP(const VM: PVM);
 Procedure op_PUSH(const VM: PVM);
 Procedure op_POP(const VM: PVM);
 Procedure op_ADD(const VM: PVM);
 Procedure op_SUB(const VM: PVM);
 Procedure op_MUL(const VM: PVM);
 Procedure op_DIV(const VM: PVM);
 Procedure op_NEG(const VM: PVM);
 Procedure op_MOV(const VM: PVM);
 Procedure op_JMP(const VM: PVM);
 Procedure op_TJMP(const VM: PVM);
 Procedure op_FJMP(const VM: PVM);
 Procedure op_CALL(const VM: PVM);
 Procedure op_ICALL(const VM: PVM);
 Procedure op_ACALL(const VM: PVM);
 Procedure op_RET(const VM: PVM);
 Procedure op_IF_E(const VM: PVM);
 Procedure op_IF_NE(const VM: PVM);
 Procedure op_IF_G(const VM: PVM);
 Procedure op_IF_L(const VM: PVM);
 Procedure op_IF_GE(const VM: PVM);
 Procedure op_IF_LE(const VM: PVM);
 Procedure op_STRJOIN(const VM: PVM);
 Procedure op_NOT(const VM: PVM);
 Procedure op_OR(const VM: PVM);
 Procedure op_XOR(const VM: PVM);
 Procedure op_AND(const VM: PVM);
 Procedure op_SHL(const VM: PVM);
 Procedure op_SHR(const VM: PVM);
 Procedure op_MOD(const VM: PVM);
 Procedure op_ARSET(const VM: PVM);
 Procedure op_ARGET(const VM: PVM);
 Procedure op_ARCRT(const VM: PVM);
 Procedure op_ARLEN(const VM: PVM);
 Procedure op_STRSET(const VM: PVM);
 Procedure op_STRGET(const VM: PVM);
 Procedure op_STRLEN(const VM: PVM);
 Procedure op_LOCATION(const VM: PVM);

 Type TOpcodeProc = Procedure(const VM: PVM);
 Const OpcodeTable: Array[TOpcodeKind] of TOpcodeProc = // opcode list
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
  @op_STRSET,
  @op_STRGET,
  @op_STRLEN
 );

 Implementation
Uses VMObjects, VMStack, VMICall, VMExceptions, TypInfo;

(* InvalidArgumentsException *)
Procedure InvalidArgumentsException(const VM: PVM; const ArgList: Array of TMixedValue);
Var OpName: String;
Begin
 OpName := GetEnumName(TypeInfo(TOpcodeKind), VM^.Bytecode.getCurrentOpcode^);
 Delete(OpName, 1, 2); // remove the "o_"

 Case Length(ArgList) of
  1: VM^.ThrowException('''%s'' called with arguments: %s', [OpName, getTypeName(ArgList[0])]);
  2: VM^.ThrowException('''%s'' called with arguments: %s, %s', [OpName, getTypeName(ArgList[0]), getTypeName(ArgList[1])]);
  3: VM^.ThrowException('''%s'' called with arguments: %s, %s, %s', [OpName, getTypeName(ArgList[0]), getTypeName(ArgList[1]), getTypeName(ArgList[2])]);

  else
   VM^.ThrowException('InvalidArgumentsException()');
 End;
End;

(* CheckStringBounds *)
{
 Checks if "Index" fits in string bounds (1..Length).
}
Procedure CheckStringBounds(const VM: PVM; const Str: PVMString; const Index: VMInt);
Var Len: uint32;
Begin
 if (Index < 1) Then
  VM^.ThrowException('String index out of bounds. Tried to access char #%d, while strings starts from 1.', [Index]);

 if (Str = nil) Then
  Len := 1 Else
  Len := Str^.Length;

 if (Index > Len) Then
  VM^.ThrowException('String index out of bounds. Tried to access char #%d, while %d is the last one.', [Index, Len]);
End;

{ _ }
Procedure op_(const VM: PVM); // an unimplemented opcode
Begin
 VM^.ThrowException('Opcode 0x%x unimplemented!', [VM^.Bytecode.getCurrentOpcode^]);
End;

{ NOP() }
Procedure op_NOP(const VM: PVM);
Begin
End;

{ STOP() }
Procedure op_STOP(const VM: PVM);
Begin
 VM^.Stop := True;
End;

{ PUSH (value) }
Procedure op_PUSH(const VM: PVM);
Begin
 With VM^ do
  Stack.Push(Bytecode.read_param(True));
End;

{ POP (register) }
Procedure op_POP(const VM: PVM);
Var reg, val: TMixedValue;
Begin
 With VM^ do
 Begin
  val := Stack.Pop;
  reg := Bytecode.read_param;

  if (not (reg.isReg or reg.isStackval)) Then
   VM^.ThrowException('''pop'' requires the first parameter to be a register or a stackval.');

  if (reg.isStackval) Then
  Begin
   reg.Stackval^ := val;
  End Else
  Begin
   Case reg.Typ of
    mvBool     : Regs.b[reg.RegIndex] := getBool(val);
    mvChar     : Regs.c[reg.RegIndex] := getChar(val);
    mvInt      : Regs.i[reg.RegIndex] := getInt(val);
    mvFloat    : Regs.f[reg.RegIndex] := getFloat(val);
    mvString   : Regs.s[reg.RegIndex] := getString(val);
    mvReference: Regs.r[reg.RegIndex] := getReference(val);

    else
     VM^.ThrowException('''pop'' called with arguments: '+getTypeName(reg)+' <- '+getTypeName(val));
   End;
  End;
 End;
End;

{ ADD (lvalue, value) }
Procedure op_ADD(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''add'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) Then
  Begin
   { ADD (memory reference, value) }
   Case param.Typ of
    mvChar : PByte(reg.MemAddr)^    += getInt(param); { char }
    mvInt  : PVMInt(reg.MemAddr)^   += getInt(param); { int }
    mvFloat: PVMFloat(reg.MemAddr)^ += getFloat(param); { float }

    else
     VM^.ThrowException('''add'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { ADD (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex])+getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] += getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] += getFloat(param); { float }

    else
     VM^.ThrowException('''add'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { ADD (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvChar : Value.Char := chr(ord(Value.Char)+getInt(param)); { char }
     mvInt  : Value.Int += getInt(param); { int }
     mvFloat: Value.Float += getFloat(param); { float }

     else
      VM^.ThrowException('''add'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ SUB (lvalue, value) }
Procedure op_SUB(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''sub'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) Then
  Begin
   { SUB (memory reference, value) }
   Case param.Typ of
    mvChar : PByte(reg.MemAddr)^    -= getInt(param); { char }
    mvInt  : PVMInt(reg.MemAddr)^   -= getInt(param); { int }
    mvFloat: PVMFloat(reg.MemAddr)^ -= getFloat(param); { float }

    else
     VM^.ThrowException('''sub'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { SUB (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex])-getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] -= getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] -= getFloat(param); { float }

    else
     VM^.ThrowException('''sub'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { SUB (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvChar : Value.Char := chr(ord(Value.Char)-getInt(param)); { char }
     mvInt  : Value.Int -= getInt(param); { int }
     mvFloat: Value.Float -= getFloat(param); { float }

     else
      VM^.ThrowException('''sub'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ MUL (lvalue, value) }
Procedure op_MUL(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''mul'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) Then
  Begin
   { MUL (memory reference, value) }
   Case param.Typ of
    mvChar : PByte(reg.MemAddr)^    *= getInt(param); { char }
    mvInt  : PVMInt(reg.MemAddr)^   *= getInt(param); { int }
    mvFloat: PVMFloat(reg.MemAddr)^ *= getFloat(param); { float }

    else
     VM^.ThrowException('''mul'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { MUL (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex])*getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] *= getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] *= getFloat(param); { float }

    else
     VM^.ThrowException('''mul'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { MUL (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvChar : Value.Char := chr(ord(Value.Char)*getInt(param)); { char }
     mvInt  : Value.Int *= getInt(param); { int }
     mvFloat: Value.Float *= getFloat(param); { float }

     else
      VM^.ThrowException('''mul'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ DIV (lvalue, value) }
Procedure op_DIV(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''div'' requires the first parameter to be an L-value.');

 // if (getFloat(param) = 0) Then @TODO
 //  div_by_zero();

  if (reg.isMemRef) Then
  Begin
   { DIV (memory reference, value) }
   Case param.Typ of
    mvChar : PByte(reg.MemAddr)^    := PByte(reg.MemAddr)^ div getInt(param); { char }
    mvInt  : PVMInt(reg.MemAddr)^   := PVMInt(reg.MemAddr)^ div getInt(param); { int }
    mvFloat: PVMFloat(reg.MemAddr)^ /= getFloat(param); { float }

    else
     VM^.ThrowException('''div'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { DIV (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex]) div getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] div getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] /= getFloat(param); { float }

    else
     VM^.ThrowException('''div'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { DIV (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvChar : Value.Char := chr(ord(Value.Char) div getInt(param)); { char }
     mvInt  : Value.Int := Value.Int div getInt(param); { int }
     mvFloat: Value.Float /= getFloat(param); { float }

     else
      VM^.ThrowException('''div'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ NEG (register/stackval) }
Procedure op_NEG(const VM: PVM);
Var reg: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := Bytecode.read_param;

  if (not (reg.isReg or reg.isStackval)) Then
   VM^.ThrowException('''neg'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { NEG (register) }
   Case reg.Typ of
    mvInt  : Regs.i[reg.RegIndex] := -Regs.i[reg.RegIndex]; { int }
    mvFloat: Regs.f[reg.RegIndex] := -Regs.f[reg.RegIndex]; { float }

    else
     VM^.ThrowException('''neg'' called with argument: '+getTypeName(reg));
   End;
  End Else
  Begin
   { NEG (stackval) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvInt  : Value.Int   := -Value.Int; { int }
     mvFloat: Value.Float := -Value.Float; { float }
    End;
  End;
 End;
End;

{ MOV (lvalue, value) }
Procedure op_MOV(const VM: PVM);
Var reg, val: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := Bytecode.read_param;
  val := Bytecode.read_param(True);

  if (not reg.isLValue) Then
   VM^.ThrowException('''mov'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) Then
  Begin
   { MOV (memory reference, value) }
   Case val.Typ of
    mvBool     : PVMBool(reg.MemAddr)^  := getBool(val); { bool }
    mvChar     : PVMChar(reg.MemAddr)^  := getChar(val); { char }
    mvInt      : PVMInt(reg.MemAddr)^   := getInt(val); { int }
    mvFloat    : PVMFloat(reg.MemAddr)^ := getFloat(val); { float }
    mvString   : PPointer(reg.MemAddr)^ := getString(val); { string }
    mvReference: PPointer(reg.MemAddr)^ := getReference(val); { reference }

    else
     VM^.ThrowException('''mov'' called with arguments: '+getTypeName(reg)+', '+getTypeName(val));
   End;
  End Else

  if (reg.isReg) Then
  Begin
   { MOV (register, value) }
   Case reg.Typ of
    mvBool     : Regs.b[reg.RegIndex] := getBool(val); { bool }
    mvChar     : Regs.c[reg.RegIndex] := getChar(val); { char }
    mvInt      : Regs.i[reg.RegIndex] := getInt(val); { int }
    mvFloat    : Regs.f[reg.RegIndex] := getFloat(val); { float }
    mvString   : Regs.s[reg.RegIndex] := getString(val); { string }
    mvReference: Regs.r[reg.RegIndex] := getReference(val); { reference }

    else
     VM^.ThrowException('''mov'' called with arguments: '+getTypeName(reg)+', '+getTypeName(val));
   End;
  End Else

  Begin
   { MOV (stackval, value) }
   reg.Stackval^ := val;
  End;
 End;
End;

{ JMP (const int) }
Procedure op_JMP(const VM: PVM);
Var NewAddr: VMInt;
Begin
 With VM^ do
 Begin
  NewAddr := Bytecode.getRelativePosition;
  NewAddr += getInt(Bytecode.read_param)-1; // '-1' because instruction pointer will be incremented after this JMP finishes executing and finally we'd jump one byte further than we want to
  Bytecode.setRelativePosition(NewAddr);
 End;
End;

{ TJMP (const int) }
Procedure op_TJMP(const VM: PVM);
Var NewAddr: VMInt;
Begin
 With VM^ do
 Begin
  NewAddr := Bytecode.getRelativePosition;
  NewAddr += getInt(Bytecode.read_param)-1;

  if (Regs.b[5]) Then
   Bytecode.setRelativePosition(NewAddr);
 End;
End;

{ FJMP (const int) }
Procedure op_FJMP(const VM: PVM);
Var NewAddr: VMInt;
Begin
 With VM^ do
 Begin
  NewAddr := Bytecode.getRelativePosition;
  NewAddr += getInt(Bytecode.read_param)-1;

  if (not Regs.b[5]) Then
   Bytecode.setRelativePosition(NewAddr);
 End;
End;

{ CALL (const int) }
Procedure op_CALL(const VM: PVM);
Var NewAddr: VMInt;
    Elem   : TStackElement;
Begin
 With VM^ do
 Begin
  NewAddr := Bytecode.getRelativePosition;
  NewAddr += getInt(Bytecode.read_param)-1;

  Elem.Reset;
  Elem.Typ       := mvCallstackRef;
  Elem.Value.Int := VMInt(Bytecode.getPosition);
  Stack.Push(Elem); // push the old position onto the stack

  Bytecode.setRelativePosition(NewAddr);
 End;
End;

{ ICALL (const string) }
Procedure op_ICALL(const VM: PVM);
Var Name  : String;
    NameVS: PVMString;
    Call  : PInternalCall;
    Params: PMixedValue;
    Result: PMixedValue;
    I     : int16;

    Param: TMixedValue;
Begin
 With VM^ do
 Begin
  NameVS := getString(Bytecode.read_param);

  Name := NameVS^.asString;

  // VM^.VMStringList.Remove(NameVS);

  if (Name[1] = 'v') and (Name[2] = 'm') and (Name[3] = '.') Then
  Begin
   {$I interpreter_icalls.pas}
   Exit;
  End;

  { user call }
  For Call in InternalCallList Do // each icall
  if (AnsiCompareStr(Call^.FullName, Name) = 0) Then // is this what we are searching for?
   Begin
    Params := AllocMem(Call^.ParamCount*sizeof(TMixedValue));

    For I := 0 To Call^.ParamCount-1 Do // get parameters
     Params[I] := Stack.Pop;

    New(Result);
    Result^.Reset;

    Call^.Handler(VM, @Params[0], Result); // call handler

    if (Result^.Typ <> mvNone) Then // push result
     Stack.Push(Result^);

    Dispose(Result);
    Dispose(Params);

    Exit;
   End;

  VM^.ThrowException('Undefined internal call: %s', [Name]);
 End;
End;

{ ACALL (int) }
Procedure op_ACALL(const VM: PVM);
Var NewAddr: VMIReference;
    Elem   : TStackElement;
Begin
 With VM^ do
 Begin
  NewAddr := getInt(Bytecode.read_param);

  Elem.Typ       := mvCallstackRef;
  Elem.Value.Int := VMIReference(Bytecode.getPosition);
  Stack.Push(Elem); // push the old position onto the stack

  Bytecode.setPosition(PByte(NewAddr));
 End;
End;

{ RET() }
Procedure op_RET(const VM: PVM);
Begin
 With VM^ do
  Bytecode.setPosition(PByte(getInt(Stack.Pop)));
End;

{ IF_E (value, value) }
Procedure op_IF_E(const VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := Bytecode.read_param;
  P2 := Bytecode.read_param;

  Regs.b[5] := (P1 = P2); // set 'IF' register
 End;
End;

{ IF_NE (value, value) }
Procedure op_IF_NE(const VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := Bytecode.read_param;
  P2 := Bytecode.read_param;

  Regs.b[5] := (P1 <> P2); // set 'IF' register
 End;
End;

{ IF_G (value, value) }
Procedure op_IF_G(const VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := Bytecode.read_param;
  P2 := Bytecode.read_param;

  Regs.b[5] := (P1 > P2); // set 'IF' register
 End;
End;

{ IF_L (value, value) }
Procedure op_IF_L(const VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := Bytecode.read_param;
  P2 := Bytecode.read_param;

  Regs.b[5] := (P1 < P2); // set 'IF' register
 End;
End;

{ IF_GE (value, value) }
Procedure op_IF_GE(const VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := Bytecode.read_param;
  P2 := Bytecode.read_param;

  Regs.b[5] := (P1 >= P2); // set 'IF' register
 End;
End;

{ IF_LE (value, value) }
Procedure op_IF_LE(const VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := Bytecode.read_param;
  P2 := Bytecode.read_param;

  Regs.b[5] := (P1 <= P2); // set 'IF' register
 End;
End;

{ STRJOIN (lvalue, char/string) }
Procedure op_STRJOIN(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''strjoin'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) Then
  Begin
   { STRJOIN (memory reference, char/string) }
   Case param.Typ of
    mvChar  : StringConcat(reg.MemAddr, getChar(Param)); // char
    mvString: StringConcat(reg.MemAddr, getString(Param)); // string

    else
     VM^.ThrowException('''strjoin'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else

  if (reg.isReg) Then
  Begin
   { STRJOIN (register, char/string) }
   Case reg.Typ of
    mvChar  : StringConcat(Regs.s[reg.RegIndex], getChar(param)); // char
    mvString: StringConcat(Regs.s[reg.RegIndex], getString(param)); // string

    else
     VM^.ThrowException('''strjoin'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else

  Begin
   { STRJOIN (stackval, char/string) }
   With reg.Stackval^ do
   Begin
    Case reg.Typ of
     mvChar  : StringConcat(Value.Str, getChar(param)); // char
     mvString: StringConcat(Value.Str, getString(param)); // string

     else
      VM^.ThrowException('''strjoin'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
   End;
  End;
 End;
End;

{ NOT (register/stackval) }
Procedure op_NOT(const VM: PVM);
Var reg: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := Bytecode.read_param;

  if not (reg.isReg or reg.isStackval) Then
   VM^.ThrowException('''not'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { NOT (register) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := not Regs.b[reg.RegIndex]; { bool }
    mvInt : Regs.i[reg.RegIndex] := not Regs.i[reg.RegIndex]; { int }

    else
     VM^.ThrowException('''not'' called with argument: '+getTypeName(reg));
   End;
  End Else
  Begin
   { NOT (stackval) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := not Value.Bool; { bool }
     mvInt : Value.Int  := not Value.Int; { int }

     else
      VM^.ThrowException('''not'' called with argument: '+getTypeName(reg));
    End;
  End;
 End;
End;

{ OR (lvalue, value) }
Procedure op_OR(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''or'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) THen
  Begin
   { OR (memory reference, value) }
   Case param.Typ of
    mvBool: PVMBool(reg.MemAddr)^ := PVMBool(reg.MemAddr)^ or getBool(param); { bool }
    mvInt : PVMInt(reg.MemAddr)^  := PVMInt(reg.MemAddr)^ or getInt(Param); { int }

    else
     VM^.ThrowException('''or'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { OR (register, value) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := Regs.b[reg.RegIndex] or getBool(param); { bool }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] or getInt(param); { int }

    else
     VM^.ThrowException('''or'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { OR (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := Value.Bool or getBool(param); { bool }
     mvInt : Value.Int  := Value.Int or getInt(param); { int }

     else
      VM^.ThrowException('''or'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ XOR (lvalue, value) }
Procedure op_XOR(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''xor'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) THen
  Begin
   { XOR (memory reference, value) }
   Case param.Typ of
    mvBool: PVMBool(reg.MemAddr)^ := PVMBool(reg.MemAddr)^ xor getBool(param); { bool }
    mvInt : PVMInt(reg.MemAddr)^  := PVMInt(reg.MemAddr)^ xor getInt(Param); { int }

    else
     VM^.ThrowException('''xor'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { XOR (register, value) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := Regs.b[reg.RegIndex] xor getBool(param); { bool }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] xor getInt(param); { int }

    else
     VM^.ThrowException('''xor'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { XOR (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := Value.Bool xor getBool(param); { bool }
     mvInt : Value.Int  := Value.Int xor getInt(param); { int }

     else
      VM^.ThrowException('''xor'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ AND (lvalue, value) }
Procedure op_AND(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''and'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) THen
  Begin
   { AND (memory reference, value) }
   Case param.Typ of
    mvBool: PVMBool(reg.MemAddr)^ := PVMBool(reg.MemAddr)^ and getBool(param); { bool }
    mvInt : PVMInt(reg.MemAddr)^  := PVMInt(reg.MemAddr)^ and getInt(Param); { int }

    else
     VM^.ThrowException('''and'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { AND (register, value) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := Regs.b[reg.RegIndex] and getBool(param); { bool }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] and getInt(param); { int }

    else
     VM^.ThrowException('''and'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { AND (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := Value.Bool and getBool(param); { bool }
     mvInt : Value.Int  := Value.Int and getInt(param); { int }

     else
      VM^.ThrowException('''and'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ SHL (lvalue, value) }
Procedure op_SHL(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''shl'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) THen
  Begin
   { OR (memory reference, value) }
   Case param.Typ of
    mvInt: PVMInt(reg.MemAddr)^ := PVMInt(reg.MemAddr)^ shl getInt(Param); { int }

    else
     VM^.ThrowException('''shl'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { SHL (register, value) }
   Case reg.Typ of
    mvInt: Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] shl getInt(param); { int }

    else
     VM^.ThrowException('''shl'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { SHL (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvInt: Value.Int := Value.Int shl getInt(param); { int }

     else
      VM^.ThrowException('''shl'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ SHR (lvalue, value) }
Procedure op_SHR(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''shr'' requires the first parameter to be an L-value.');

  if (reg.isMemRef) THen
  Begin
   { OR (memory reference, value) }
   Case param.Typ of
    mvInt: PVMInt(reg.MemAddr)^ := PVMInt(reg.MemAddr)^ shr getInt(Param); { int }

    else
     VM^.ThrowException('''shr'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { SHr (register, value) }
   Case reg.Typ of
    mvInt: Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] shr getInt(param); { int }

    else
     VM^.ThrowException('''shr'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { Sr (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvInt: Value.Int := Value.Int shr getInt(param); { int }

     else
      VM^.ThrowException('''shr'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ MOD (lvalue, value) }
Procedure op_MOD(const VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := Bytecode.read_param;
  param := Bytecode.read_param;

  if (not reg.isLValue) Then
   VM^.ThrowException('''mod'' requires the first parameter to be an L-value.');

  // if (getInt(param) = 0) Then
  //  div_by_zero(); // @TODO

  if (reg.isMemRef) THen
  Begin
   { MOD (memory reference, value) }
   Case param.Typ of
    mvChar: PVMChar(reg.MemAddr)^ := VMChar(VMIChar(PVMChar(reg.MemAddr)^) mod VMIChar(getChar(Param))); { char }
    mvInt : PVMInt(reg.MemAddr)^ := PVMInt(reg.MemAddr)^ mod getInt(Param); { int }

    else
     VM^.ThrowException('''mod'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  if (reg.isReg) Then
  Begin
   { MOD (register, value) }
   Case reg.Typ of
    mvChar: Regs.c[reg.RegIndex] := VMChar(VMIChar(Regs.c[reg.RegIndex]) mod VMIChar(getChar(param))); { char }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] mod getInt(param); { int }

    else
     VM^.ThrowException('''mod'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { MOD (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvChar: Value.Char := VMChar(VMIChar(Value.Char) mod VMIChar(getInt(param))); { char }
     mvInt : Value.Int := Value.Int mod getInt(param); { int }

     else
      VM^.ThrowException('''mod'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ ARSET (reg/stvl/mem arrayReference, int indexCount, newValue) }
Procedure op_ARSET(const VM: PVM);
Var arrayReference, indexCount, newValue: TMixedValue;
    IndexArray                          : TIndexArray;

    ArrayPnt: VMReference;
    Typ     : TMixedValueType;

    I: uint32;
Begin
 With VM^ do
 Begin
  // read parameters
  arrayReference := Bytecode.read_param;
  indexCount     := Bytecode.read_param;
  newValue       := Bytecode.read_param;

  // read indexes
  SetLength(IndexArray, getInt(indexCount));
  For I := 0 To High(IndexArray) Do
   IndexArray[I] := getInt(Stack.Pop);

  // prepare pointers if register
  if (arrayReference.isReg) Then
  Begin
   ArrayPnt := getReference(arrayReference);
   Typ      := arrayReference.Typ;
  End Else

  // prepare pointers if stackval
  if (arrayReference.isStackval) Then
  Begin
   ArrayPnt := Pointer(arrayReference.Value.Int);
   Typ      := arrayReference.Stackval^.Typ;
  End Else

  // prepare pointers if memory reference
  if (arrayReference.isMemRef) Then
  Begin
   ArrayPnt := PPointer(arrayReference.MemAddr)^;
   Typ      := mvReference;
  End Else

  // throw an exception if something other
  Begin
   VM^.ThrowException('''arset'' requires the first parameter to be a register, stackval or memory reference.');
  End;

  // do magic
  Case Typ of
   // array reference
   mvReference:
   Begin
    TMArray(CheckObject(ArrayPnt)).setValue(IndexArray, newValue);
   End;

   // invalid
   else
    VM^.ThrowException('''arset'' called with arguments: %s, %s, %s', [getTypeName(arrayReference), getTypeName(indexCount), getTypeName(newValue)]);
  End;
 End;
End;

{ ARGET (reg/stvl/mem arrayReference, int indexCount, out outValue) }
Procedure op_ARGET(const VM: PVM);
Var arrayReference, indexCount, outValue: TMixedValue;
    IndexArray                          : TIndexArray;

    ArrayPnt: Pointer;
    Typ     : TMixedValueType;

    AValue: TMixedValue;

    I: uint32;
Label Fail;
Begin
 With VM^ do
 Begin
  // reset temporary variable
  AValue.Reset;

  // read parameters
  arrayReference := Bytecode.read_param;
  indexCount     := Bytecode.read_param;
  outValue       := Bytecode.read_param;

  // read indexes
  SetLength(IndexArray, getInt(indexCount));
  For I := 0 To High(IndexArray) Do
   IndexArray[I] := getInt(Stack.Pop);

  // prepare variables if register
  if (arrayReference.isReg) Then
  Begin
   ArrayPnt := getReference(arrayReference);
   Typ      := arrayReference.Typ;
  End Else

  // prepare variables if stackval
  if (arrayReference.isStackval) Then
  Begin
   ArrayPnt := Pointer(arrayReference.Stackval^.Value.Int);
   Typ      := arrayReference.Stackval^.Typ;
  End Else

  // prepare variables if memory reference
  if (arrayReference.isMemRef) Then
  Begin
   ArrayPnt := PPointer(arrayReference.MemAddr)^;
   Typ      := mvReference;
  End Else

  // throw an exception otherwise
  Begin
   goto Fail;
  End;

  // do magic
  Case Typ of
   // array reference
   mvReference:
   Begin
    AValue := TMArray(CheckObject(ArrayPnt)).getValue(IndexArray);
   End;

   // invalid
   else
    goto Fail;
  End;

  // save result (register)
  if (outValue.isReg) Then
  Begin
   Case outValue.Typ of
    mvBool     : Regs.b[outValue.RegIndex] := getBool(AValue);
    mvChar     : Regs.c[outValue.RegIndex] := getChar(AValue);
    mvInt      : Regs.i[outValue.RegIndex] := getInt(AValue);
    mvFloat    : Regs.f[outValue.RegIndex] := getFloat(AValue);
    mvString   : Regs.s[outValue.RegIndex] := getString(AValue);
    mvReference: Regs.r[outValue.RegIndex] := getReference(AValue);

    else
     goto Fail;
   End;
  End Else

  // save result (stackval)
  if (outValue.isStackval) Then
  Begin
   outValue.Stackval^ := AValue;
  End Else

  // throw exception if invalid result parameter
  Begin
   goto Fail;
  End;

  Exit;

 Fail:
  VM^.ThrowException('''arget'' called with arguments: %s, %s, %s', [getTypeName(arrayReference), getTypeName(indexCount), getTypeName(outValue)]);
 End;
End;

{ ARCRT (reg/stvl/mem arrayReference, int arrayType, const int dimensionCount) }
Procedure op_ARCRT(const VM: PVM);
Var arrayReference, arrayType, dimensionCount: TMixedValue;
    SizeArray                                : TIndexArray;

    ArrayObj: TMArray;

    I: uint32;
Label Fail;
Begin
 With VM^ do
 Begin
  // read parameters
  arrayReference := Bytecode.read_param;
  arrayType      := Bytecode.read_param;
  dimensionCount := Bytecode.read_param;

  // read dimension sizes
  SetLength(SizeArray, getInt(dimensionCount));
  For I := 0 To High(SizeArray) Do
   SizeArray[I] := getInt(Stack.Pop);

  // create array
  ArrayObj := TMArray.Create(VM, getInt(arrayType), SizeArray);

  // save result if register
  if (arrayReference.isReg) Then
  Begin
   if (arrayReference.Typ = mvReference) Then
    Regs.r[arrayReference.RegIndex] := ArrayObj Else
    goto Fail;
  End Else

  // save result if stackval
  if (arrayReference.isStackval) Then
  Begin
   With arrayReference do
   Begin
    Stackval^.Typ       := mvReference;
    Stackval^.Value.Int := VMIReference(ArrayObj);
   End;
  End Else

  // save result if memref
  if (arrayReference.isMemRef) Then
  Begin
   PPointer(arrayReference.MemAddr)^ := ArrayObj;
  End Else

  // fail otherwise
  Begin
   goto Fail;
  End;

  Exit;
 End;

Fail:
 InvalidArgumentsException(VM, [arrayReference, arrayType, dimensionCount]);
End;

{ ARLEN (reg/stvl/mem arrayReference, int dimensionId, reg/stvl/mem int arrayLength) }
Procedure op_ARLEN(const VM: PVM);
Var arrayReference, dimensionId, arrayLength: TMixedValue;
    DimSize                                 : uint32;

    ArrayPnt: Pointer;
Label Fail;
Begin
 With VM^ do
 Begin
  // read parameters
  arrayReference := Bytecode.read_param;
  dimensionId    := Bytecode.read_param;
  arrayLength    := Bytecode.read_param;

  // get array pointer if arrayReference is register
  if (arrayReference.isReg) Then
  Begin
   ArrayPnt := getReference(arrayReference);
  End Else

  // get array pointer if stackval
  if (arrayReference.isStackval) Then
  Begin
   ArrayPnt := getReference(arrayReference.Stackval^);
  End Else

  // get array pointer if memory reference
  if (arrayReference.isMemRef) Then
  Begin
   ArrayPnt := PPointer(arrayReference.MemAddr)^;
  End Else

  // fail otherwise
  Begin
   goto Fail;
  End;

  // fetch array size
  DimSize := TMArray(CheckObject(ArrayPnt)).getSize(getInt(dimensionId));

  // save array size to register
  if (arrayLength.isReg) Then
  Begin
   Regs.i[arrayLength.RegIndex] := DimSize;
  End Else

  // save array size to stackval
  if (arrayLength.isStackval) Then
  Begin
   With arrayLength do
   Begin
    Stackval^.Typ       := mvInt;
    Stackval^.Value.Int := DimSize;
   End;
  End Else

  // save array size to memref
  if (arrayLength.isMemRef) Then
  Begin
   PVMInt(arrayLength.MemAddr)^ := DimSize;
  End Else

  // or fail, if none of the above fits the parameter list
  Begin
   goto Fail;
  End;

  Exit;
 End;

Fail:
 InvalidArgumentsException(VM, [arrayReference, dimensionId, arrayLength]);
End;

{ STRSET (reg/stvl/mem string modString, int charIndex, char newValue) }
Procedure op_STRSET(const VM: PVM);
Var modString, charIndex, newValue: TMixedValue;

    Index: VMInt;
    Str  : PVMString;
Label Fail;
Begin
 With VM^ do
 Begin
  // read parameter list
  modString := Bytecode.read_param;
  charIndex := Bytecode.read_param;
  newValue  := Bytecode.read_param;

  // get index
  Index := getInt(charIndex);

  // fetch string if modString is register
  if (modString.isReg) Then
  Begin
   Str := getString(modString);
  End Else

  // fetch string if modString is stackval
  if (modString.isStackval) Then
  Begin
   Str := getString(modString.Stackval^);
  End Else

  // fetch string if modString is memory reference
  if (modString.isMemRef) Then
  Begin
   Str := PPointer(modString.MemAddr)^;
  End Else

  // give up otherwise
  Begin
   goto Fail;
  End;

  // check bounds
  CheckStringBounds(VM, Str, Index);

  // modify string
  Str^.Data[Index-1] := getChar(newValue);

  Exit;
 End;

Fail:
 InvalidArgumentsException(VM, [modString, charIndex, newValue]);
End;

{ STRGET (string modString, int charIndex, reg/stvl/mem char outValue) }
Procedure op_STRGET(const VM: PVM);
Var modString, charIndex, outValue: TMixedValue;

    Index: VMInt;
    Char : System.Char;
    Str  : PVMString;
Label Fail;
Begin
 With VM^ do
 Begin
  // read parameters
  modString := Bytecode.read_param;
  charIndex := Bytecode.read_param;
  outValue  := Bytecode.read_param;

  // get index
  Index := getInt(charIndex);

  // fetch string
  Str := getString(modString);

  // check bounds
  CheckStringBounds(VM, Str, Index);

  // fetch char
  Char := Str^.Data[Index-1];

  // save char to register
  if (outValue.isReg) and (outValue.Typ = mvChar) Then
  Begin
   Regs.c[outValue.RegIndex] := Char;
  End Else

  // save char to stackval
  if (outValue.isStackval) Then
  Begin
   With outValue do
   Begin
    Stackval^.Typ        := mvChar;
    Stackval^.Value.Char := Char;
   End;
  End Else

  // save char to memref
  if (outValue.isMemRef) Then
  Begin
   PChar(outValue.MemAddr)^ := Char;
  End Else

  // throw an exception otherwise
  Begin
   goto Fail;
  End;

  Exit;
 End;

Fail:
 InvalidArgumentsException(VM, [modString, charIndex, outValue]);
End;

{ STRLEN (string register, out int register/stackval) }
Procedure op_STRLEN(const VM: PVM);
Var strreg, outreg: TMixedValue;
    TmpStr        : PVMString;
Begin
 With VM^ do
 Begin
  strreg := Bytecode.read_param;
  outreg := Bytecode.read_param;

  if (not (strreg.isReg or strreg.isStackval or (strreg.Typ = mvString))) Then
   VM^.ThrowException('''strlen'' requires the first parameter to be a register, stackval or string.');

  if (not ((outreg.isReg and (outreg.Typ = mvInt)) or outreg.isStackval)) Then
   VM^.ThrowException('''strlen'' requires the second parameter to be an int register or a stackval.');

  TmpStr := getString(strreg);

  if (outreg.isStackval) Then
  Begin
   outreg.Stackval^.Typ       := mvInt;
   outreg.Stackval^.Value.Int := TmpStr^.Length;
  End Else
  Begin
   Regs.i[outreg.RegIndex] := TmpStr^.Length;
  End;
 End;
End;

{ LOCATION }
Procedure op_LOCATION(const VM: PVM);
Begin
 VM^.Bytecode.read_param; // do nothing, just skip the parameter
End;
End.
