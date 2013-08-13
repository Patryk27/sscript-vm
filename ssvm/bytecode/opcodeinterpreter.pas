(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
{$H+}
Unit OpcodeInterpreter;

 Interface
 Uses VM, Opcodes, SysUtils;

 Procedure op_(VM: PVM);

 Procedure op_NOP(VM: PVM);
 Procedure op_STOP(VM: PVM);
 Procedure op_PUSH(VM: PVM);
 Procedure op_POP(VM: PVM);
 Procedure op_ADD(VM: PVM);
 Procedure op_SUB(VM: PVM);
 Procedure op_MUL(VM: PVM);
 Procedure op_DIV(VM: PVM);
 Procedure op_NEG(VM: PVM);
 Procedure op_MOV(VM: PVM);
 Procedure op_JMP(VM: PVM);
 Procedure op_TJMP(VM: PVM);
 Procedure op_FJMP(VM: PVM);
 Procedure op_CALL(VM: PVM);
 Procedure op_ICALL(VM: PVM);
 Procedure op_ACALL(VM: PVM);
 Procedure op_RET(VM: PVM);
 Procedure op_IF_E(VM: PVM);
 Procedure op_IF_NE(VM: PVM);
 Procedure op_IF_G(VM: PVM);
 Procedure op_IF_L(VM: PVM);
 Procedure op_IF_GE(VM: PVM);
 Procedure op_IF_LE(VM: PVM);
 Procedure op_STRJOIN(VM: PVM);
 Procedure op_NOT(VM: PVM);
 Procedure op_OR(VM: PVM);
 Procedure op_XOR(VM: PVM);
 Procedure op_AND(VM: PVM);
 Procedure op_SHL(VM: PVM);
 Procedure op_SHR(VM: PVM);
 Procedure op_MOD(VM: PVM);
 Procedure op_ARSET(VM: PVM);
 Procedure op_ARGET(VM: PVM);
 Procedure op_ARCRT(VM: PVM);
 Procedure op_ARLEN(VM: PVM);
 Procedure op_STRLEN(VM: PVM);
 Procedure op_LOCATION(VM: PVM);

 Type TOpcodeProc = Procedure(VM: PVM);
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
  @op_STRLEN,
  @op_LOCATION,
  @op_LOCATION,
  @op_LOCATION
 );

 Implementation
Uses Objects, Stack, mStrings;

Type eInvalidOpcode = Class(Exception);

{ CheckStringBounds }
Procedure CheckStringBounds(VM: PVM; const Str: String; const Index: Integer);
Begin
 if (Index < 1) Then
  VM^.ThrowExceptionByMessage(PChar('String out of bounds. Tried to access char #'+IntToStr(Index)+', while string starts at 1.'));

 if (Index > Length(Str)) Then
  VM^.ThrowExceptionByMessage(PChar('String out of bounds. Tried to access char #'+IntToStr(Index)+', while '+IntToStr(Length(Str))+' is the last one.'));
End;

{ _ }
Procedure op_(VM: PVM); // an unimplemented opcode
Begin
 raise Exception.CreateFmt('Opcode `0x%x` unimplemented!', [VM^.CurrentOpcode^]);
End;

{ NOP() }
Procedure op_NOP(VM: PVM);
Begin
End;

{ STOP() }
Procedure op_STOP(VM: PVM);
Begin
 VM^.Stop := True;
End;

{ PUSH (value) }
Procedure op_PUSH(VM: PVM);
Begin
 With VM^ do
  StackPush(read_param);
End;

{ POP (register) }
Procedure op_POP(VM: PVM);
Var reg, val: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := read_param;
  val := StackPop;

  if (not reg.isReg) Then
   raise eInvalidOpcode.Create('''pop'' requires the first parameter to be a register.');

  Case reg.Typ of
   mvBool     : Regs.b[reg.RegIndex] := getBool(val);
   mvChar     : Regs.c[reg.RegIndex] := getChar(val);
   mvInt      : Regs.i[reg.RegIndex] := getInt(val);
   mvFloat    : Regs.f[reg.RegIndex] := getFloat(val);
   mvString   : Regs.s[reg.RegIndex] := getString(val);
   mvReference: Regs.r[reg.RegIndex] := getReference(val);

   else
    raise eInvalidOpcode.Create('''pop'' called with arguments: '+getTypeName(reg)+' <- '+getTypeName(val));
  End;
 End;
End;

{ ADD (register, value) }
Procedure op_ADD(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''add'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { ADD (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex])+getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] += getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] += getFloat(param); { float }

    else
     raise eInvalidOpcode.Create('''add'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
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
      raise eInvalidOpcode.Create('''add'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ SUB (register, value) }
Procedure op_SUB(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''sub'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { SUB (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex])-getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] -= getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] -= getFloat(param); { float }

    else
     raise eInvalidOpcode.Create('''sub'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
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
      raise eInvalidOpcode.Create('''sub'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ MUL (register, value) }
Procedure op_MUL(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''mul'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { SUB (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex])*getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] *= getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] *= getFloat(param); { float }

    else
     raise eInvalidOpcode.Create('''mul'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { SUB (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvChar : Value.Char := chr(ord(Value.Char)*getInt(param)); { char }
     mvInt  : Value.Int *= getInt(param); { int }
     mvFloat: Value.Float *= getFloat(param); { float }

     else
      raise eInvalidOpcode.Create('''mul'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ DIV (register, value) }
Procedure op_DIV(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''div'' requires the first parameter to be a register or a stackval.');

 // if (getFloat(param) = 0) Then
 //  div_by_zero();

  if (reg.isReg) Then
  Begin
   { DIV (register, value) }
   Case reg.Typ of
    mvChar : Regs.c[reg.RegIndex] := chr(ord(Regs.c[reg.RegIndex]) div getInt(param)); { char }
    mvInt  : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] div getInt(param); { int }
    mvFloat: Regs.f[reg.RegIndex] /= getFloat(param); { float }

    else
     raise eInvalidOpcode.Create('''div'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
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
      raise eInvalidOpcode.Create('''div'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ NEG (register) }
Procedure op_NEG(VM: PVM);
Var reg: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''neg'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { NEG (register) }
   Case reg.Typ of
    mvInt  : Regs.i[reg.RegIndex] := -Regs.i[reg.RegIndex]; { int }
    mvFloat: Regs.f[reg.RegIndex] := -Regs.f[reg.RegIndex]; { float }

    else
     raise eInvalidOpcode.Create('''neg'' called with argument: '+getTypeName(reg));
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

{ MOV (register, value) }
Procedure op_MOV(VM: PVM);
Var reg, val: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := read_param;
  val := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''mov'' requires the first parameter to be a register or a stackval.');

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
     raise eInvalidOpcode.Create('''mov'' called with arguments: '+getTypeName(reg)+', '+getTypeName(val));
   End;
  End Else
  Begin
   { MOV (stackval, value) }
   reg.Stackval^ := val;
  End;
 End;
End;

{ JMP (int) }
Procedure op_JMP(VM: PVM);
Var NewAddr: uint32;
Begin
 With VM^ do
 Begin
  NewAddr := getPosition;
  NewAddr += getInt(read_param)-1;
  setPosition(NewAddr);
 End;
End;

{ TJMP (int) }
Procedure op_TJMP(VM: PVM);
Var NewAddr: uint32;
Begin
 With VM^ do
 Begin
  NewAddr := getPosition;
  NewAddr += getInt(read_param)-1;
  if (Regs.b[5]) Then
   setPosition(NewAddr);
 End;
End;

{ FJMP (int) }
Procedure op_FJMP(VM: PVM);
Var NewAddr: uint32;
Begin
 With VM^ do
 Begin
  NewAddr := getPosition;
  NewAddr += getInt(read_param)-1;
  if (not Regs.b[5]) Then
   setPosition(NewAddr);
 End;
End;

{ CALL (int) }
Procedure op_CALL(VM: PVM);
Var NewAddr: uint32;
    Elem   : TStackElement;
Begin
 With VM^ do
 Begin
  NewAddr := getPosition;
  NewAddr += getInt(read_param)-1;

  Elem.Typ       := mvCallstackRef;
  Elem.Value.Int := getPosition;
  StackPush(Elem); // push the old position onto the stack

  setPosition(NewAddr);
 End;
End;

{ ICALL (string) }
Procedure op_ICALL(VM: PVM);
Var Name  : String;
    Call  : PCall;
    Params: PMixedValue;
    Result: PMixedValue;
    I     : int16;

    Param: TMixedValue;
Begin
 With VM^ do
 Begin
  Name := getString(read_param);

  if (Copy(Name, 1, 3) = 'vm.') Then
  Begin
   { vm.exit }
   if (AnsiCompareStr(Name, 'vm.exit') = 0) Then
   Begin
   // ExitCode := getInt(StackPop);
    getInt(StackPop);

    Stop := True;
   End Else

   { vm.save_exception_state }
   if (AnsiCompareStr(Name, 'vm.save_exception_state') = 0) Then
   Begin
    ExceptionStack^ := StackPos^; // save stack position
    Inc(ExceptionStack);

    ExceptionStack^ := ExceptionHandler; // save previous handler
    Inc(ExceptionStack);
   End Else

   { vm.restore_exception_state }
   if (AnsiCompareStr(Name, 'vm.restore_exception_state') = 0) Then
   Begin
    Dec(ExceptionStack);
    ExceptionHandler := ExceptionStack^; // restore previous handler

    Dec(ExceptionStack);
    StackPos^ := ExceptionStack^; // restore stack position
   End Else

   { vm.set_exception_handler }
   if (AnsiCompareStr(Name, 'vm.set_exception_handler') = 0) Then
   Begin
    ExceptionHandler := getInt(StackPop);
   End Else

   { vm.get_exception_handler }
   if (AnsiCompareStr(Name, 'vm.get_exception_handler') = 0) Then
   Begin
    Param.Typ       := mvInt;
    Param.Value.Int := ExceptionHandler;
    StackPush(Param);
   End Else

   { vm.throw }
   if (AnsiCompareStr(Name, 'vm.throw') = 0) Then
   Begin
    ThrowExceptionByMessage(getString(StackPop));
   End Else

   { vm.get_last_exception }
   if (AnsiCompareStr(Name, 'vm.get_last_exception') = 0) Then
   Begin
    if (LastException.Typ <> etByMessage) Then
     raise Exception.Create('VM internal error: opcodeinterpreter.pas => LastException.Typ <> etByMessage');

    Param.Typ       := mvString;
    Param.Value.Str := LastException.Data;
    StackPush(Param);
   End Else

   { vm.version }
   if (AnsiCompareStr(Name, 'vm.version') = 0) Then
   Begin
    Param.Typ       := mvString;
    Param.Value.Str := VMVersion;
    StackPush(Param);
   End Else

    VM^.ThrowExceptionByMessage(PChar('Invalid VM-icall: '+Name));

   Exit;
  End;

  { user call }
  For Call in InternalCallList Do // each icall
  if (AnsiCompareStr(Call^.FullName, Name) = 0) Then // is this what we are searching for?
   Begin
    Params := AllocMem(Call^.ParamCount*sizeof(TMixedValue));

    For I := 0 To Call^.ParamCount-1 Do // get parameters
     Params[I] := StackPop;

    New(Result);
    Result^.Typ := mvNone;

    Call^.Handler(VM, @Params[0], Result); // call handler

    if (Result^.Typ <> mvNone) Then // push result
     StackPush(Result^);

    Dispose(Result);

    Exit;
   End;

  VM^.ThrowExceptionByMessage(PChar('Undefined internal call: '+Name));
 // raise Exception.Create('Undefined internal call: '+Name);
 End;
End;

{ ACALL (int) }
Procedure op_ACALL(VM: PVM);
Var NewAddr: LongWord;
    Elem   : TStackElement;
Begin
 With VM^ do
 Begin
  NewAddr := getInt(read_param);

  Elem.Typ       := mvCallstackRef;
  Elem.Value.Int := getPosition;
  StackPush(Elem); // push the old position onto the stack

  setPosition(NewAddr);
 End;
End;

{ RET() }
Procedure op_RET(VM: PVM);
Begin
 With VM^ do
  setPosition(getInt(StackPop));
End;

{ IF_E (value, value) }
Procedure op_IF_E(VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := read_param;
  P2 := read_param;

  Regs.b[5] := (P1 = P2); // set 'IF' register
 End;
End;

{ IF_NE (value, value) }
Procedure op_IF_NE(VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := read_param;
  P2 := read_param;

  Regs.b[5] := (P1 <> P2); // set 'IF' register
 End;
End;

{ IF_G (value, value) }
Procedure op_IF_G(VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := read_param;
  P2 := read_param;

  Regs.b[5] := (P1 > P2); // set 'IF' register
 End;
End;

{ IF_L (value, value) }
Procedure op_IF_L(VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := read_param;
  P2 := read_param;

  Regs.b[5] := (P1 < P2); // set 'IF' register
 End;
End;

{ IF_GE (value, value) }
Procedure op_IF_GE(VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := read_param;
  P2 := read_param;

  Regs.b[5] := (P1 >= P2); // set 'IF' register
 End;
End;

{ IF_LE (value, value) }
Procedure op_IF_LE(VM: PVM);
Var P1, P2: TMixedValue;
Begin
 With VM^ do
 Begin
  P1 := read_param;
  P2 := read_param;

  Regs.b[5] := (P1 <= P2); // set 'IF' register
 End;
End;

{ STRJOIN (register, string) }
Procedure op_STRJOIN(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''strjoin'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { STRJOIN (register, string) }
   Case reg.Typ of
    mvString: Regs.s[reg.RegIndex] += getString(param); { string }

    else
     raise eInvalidOpcode.Create('''strjoin'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { STRJOIN (stackval, string) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvString: Value.Str := CopyStringToPChar(AnsiString(Value.Str)+getString(param));

     else
      raise eInvalidOpcode.Create('''strjoin'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ NOT (register) }
Procedure op_NOT(VM: PVM);
Var reg: TMixedValue;
Begin
 With VM^ do
 Begin
  reg := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''not'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { NOT (register) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := not Regs.b[reg.RegIndex]; { bool }
    mvInt : Regs.i[reg.RegIndex] := not Regs.i[reg.RegIndex]; { int }

    else
     raise eInvalidOpcode.Create('''not'' called with argument: '+getTypeName(reg));
   End;
  End Else
  Begin
   { NOT (stackval) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := not Value.Bool; { bool }
     mvInt : Value.Int  := not Value.Int; { int }

     else
      raise eInvalidOpcode.Create('''not'' called with argument: '+getTypeName(reg));
    End;
  End;
 End;
End;

{ OR (register, value) }
Procedure op_OR(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''or'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { OR (register, value) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := Regs.b[reg.RegIndex] or getBool(param); { bool }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] or getInt(param); { int }

    else
     raise eInvalidOpcode.Create('''or'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { OR (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := Value.Bool or getBool(param); { bool }
     mvInt : Value.Int  := Value.Int or getInt(param); { int }

     else
      raise eInvalidOpcode.Create('''or'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ XOR (register, value) }
Procedure op_XOR(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''xor'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { XOR (register, value) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := Regs.b[reg.RegIndex] xor getBool(param); { bool }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] xor getInt(param); { int }

    else
     raise eInvalidOpcode.Create('''xor'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { XOR (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := Value.Bool xor getBool(param); { bool }
     mvInt : Value.Int  := Value.Int xor getInt(param); { int }

     else
      raise eInvalidOpcode.Create('''xor'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ AND (register, value) }
Procedure op_AND(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''and'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { AND (register, value) }
   Case reg.Typ of
    mvBool: Regs.b[reg.RegIndex] := Regs.b[reg.RegIndex] and getBool(param); { bool }
    mvInt : Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] and getInt(param); { int }

    else
     raise eInvalidOpcode.Create('''and'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { AND (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvBool: Value.Bool := Value.Bool and getBool(param); { bool }
     mvInt : Value.Int  := Value.Int and getInt(param); { int }

     else
      raise eInvalidOpcode.Create('''and'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ SHL (register, value) }
Procedure op_SHL(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''shl'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { SHL (register, value) }
   Case reg.Typ of
    mvInt: Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] shl getInt(param); { int }

    else
     raise eInvalidOpcode.Create('''shl'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { SHL (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvInt: Value.Int := Value.Int shl getInt(param); { int }

     else
      raise eInvalidOpcode.Create('''shl'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ SHR (register, value) }
Procedure op_SHR(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''shr'' requires the first parameter to be a register or a stackval.');

  if (reg.isReg) Then
  Begin
   { SHr (register, value) }
   Case reg.Typ of
    mvInt: Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] shr getInt(param); { int }

    else
     raise eInvalidOpcode.Create('''shr'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { Sr (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvInt: Value.Int := Value.Int shr getInt(param); { int }

     else
      raise eInvalidOpcode.Create('''shr'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ MOD (register, value) }
Procedure op_MOD(VM: PVM);
Var reg, param: TMixedValue;
Begin
 With VM^ do
 Begin
  reg   := read_param;
  param := read_param;

  if not (reg.isReg or reg.isStackval) Then
   raise eInvalidOpcode.Create('''mod'' requires the first parameter to be a register or a stackval.');

  // if (getInt(param) = 0) Then
  //  div_by_zero(); // @TODO

  if (reg.isReg) Then
  Begin
   { MOD (register, value) }
   Case reg.Typ of
    mvInt: Regs.i[reg.RegIndex] := Regs.i[reg.RegIndex] mod getInt(param); { int }

    else
     raise eInvalidOpcode.Create('''mod'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
   End;
  End Else
  Begin
   { MOD (stackval, value) }
   With reg.Stackval^ do
    Case reg.Typ of
     mvInt: Value.Int := Value.Int mod getInt(param); { int }

     else
      raise eInvalidOpcode.Create('''mod'' called with arguments: '+getTypeName(reg)+', '+getTypeName(param));
    End;
  End;
 End;
End;

{ ARSET (register, index count, value) }
Procedure op_ARSET(VM: PVM);
Var refreg, index_count, new_value: TMixedValue;
    PosArray                      : uint32Array;
    I                             : uint32;
Label Fail;
Begin
 With VM^ do
 Begin
  refreg      := read_param;
  index_count := read_param;
  new_value   := read_param;

  if not (refreg.isReg or refreg.isStackval) Then
   raise eInvalidOpcode.Create('''arset'' requires the first parameter to be a register or a stackval.');

  SetLength(PosArray, getInt(index_count));
  // if (Length(PosArray) = 0) Then raise Exception.Create(...);
  For I := 0 To High(PosArray) Do
   PosArray[I] := getInt(StackPop);

  if (refreg.isReg) Then
  Begin
   { ARSET (register, index count, value) }
   Case refreg.Typ of
    mvInt, mvReference: TMArray(CheckObject(getReference(refreg))).setValue(PosArray, new_value);

    mvString: // string reg
    Begin
     CheckStringBounds(VM, Regs.s[refreg.RegIndex], PosArray[0]);
     Regs.s[refreg.RegIndex][PosArray[0]] := getChar(new_value);
    End;

    mvChar: // char reg
    Begin
     CheckStringBounds(VM, ' ', PosArray[0]);
     Regs.c[refreg.RegIndex] := getChar(new_value);
    End;

    else
     goto Fail;
   End;
  End Else
  Begin
   { ARSET (stackval, index count, value) }
   With refreg.Stackval^ do
    Case refreg.Typ of
      mvInt, mvReference: TMArray(CheckObject(Pointer(Int32(Value.Int)))).setValue(PosArray, new_value);

      mvString:
      Begin
       CheckStringBounds(VM, Value.Str, PosArray[0]);
       Value.Str[PosArray[0]-1] := getChar(new_value); // `-1`, because we have `PChar` (which is counted from zero), not `AnsiString`.
      End;

      mvChar: // char stackval
      Begin
       CheckStringBounds(VM, ' ', PosArray[0]);
       Value.Char := getChar(new_value);
      End;

      else
       goto Fail;
     End;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''arset'' called with arguments: '+getTypeName(refreg)+', '+getTypeName(index_count)+', '+getTypeName(new_value));
 End;
End;

{ ARGET (register, index count, result register) }
Procedure op_ARGET(VM: PVM);
Var refreg, index_count, out_reg: TMixedValue;
    PosArray                    : uint32Array;
    I                           : uint32;
    AValue                      : TMixedValue;
Label Fail;
Begin
 With VM^ do
 Begin
  refreg      := read_param;
  index_count := read_param;
  out_reg     := read_param;

  SetLength(PosArray, getInt(index_count));
  For I := 0 To High(PosArray) Do
   PosArray[I] := getInt(StackPop);

  if (not (refreg.isReg or refreg.isStackval)) or (not (out_reg.isReg or out_reg.isStackval)) Then
   raise eInvalidOpcode.Create('''arget'' requires the first and third parameter to be a register or a stackval.');

  if (refreg.isReg) Then
  Begin
   { ARGET (register, index count, result register) }
   Case refreg.Typ of
    mvInt, mvReference: AValue := TMArray(CheckObject(getReference(refreg))).getValue(PosArray); { int, reference }

    mvString: { string }
    Begin
     AValue.Typ        := mvChar;
     AValue.Value.Char := getString(refreg)[PosArray[0]-1]; // `-1`, because we have `PChar` (which is counted from zero), not `AnsiString`.
    End;

    mvChar: // char
    Begin
     AValue.Typ        := mvChar;
     AValue.Value.Char := getChar(refreg);
    End;

    else
     goto Fail;
   End;
  End Else
  Begin
   { ARGET (stackval, index count, result register) }
   With refreg.Stackval^ do
    Case refreg.Typ of
     mvInt, mvReference: AValue := TMArray(CheckObject(Pointer(Int32(Value.Int)))).getValue(PosArray);

     mvString: // string stackval
     Begin
      CheckStringBounds(VM, Value.Str, PosArray[0]);

      AValue.Typ        := mvChar;
      AValue.Value.Char := Value.Str[PosArray[0]-1];
     End;

     mvChar: // char stackval
     Begin
      CheckStringBounds(VM, ' ', PosArray[0]);

      AValue.Typ        := mvChar;
      AValue.Value.Char := Value.Char;
     End;

     else
      goto Fail;
    End;
  End;

  Case out_reg.Typ of
   mvBool     : Regs.b[out_reg.RegIndex] := getBool(AValue);
   mvChar     : Regs.c[out_reg.RegIndex] := getChar(AValue);
   mvInt      : Regs.i[out_reg.RegIndex] := getInt(AValue);
   mvFloat    : Regs.f[out_reg.RegIndex] := getFloat(AValue);
   mvString   : Regs.s[out_reg.RegIndex] := getString(AValue);
   mvReference: Regs.r[out_reg.RegIndex] := getReference(AValue);

   else
    goto Fail;
  End;

  Exit;

 Fail:
  raise eInvalidOpcode.Create('''arget'' called with arguments: '+getTypeName(refreg)+', '+getTypeName(index_count)+', '+getTypeName(out_reg));
 End;
End;

{ ARCRT (register, array type, dimensions count) }
Procedure op_ARCRT(VM: PVM);
Var refreg, typ, dimcount: TMixedValue;
    ArrayObj             : TMArray;
    Sizes                : uint32Array;
    I                    : uint32;
Begin
 With VM^ do
 Begin
  refreg   := read_param;
  typ      := read_param;
  dimcount := read_param;

  if not (refreg.isReg) Then
   raise eInvalidOpcode.Create('''arcrt'' requires the first parameter to be a register.');

  SetLength(Sizes, getInt(dimcount));
  For I := 0 To High(Sizes) Do
   Sizes[I] := getInt(StackPop);

  ArrayObj := TMArray.Create(VM, getInt(typ), Sizes);

  Case refreg.Typ of
   mvInt      : Regs.i[refreg.RegIndex] := uint32(Pointer(ArrayObj));
   mvReference: Regs.r[refreg.RegIndex] := ArrayObj;

   else
    raise eInvalidOpcode.Create('''arcrt'' called with arguments: '+getTypeName(refreg)+', '+getTypeName(typ)+', '+getTypeName(dimcount));
  End;
 End;
End;

{ ARLEN (register, dimension ID, result register) }
Procedure op_ARLEN(VM: PVM);
Var refreg, dimension, out_reg: TMixedValue;
Begin
 With VM^ do
 Begin
  refreg    := read_param;
  dimension := read_param;
  out_reg   := read_param;

  if not (refreg.isReg or refreg.isStackval) Then
   raise eInvalidOpcode.Create('''arlen'' requires the first parameter to be a register or a stackval.');

  if not ((out_reg.isReg) and (out_reg.Typ = mvInt)) Then
   raise eInvalidOpcode.Create('''arlen'' requires the third parameter to be an int register.');

  Regs.i[out_reg.RegIndex] := TMArray(CheckObject(getReference(refreg))).getSize(getInt(dimension));
 End;
End;

{ STRLEN (string register, out int register) }
Procedure op_STRLEN(VM: PVM);
Var strreg, outreg: TMixedValue;
Begin
 With VM^ do
 Begin
  strreg := read_param;
  outreg := read_param;

  if (not (strreg.isReg or strreg.isStackval or (strreg.Typ = mvString))) Then
   raise eInvalidOpcode.Create('''strlen'' requires the first parameter to be a register, stackval or string.');

  if (not (outreg.isReg and (outreg.Typ = mvInt))) Then
   raise eInvalidOpcode.Create('''strlen'' requires the second parameter to be an int register.');

  Regs.i[outreg.RegIndex] := Length(getString(strreg));
 End;
End;

{ LOCATION }
Procedure op_LOCATION(VM: PVM);
Begin
 VM^.read_param; // do nothing, just skip the parameter
End;
End.
