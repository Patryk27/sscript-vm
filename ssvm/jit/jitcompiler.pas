(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MACRO ON}
Unit JITCompiler;

 Interface
 Uses VMStruct, VMTypes, VMICall, BCReader, JITOpcodes, JITOpcodeList, JITAbstractCPU, JITCPU, JITJumpTable, Opcodes;

 { TJumpToResolve }
 Type TJumpToResolve =
      Record
       JITOpcodeIndex     : uint32;
       AbsoluteJumpAddress: uint32;
      End;

 { TJumpsToResolveArray }
 Type TJumpsToResolveArray = Array of TJumpToResolve;

 { TJITCompiler }
 Type TJITCompiler =
      Class
       Private
        VM            : PVM; // virtual machine instance
        CPU           : TJITAbstractCPU;
        OpcodeList    : TJITOpcodeList;
        JumpTable     : TJITJumpTable;
        JumpsToResolve: TJumpsToResolveArray;

       Private
        Function getRegisterAddress(const Arg: TOpcodeArg): uint64;
        Procedure PutOpcode(const Kind: TJITOpcodeKind; const ArgTypes: Array of TJITOpcodeArgKind; const Args: Array of Variant);

        Procedure ResolveJITJumps;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile: Pointer;
       End;

 Implementation
Uses Variants, SysUtils;

(* TJITCompiler.getRegisterAddress *)
Function TJITCompiler.getRegisterAddress(const Arg: TOpcodeArg): uint64;
Begin
 Result := 0;

 Case Arg.ArgType of
  ptBoolReg     : Result := uint32(@VM^.Regs.b[Arg.RegID]);
  ptCharReg     : Result := uint32(@VM^.Regs.c[Arg.RegID]);
  ptIntReg      : Result := uint32(@VM^.Regs.i[Arg.RegID]);
  ptFloatReg    : Result := uint32(@VM^.Regs.f[Arg.RegID]);
  ptStringReg   : Result := uint32(@VM^.Regs.s[Arg.RegID]);
  ptReferenceReg: Result := uint32(@VM^.Regs.r[Arg.RegID]);

  else
   raise Exception.CreateFmt('TJITCompiler.getRegisterAddress() called with an invalid (non-register) argument of type %d', [ord(Arg.ArgType)]);
 End;
End;

(* TJITCompiler.PutOpcode *)
Procedure TJITCompiler.PutOpcode(const Kind: TJITOpcodeKind; const ArgTypes: Array of TJITOpcodeArgKind; const Args: Array of Variant);
Var Opcode : TJITOpcode;
    Arg    : TJITOpcodeArg;
    I      : uint8;
Begin
 if (Length(ArgTypes) <> Length(Args)) or (Length(Args) > High(TJITOpcode.Args)) or (Length(Args) <> JITOpcodeParamCount[Kind]) Then // lenghts of the arrays are not the same (or too high)
  raise Exception.CreateFmt('TJITCompiler.PutOpcode() -> shouldn''t happen! (%d, %d, %d, %d)', [Length(ArgTypes), Length(Args), High(TJITOpcode.Args), JITOpcodeParamCount[Kind]]);

 Opcode.Kind := Kind;

 if (Length(Args) > 0) Then
 Begin
  For I := Low(Args) To High(Args) Do // iterate each argument
  Begin
   Arg.Kind := ArgTypes[I];

   Case Arg.Kind of
    joa_register: Arg.RegisterID  := StrToInt(VarToStr(Args[I]));
    joa_memory  : Arg.MemoryAddr  := VMReference(uint64(StrToInt64(VarToStr(Args[I]))));
    joa_constant: Arg.Constant    := Args[I];
    joa_stackval: Arg.StackvalPos := StrToInt(VarToStr(Args[I]));

    else
     raise Exception.CreateFmt('TJITCompiler.PutOpcode() -> unknown (invalid) argument type (arg #%d): #%d', [I, ord(Arg.Kind)]);
   End;

   Opcode.Args[I] := Arg;
  End;
 End;

 OpcodeList.Append(Opcode); // append opcode
End;

(* TJITCompiler.ResolveJITJumps *)
Procedure TJITCompiler.ResolveJITJumps;
Var Jump   : TJumpToResolve;
    JumpRec: TJITJumpRecord;
    Opcode : TJITOpcode;
Begin
 For Jump in JumpsToResolve Do
 Begin
  if (not JumpTable.FindJumpByBytecodeAddress(Jump.AbsoluteJumpAddress, JumpRec)) Then
   raise Exception.Create('Unexpected state: invalid jump!'); // @TODO: this should be a bit nicer message imho

  {$DEFINE Op := OpcodeList[Jump.JITOpcodeIndex]}
  Opcode                  := Op;
  Opcode.Args[0].Constant := JumpRec.CodeAddress;
  Op                      := Opcode;
  {$UNDEF Op}
 End;
End;

(* TJITCompiler.Create *)
Constructor TJITCompiler.Create(const fVM: PVM);
Begin
 VM        := fVM;
 CPU       := TJITCPU.Create(VM);
 JumpTable := TJITJumpTable.Create;
End;

(* TJITCompiler.Destroy *)
Destructor TJITCompiler.Destroy;
Begin
 CPU.Free;
 JumpTable.Free;

 inherited Destroy;
End;

(* TJITCompiler.Compile *)
Function TJITCompiler.Compile: Pointer;
Var Reader   : TBytecodeReader;
    Opcode   : TOpcode_E;
    Args     : TOpcodeArgArray;
    OpcodePos: uint32;

    JITOpcode         : TJITOpcodeKind;
    Arg0, Arg1        : Variant;
    Arg0Kind, Arg1Kind: TJITOpcodeArgKind;

    icall: PInternalCall;

  { InvalidOpcodeException }
  Procedure InvalidOpcodeException; inline;
  Begin
   raise Exception.CreateFmt('JIT: invalid opcode: [0x%x] %s', [OpcodePos, Reader.OpcodeToString(Opcode, Args)]);
  End;

  { ParseArgument }
  Procedure ParseArgument(out ArgKind: TJITOpcodeArgKind; out Arg: Variant; const OpArg: TOpcodeArg);
  Begin
   // imm bool
   if (OpArg.ArgType = ptBool) Then
   Begin
    Arg     := OpArg.ImmBool;
    ArgKind := joa_constant;
   End Else

   // imm char
   if (OpArg.ArgType = ptChar) Then
   Begin
    Arg     := OpArg.ImmChar;
    ArgKind := joa_constant;
   End Else

   // imm int
   if (OpArg.ArgType = ptInt) Then
   Begin
    Arg     := OpArg.ImmInt;
    ArgKind := joa_constant;
   End Else

   // imm float
   if (OpArg.ArgType = ptFloat) Then
   Begin
    if (CPU.AllocateFloatConstants) Then
    Begin
     Arg     := VMIReference(CPU.AllocateFloat(OpArg.ImmFloat));
     ArgKind := joa_memory;
    End Else
    Begin
     Arg     := OpArg.ImmFloat;
     ArgKind := joa_constant;
    End;
   End Else

   // imm string
   if (OpArg.ArgType = ptString) Then
   Begin
    if (CPU.AllocateStringConstants) Then
    Begin
     Arg     := VMIReference(CPU.AllocateString(OpArg.ImmString));
     ArgKind := joa_memory;
    End Else
    Begin
     Arg     := OpArg.ImmString;
     ArgKind := joa_constant;
    End;
   End Else

   // reg bool/char/int/float/string/reference
   if (OpArg.ArgType in [ptBoolReg..ptReferenceReg]) Then
   Begin
    if (CPU.isRegNative(OpArg)) Then
    Begin
     Arg     := OpArg.RegID;
     ArgKind := joa_register;
    End Else
    Begin
     Arg     := getRegisterAddress(OpArg);
     ArgKind := joa_memory;
    End;
   End Else

   // stackval
   if (OpArg.ArgType = ptStackval) Then
   Begin
    Arg     := OpArg.StackvalPos;
    ArgKind := joa_stackval;
   End Else

    VM^.ThrowException('TJITCompiler.Compile::ParseArgument() called with invalid opcode argument type #%d', [ord(OpArg.ArgType)]);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg0: TOpcodeArgType): Boolean;
  Begin
   Result := (Args[0].ArgType = Arg0);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg0, Arg1: TOpcodeArgType): Boolean;
  Begin
   Result := (Args[0].ArgType = Arg0) and (Args[1].ArgType = Arg1);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg0, Arg1: Array of TOpcodeArgType): Boolean;
  Var Arg   : TOpcodeArgType;
      A0, A1: Boolean;
  Begin
   A0 := False;
   A1 := False;

   For Arg in Arg0 Do
    A0 := A0 or (Args[0].ArgType = Arg);

   For Arg in Arg1 Do
    A1 := A1 or (Args[1].ArgType = Arg);

   Result := (A0 and A1);
  End;

Begin
 Result := nil;

 OpcodeList := TJITOpcodeList.Create;
 Reader     := TBytecodeReader.Create(VM^.LoaderData.CodeData);

 Try
  (* Stage 1: bytecode -> JIT bytecode *)
  While (Reader.AnyOpcodeLeft) Do
  Begin
   OpcodePos := Reader.getBytecodeData.Position;
   Reader.FetchOpcode(Opcode, Args);

   JumpTable.AddJump(OpcodePos, OpcodeList.getSize);

   if (Length(Args) > 0) Then
    ParseArgument(Arg0Kind, Arg0, Args[0]);

   if (Length(Args) > 1) Then
    ParseArgument(Arg1Kind, Arg1, Args[1]);

   if (Length(Args) > 2) Then
    raise Exception.Create('@TODO: Length(Args) > 2');

   { compile code to the JIT microcode }
   Case Opcode of
    { nop }
    o_nop: ;

    { stop }
    o_stop:
    Begin
     PutOpcode(jo_stop, [], []);
    End;

    { push }
    o_push:
    Begin
     // push(reg/imm bool)
     if (Args[0].ArgType in [ptBool, ptBoolReg]) Then
     Begin
      JITOpcode := jo_bpush;
     End Else

     // push(reg/imm char)
     if (Args[0].ArgType in [ptChar, ptCharReg]) Then
     Begin
      JITOpcode := jo_cpush;
     End Else

     // push(reg/imm int)
     if (Args[0].ArgType in [ptInt, ptIntReg]) Then
     Begin
      JITOpcode := jo_ipush;
     End Else

     // push(reg/imm float)
     if (Args[0].ArgType in [ptFloat, ptFloatReg]) Then
     Begin
      JITOpcode := jo_fpush;
     End Else

     // push(reg/imm string)
     if (Args[0].ArgType in [ptString, ptStringReg]) Then
     Begin
      JITOpcode := jo_spush;
     End Else

     {// push(reg reference)
     if (CheckArgs(ptReferenceReg)) Then
     Begin @TODO?
      JITOpcode := jo_rpush;
     End Else}

     // push(stackval)
     if (CheckArgs(ptStackval)) Then
     Begin
      JITOpcode := jo_vpush;
     End Else

      InvalidOpcodeException;

     PutOpcode(JITOpcode, [Arg0Kind], [Arg0]);
    End;

    { pop }
    o_pop:
    Begin
     // pop(reg bool/char/int/float/string/reference)
     if (Args[0].ArgType in [ptBoolReg..ptReferenceReg]) Then
     Begin
      JITOpcode := TJITOpcodeKind(ord(jo_bpop) + ord(Args[0].ArgType) - ord(ptBoolReg));

      PutOpcode(JITOpcode, [Arg0Kind], [Arg0]);
     End Else

      InvalidOpcodeException;
    End;

    { add, sub, mul, div, mod }
    o_add, o_sub, o_mul, o_div, o_mod:
    Begin
     if (Args[0].ArgType = ptStackval) and (Args[1].ArgType = ptStackval) Then // @TODO
      InvalidOpcodeException;

     // op(reg char | stackval, reg/imm char | stackval)
     if (CheckArgs([ptCharReg, ptStackval], [ptCharReg, ptChar, ptStackval])) Then
     Begin
      if (Opcode = o_mod) Then
       JITOpcode := jo_ccmod Else
       JITOpcode := TJITOpcodeKind(ord(jo_ccadd) + ord(Opcode)-ord(o_add));
     End Else

     // op(reg int | stackval, reg/imm int | stackval)
     if (CheckArgs([ptIntReg, ptStackval], [ptIntReg, ptInt, ptStackval])) Then
     Begin
      if (Opcode = o_mod) Then // special case - see opcode list
       JITOpcode := jo_iimod Else
       JITOpcode := TJITOpcodeKind(ord(jo_iiadd) + ord(Opcode)-ord(o_add));
     End Else

     // op(reg float | stackval, reg/imm float | imm int | stackval)
     if (CheckArgs([ptFloatReg, ptStackval], [ptFloatReg, ptFloat, ptInt, ptStackval])) Then
     Begin
      if (Opcode = o_mod) Then
       InvalidOpcodeException { no "mod" operation for floating point types } Else
       JITOpcode := TJITOpcodeKind(ord(jo_ffadd) + ord(Opcode)-ord(o_add));
     End Else

     // op(reg float, reg/imm int)
     if (CheckArgs([ptFloatReg], [ptIntReg, ptInt])) Then
     Begin
      if (Opcode = o_mod) Then
       InvalidOpcodeException Else
       JITOpcode := TJITOpcodeKind(ord(jo_fiadd) + ord(Opcode)-ord(o_add));
     End Else

     // op(reg int, reg/imm float)
     if (CheckArgs([ptIntReg], [ptFloatReg, ptFloat])) Then
     Begin
      if (Opcode = o_mod) Then
       InvalidOpcodeException Else
       JITOpcode := TJITOpcodeKind(ord(jo_ifadd) + ord(Opcode)-ord(o_add));
     End Else

      InvalidOpcodeException;

     PutOpcode(JITOpcode, [Arg0Kind, Arg1Kind], [Arg0, Arg1]);
    End;

    { mov }
    o_mov:
    Begin
     // mov(reg bool, reg/imm bool | stackval)
     if (Args[0].ArgType = ptBoolReg) and (Args[1].ArgType in [ptBoolReg, ptBool, ptStackval]) Then
     Begin
      JITOpcode := jo_bbmov;
     End Else

     // mov(reg char, reg/imm char | stackval)
     if (Args[0].ArgType = ptCharReg) and (Args[1].ArgType in [ptCharReg, ptChar, ptStackval]) Then
     Begin
      JITOpcode := jo_ccmov;
     End Else

     // mov(reg char, reg/imm int)
     if (Args[0].ArgType = ptCharReg) and (Args[1].ArgType in [ptIntReg, ptInt]) Then
     Begin
      JITOpcode := jo_cimov;
     End Else

     // mov(reg int, reg/imm int | stackval)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType in [ptIntReg, ptInt, ptStackval]) Then
     Begin
      JITOpcode := jo_iimov;
     End Else

     // mov(reg int, reg/imm char)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType in [ptCharReg, ptChar]) Then
     Begin
      JITOpcode := jo_icmov;
     End Else

     // mov(reg float, reg/imm float | imm int | stackval)
     if (Args[0].ArgType = ptFloatReg) and (Args[1].ArgType in [ptFloatReg, ptFloat, ptInt, ptStackval]) Then
     Begin
      JITOpcode := jo_ffmov;
     End Else

     // mov(reg string, reg/imm string | stackval)
     if (Args[0].ArgType = ptStringReg) and (Args[1].ArgType in [ptStringReg, ptString, ptStackval]) Then
     Begin
      JITOpcode := jo_ssmov;
     End Else

     // mov(stackval, imm/reg bool/char/int/float/string/reference)
     if (Args[0].ArgType = ptStackval) Then
     Begin
      Case Args[1].ArgType of
       ptBool, ptBoolReg    : JITOpcode := jo_bbmov;
       ptChar, ptCharReg    : JITOpcode := jo_ccmov;
       ptInt, ptIntReg      : JITOpcode := jo_iimov;
       ptFloat, ptFloatReg  : JITOpcode := jo_ffmov;
       ptString, ptStringReg: JITOpcode := jo_ssmov;
       ptReferenceReg       : JITOpcode := jo_rrmov;

       else
        InvalidOpcodeException;
      End;
     End Else

      InvalidOpcodeException;

     PutOpcode(JITOpcode, [Arg0Kind, Arg1Kind], [Arg0, Arg1]);
    End;

    { jmp, tjmp, fjmp, call }
    o_jmp, o_tjmp, o_fjmp, o_call:
    Begin
     if (Args[0].ArgType <> ptInt) Then // jumps and calls have to be constant
      InvalidOpcodeException;

     Case Opcode of
      o_jmp : JITOpcode := jo_jmp;
      o_tjmp: JITOpcode := jo_tjmp;
      o_fjmp: JITOpcode := jo_fjmp;
      o_call: JITOpcode := jo_call;
     End;

     {$DEFINE JTR := JumpsToResolve}
     {$DEFINE Last := JTR[High(JTR)]}
     SetLength(JTR, Length(JTR)+1);
     Last.JITOpcodeIndex      := OpcodeList.getSize;
     Last.AbsoluteJumpAddress := OpcodePos + Args[0].ImmInt;
     {$UNDEF JTR}
     {$UNDEF Last}

     PutOpcode(JITOpcode, [joa_constant], [0]);
    End;

    { icall }
    o_icall:
    Begin
     // icall(imm string)
     if (CheckArgs(ptString)) Then
     Begin
      icall := VM^.FindInternalCall(Args[0].ImmString);

      if (icall = nil) Then
       raise Exception.CreateFmt('Unknown (unregistered) internal call: %s', [Args[0].ImmString]);

      PutOpcode(jo_icall,
               [joa_memory],
               [uint32(icall)]);
     End Else

      InvalidOpcodeException;
    End;

    { ret }
    o_ret:
    Begin
     PutOpcode(jo_ret, [], []);
    End;

    { if_* }
    o_if_e, o_if_ne, o_if_g, o_if_l, o_if_ge, o_if_le:
    Begin
     if (Args[0].ArgType in [ptInt, ptIntReg]) and
        (Args[1].ArgType in [ptInt, ptIntReg]) Then
     Begin
      JITOpcode := TJITOpcodeKind(ord(jo_iicmpe) + ord(Opcode) - ord(o_if_e));
     End Else
      InvalidOpcodeException;

     PutOpcode(JITOpcode, [Arg0Kind, Arg1Kind], [Arg0, Arg1]);
    End;

    else
     InvalidOpcodeException;
   End;
  End;

  (* Stage 2: resolve JIT jumps *)
  ResolveJITJumps;

  (* Stage 3: JIT bytecode -> CPU code *)
  Result := CPU.Compile(OpcodeList);

  if (Result = nil) Then
   raise Exception.Create('No JIT code has been generated!');
 Finally
 // OpcodeList.Dump; // dump opcode list (debug only)

  Reader.Free;
  OpcodeList.Free;
 End;
End;
End.
