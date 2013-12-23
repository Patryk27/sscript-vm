(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MACRO ON}
Unit JITCompiler;

 Interface
 Uses VM, VMTypes, BCReader, Opcodes, JITOpcodes, JITOpcodeList, JITAbstractCPU, JITCPU, JITJumpTable;

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
        Function AllocateString(const Value: String): uint64;

       Private
        Function getRegisterAddress(const Arg: TOpcodeArg): uint64;
        Procedure PutOpcode(const ID: TJITOpcodeKind; const ArgTypes: Array of TJITOpcodeArgKind; const Args: Array of Variant);

        Procedure ResolveJITJumps;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile: Pointer;
       End;

 Implementation
Uses Variants, SysUtils;

(* TJITCompiler.AllocateString *)
Function TJITCompiler.AllocateString(const Value: String): uint64;
Var I, Len: uint32;
Begin
 Result := uint64(CPU.JITMemAlloc(Length(Value)+1));

 Len := Length(Value);

 For I := 1 To Len Do
  PChar(Result + I-1)^ := Value[I];

 PChar(Result + Len)^ := #0;
End;

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
Procedure TJITCompiler.PutOpcode(const ID: TJITOpcodeKind; const ArgTypes: Array of TJITOpcodeArgKind; const Args: Array of Variant);
Var Opcode : TJITOpcode;
    Arg    : TJITOpcodeArg;
    I      : uint8;
Begin
 if (Length(ArgTypes) <> Length(Args)) or (Length(Args) > High(TJITOpcode.Args)) or (Length(Args) <> JITOpcodeParamCount[ID]) Then // lenghts of the arrays are not the same (or too high)
  raise Exception.CreateFmt('TJITCompiler.PutOpcode() -> shouldn''t happen! (Length(ArgTypes)=%d, Length(Args)=%d, High(TJITOpcode.Args)=%d, JITOpcodeParamCount[ID]=%d)', [Length(ArgTypes), Length(Args), High(TJITOpcode.Args), JITOpcodeParamCount[ID]]);

 Opcode.ID := ID;

 if (Length(Args) > 0) Then
 Begin
  For I := Low(Args) To High(Args) Do // iterate each argument
  Begin
   Arg.Kind := ArgTypes[I];

   Case Arg.Kind of
    joa_register: Arg.RegisterID := StrToInt(VarToStr(Args[I]));
    joa_memory  : Arg.MemoryAddr := VMReference(uint64(StrToInt64(VarToStr(Args[I]))));
    joa_constant: Arg.Constant := Args[I];

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
    RegAddr  : uint32;

    JITOpcode         : TJITOpcodeKind;
    Arg0, Arg1        : Variant;
    Arg0Kind, Arg1Kind: TJITOpcodeArgKind;

    icall: PCall;

  { InvalidOpcodeException }
  Procedure InvalidOpcodeException; inline;
  Begin
   raise Exception.CreateFmt('Invalid opcode: [0x%x] %s', [OpcodePos, Reader.OpcodeToString(Opcode, Args)]);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg1: TOpcodeArgType): Boolean; inline;
  Begin
   Result := (Args[0].ArgType = Arg1);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg1, Arg2: TOpcodeArgType): Boolean; inline;
  Begin
   Result := (Args[0].ArgType = Arg1) and (Args[1].ArgType = Arg2);
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

   if (isLocationOpcode(Opcode)) Then // skip the location opcodes
    Continue;

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
     // push(const string)
     if (CheckArgs(ptString)) Then
     Begin
      PutOpcode(jo_spush,
               [joa_memory],
               [AllocateString(Args[0].ImmString)]);
     End Else

     // push(reg int)
     if (CheckArgs(ptIntReg)) Then
     Begin
      if (CPU.hasNativeReg(Args[0])) Then
      Begin
       PutOpcode(jo_ipush, // ipush(reg)
                [joa_register],
                [Args[0].RegID]);
      End Else
      Begin
       PutOpcode(jo_ipush, // ipush(mem)
                [joa_memory],
                [getRegisterAddress(Args[0])]);
      End;
     End Else

      InvalidOpcodeException;
    End;

    { add, sub, mul, div, mod }
    o_add, o_sub, o_mul, o_div, o_mod:
    Begin
     // op(reg int, reg/imm int)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType in [ptIntReg, ptInt]) Then
     Begin
      if (Opcode = o_mod) Then
       JITOpcode := jo_iimod Else
       JITOpcode := TJITOpcodeKind(ord(jo_iiadd) + ord(Opcode)-ord(o_add));

      // arg0
      if (CPU.hasNativeReg(Args[0])) Then
      Begin
       Arg0     := Args[0].RegID;
       Arg0Kind := joa_register;
      End Else
      Begin
       Arg0     := getRegisterAddress(Args[0]);
       Arg0Kind := joa_memory;
      End;

      // arg1
      if (Args[1].ArgType = ptIntReg) Then
      Begin
       if (CPU.hasNativeReg(Args[1])) Then
       Begin
        Arg1     := Args[1].RegID;
        Arg1Kind := joa_register;
       End Else
       Begin
        Arg1     := getRegisterAddress(Args[1]);
        Arg1Kind := joa_memory;
       End;
      End Else
      Begin
       Arg1     := Args[1].ImmInt;
       Arg1Kind := joa_constant;
      End;

      // append opcode
      PutOpcode(JITOpcode, [Arg0Kind, Arg1Kind], [Arg0, Arg1]);
     End Else

      InvalidOpcodeException;
    End;

    { mov }
    o_mov:
    Begin
     RegAddr := getRegisterAddress(Args[0]);

     // mov(reg int, imm int)
     if (CheckArgs(ptIntReg, ptInt)) Then
     Begin
      if (CPU.hasNativeReg(Args[0])) Then // if register can be used natively
      Begin
       PutOpcode(jo_iimov, // iimov(reg, value)
                [joa_register, joa_constant],
                [Args[0].RegID, Args[1].ImmInt]);
      End Else
      Begin
       PutOpcode(jo_iimov, // iimov(mem, value)
                [joa_memory, joa_constant],
                [RegAddr, Args[1].ImmInt]);
      End;
     End Else

      InvalidOpcodeException;
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

      PutOpcode(jo_icall, // icall(mem)
               [joa_memory],
               [uint32(icall)]);
     End Else

      InvalidOpcodeException;
    End;

    { ret }
    o_ret:
    Begin
     PutOpcode(jo_ret, [], []); // ret()
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
