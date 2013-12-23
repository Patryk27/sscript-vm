(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JITCompiler;

 Interface
 Uses VM, VMTypes, BCReader, Opcodes, JITOpcodes, JITOpcodeList, JITAbstractCPU, JITCPU;

 { TJITCompiler }
 Type TJITCompiler =
      Class
       Private
        VM        : PVM; // virtual machine instance
        CPU       : TJITAbstractCPU;
        OpcodeList: TJITOpcodeList;

       Private
        Function getRegisterAddress(const Arg: TOpcodeArg): uint64;

        Procedure PutOpcode(const ID: TJITOpcodeKind; const ArgTypes: Array of TJITOpcodeArgKind; const Args: Array of Variant);

       Public
        Constructor Create(const fVM: PVM);
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

(* TJITCompiler.Create *)
Constructor TJITCompiler.Create(const fVM: PVM);
Begin
 VM  := fVM;
 CPU := TJITCPU.Create(VM);
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

    else
     InvalidOpcodeException;
   End;
  End;

  (* Stage 2: JIT bytecode -> CPU code *)
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
