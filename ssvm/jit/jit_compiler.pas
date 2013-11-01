(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)

{$I jit_cpu.inc}

Unit JIT_Compiler;

 Interface
 Uses VM, Stack, BCReader, Opcodes, JIT_Abstract_CPU, JIT_Jump_Table,

 {$I jit_cpu.inc}

 {$IF CPU = CPU_x86}
  CPU_x86
 {$ENDIF};

 { TJITCompiler }
 Type TJITCompiler =
      Class
       Private
        VM       : PVM;
        CPU      : TJITAbstractCPU;
        JumpTable: TJumpTable;

       Private
        Function getRegisterAddress(const Arg: TOpcodeArg): uint64;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile: TJITCompiledState;

        Property getCPU: TJITAbstractCPU read CPU;
       End;

 Implementation
Uses SysUtils, TypInfo, Stream;

(* jit_debug *)
Procedure jit_debug(const Text: String);
Begin
 {$IFDEF JIT_CONSOLE_LOG}
  Writeln('[JIT debug] ', Text);
 {$ENDIF}
End;

(* jit_debug *)
Procedure jit_debug(const Format: String; const Args: Array of Const);
Begin
 jit_debug(SysUtils.Format(Format, Args));
End;

// -------------------------------------------------------------------------- //
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

(* TJITCompiler.Create *)
Constructor TJITCompiler.Create(const fVM: PVM);
Begin
 VM := fVM;

 CPU       := TJITCPU.Create(VM);
 JumpTable := TJumpTable.Create;
End;

(* TJITCompiler.Destroy *)
Destructor TJITCompiler.Destroy;
Begin
 inherited Destroy;

 CPU.Free;
 JumpTable.Free;
End;

(* TJITCompiler.Compile *)
Function TJITCompiler.Compile: TJITCompiledState;
Const JUMP_MARKER: uint32 = $CAFEBABE;
Var Reader: TBytecodeReader;
    Opcode: TOpcode_E;
    Args  : TOpcodeArgArray;

    icall             : PCall;
    ResultMV, ParamsMV: PMixedValue;

    { InvalidOpcodeException }
    Procedure InvalidOpcodeException; // @TODO: separate this procedure to TBytecodeReader?
    Const BoolRegsNames: Array[1..5] of String = ('eb1', 'eb2', 'eb3', 'eb4', 'if');
          IntRegsNames : Array[1..5] of String = ('ei1', 'ei2', 'ei3', 'ei4', 'stp');
          BoolTable    : Array[Boolean] of String = ('false', 'true');
    Var Msg: String = '';
        I  : uint8;
    Begin
     jit_debug('InvalidOpcodeException()');

     Msg := 'Invalid opcode: [0x'+IntToHex(Reader.getBytecodeData.Position, 8)+'] '+Copy(GetEnumName(TypeInfo(Opcode), ord(Opcode)), 3, 50)+'(';

     For I := Low(Args) To High(Args) Do
     Begin
      Case Args[I].ArgType of
       ptBoolReg     : Msg += BoolRegsNames[Args[I].RegID];
       ptCharReg     : Msg += 'ec'+IntToStr(Args[I].RegID);
       ptIntReg      : Msg += IntRegsNames[Args[I].RegID];
       ptFloatReg    : Msg += 'ef'+IntToStr(Args[I].RegID);
       ptStringReg   : Msg += 'es'+IntToStr(Args[I].RegID);
       ptReferenceReg: Msg += 'er'+IntToStr(Args[I].RegID);

       ptBool  : Msg += BoolTable[Args[I].ImmBool];
       ptChar  : Msg += '#'+IntToStr(ord(Args[I].ImmChar));
       ptInt   : Msg += IntToStr(Args[I].ImmInt);
       ptFloat : Msg += FloatToStr(Args[I].ImmFloat);
       ptString: Msg += '"'+Args[I].ImmString+'"';

       ptStackval: Msg += '['+IntToStr(Args[I].StackvalPos)+']';

       ptConstantMemRef: Msg += '&'+IntToStr(Args[I].MemoryAddress);

       else
        raise Exception.CreateFmt('TJITCompiler.Compile::InvalidOpcodeException -> invalid opcode argument type: #%d', [ord(Args[I].ArgType)]);
      End;

      if (I < High(Args)) Then
       Msg += ', ';
     End;

     Msg += ')';

     raise Exception.Create(Msg);
    End;

    { UnknownOpcodeException }
    Procedure UnknownOpcodeException;
    Begin
     jit_debug('UnknownOpcodeException()');

     raise Exception.CreateFmt('Unknown opcode: 0x%x', [ord(Opcode)]);
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

Var ArithmeticOperation: TArithmeticOperation;
    BitwiseOperation   : TBitwiseOperation;
    CompareOperation   : TCompareOperation;

    CurrentBytecodePosition: uint32;

    CompiledData: TStream;

    JumpPos: int32;
    Jump   : PJumpRecord;
    I      : Integer;
Begin
 Result := csJITFailed;

 New(ResultMV);
 ParamsMV := AllocMem(256*sizeof(TMixedValue));

 Reader := TBytecodeReader.Create(VM^.LoaderData.CodeData);

 CPU.pre_compilation;

 CompiledData := CPU.getCompiledData;

 Try
  (* Phase 1: compilation *)
  jit_debug('Phase 1: compilation...');

  While (Reader.AnyOpcodeLeft) Do
  Begin
  // CPU.getCompiledData.write_uint8($CC); // int3

   CurrentBytecodePosition := uint32(Reader.getBytecodeData.Memory) + Reader.getBytecodeData.Position;

   JumpTable.AddJump(CurrentBytecodePosition, CPU.getCompiledData.Position);

   { fetch opcode and its arguments }
   Opcode := Reader.FetchOpcode;
   Args   := Reader.FetchArguments(Opcode);

   if (isLocationOpcode(Opcode)) Then // skip the location ("loc_*") opcodes
    Continue;

   { parse and compile it }
   Case Opcode of
    { nop }
    o_nop: ;

    { stop }
    o_stop:
    Begin
     CPU.do_stop;
    End;

    { push }
    o_push:
    Begin
     // push(imm bool)
     if (CheckArgs(ptBool)) Then
      CPU.bcpush_immbool(Args[0].ImmBool) Else

     // push(imm char)
     if (CheckArgs(ptChar)) Then
      CPU.bcpush_immchar(Args[0].ImmChar) Else

     // push(imm int)
     if (CheckArgs(ptInt)) Then
      CPU.bcpush_immint(Args[0].ImmInt) Else

     // push(imm float)
     if (CheckArgs(ptFloat)) Then
      CPU.bcpush_immfloat(Args[0].ImmFloat) Else

     // push(imm string)
     if (CheckArgs(ptString)) Then
      CPU.bcpush_immstring(Args[0].ImmString) Else

     // push(reg bool)
     if (CheckArgs(ptBoolReg)) Then
      CPU.bcpush_reg(reg_eb, getRegisterAddress(Args[0])) Else

     // push(reg char)
     if (CheckArgs(ptCharReg)) Then
      CPU.bcpush_reg(reg_ec, getRegisterAddress(Args[0])) Else

     // push(reg int)
     if (CheckArgs(ptIntReg)) Then
      CPU.bcpush_reg(reg_ei, getRegisterAddress(Args[0])) Else

     // push(reg float)
     if (CheckArgs(ptFloatReg)) Then
      CPU.bcpush_reg(reg_ef, getRegisterAddress(Args[0])) Else

     // push(reg string)
     if (CheckArgs(ptStringReg)) Then
      CPU.bcpush_reg(reg_es, getRegisterAddress(Args[0])) Else

     // push(reg reference)
     if (CheckArgs(ptReferenceReg)) Then
      CPU.bcpush_reg(reg_er, getRegisterAddress(Args[0])) Else

     // push(invalid)
      InvalidOpcodeException;
    End;

    { pop }
    o_pop:
    Begin
     // pop(reg bool)
     if (CheckArgs(ptBoolReg)) Then
      CPU.bcpop_reg(reg_eb, getRegisterAddress(Args[0])) Else

     // pop(reg char)
     if (CheckArgs(ptCharReg)) Then
      CPU.bcpop_reg(reg_ec, getRegisterAddress(Args[0])) Else

     // pop(reg int)
     if (CheckArgs(ptIntReg)) Then
      CPU.bcpop_reg(reg_ei, getRegisterAddress(Args[0])) Else

     // pop(reg float)
     if (CheckArgs(ptFloatReg)) Then
      CPU.bcpop_reg(reg_ef, getRegisterAddress(Args[0])) Else

     // pop(reg string)
     if (CheckArgs(ptStringReg)) Then
      CPU.bcpop_reg(reg_es, getRegisterAddress(Args[0])) Else

     // pop(reg reference)
     if (CheckArgs(ptReferenceReg)) Then
      CPU.bcpop_reg(reg_er, getRegisterAddress(Args[0])) Else

     // pop(invalid)
      InvalidOpcodeException;
    End;

    { add, sub, mul, div }
    o_add, o_sub, o_mul, o_div:
    Begin
     Case Opcode of
      o_add: ArithmeticOperation := ao_add;
      o_sub: ArithmeticOperation := ao_sub;
      o_mul: ArithmeticOperation := ao_mul;
      o_div: ArithmeticOperation := ao_div;
     End;

     // opcode(reg int, imm int)
     if (CheckArgs(ptIntReg, ptInt)) Then
      CPU.arithmetic_memint_immint(ArithmeticOperation, getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // opcode(reg int, reg int)
     if (CheckArgs(ptIntReg, ptIntReg)) Then
      CPU.arithmetic_memint_memint(ArithmeticOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(reg float, imm float)
     if (CheckArgs(ptFloatReg, ptFloat)) Then
      CPU.arithmetic_memfloat_immfloat(ArithmeticOperation, getRegisterAddress(Args[0]), Args[1].ImmFloat) Else

     // opcode(reg float, reg float)
     if (CheckArgs(ptFloatReg, ptFloatReg)) Then
      CPU.arithmetic_memfloat_memfloat(ArithmeticOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(reg float, imm int)
     if (CheckArgs(ptFloatReg, ptInt)) Then
      CPU.arithmetic_memfloat_immint(ArithmeticOperation, getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // opcode(reg float, reg int)
     if (CheckArgs(ptFloatReg, ptIntReg)) Then
      CPU.arithmetic_memfloat_memint(ArithmeticOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(stackval, imm int)
     if (CheckArgs(ptStackval, ptInt)) Then
      CPU.arithmetic_stackval_immint(ArithmeticOperation, Args[0].StackvalPos, Args[1].ImmInt) Else

     // opcode(stackval, reg int)
     if (CheckArgs(ptStackval, ptIntReg)) Then
      CPU.arithmetic_stackval_memint(ArithmeticOperation, Args[0].StackvalPos, getRegisterAddress(Args[1])) Else

     // opcode(stackval, imm float)
     if (CheckArgs(ptStackval, ptFloat)) Then
      CPU.arithmetic_stackval_immfloat(ArithmeticOperation, Args[0].StackvalPos, Args[1].ImmFloat) Else

     // opcode(stackval, reg float)
     if (CheckArgs(ptStackval, ptFloatReg)) Then
      CPU.arithmetic_stackval_memfloat(ArithmeticOperation, Args[0].StackvalPos, getRegisterAddress(Args[1])) Else

     // opcode(invalid)
      InvalidOpcodeException;
    End;

    { mov }
    o_mov:
    Begin
     // mov(reg bool, imm bool)
     if (CheckArgs(ptBoolReg, ptBool)) Then
      CPU.move_membool_immbool(getRegisterAddress(Args[0]), Args[1].ImmBool) Else

     // mov(reg bool, reg bool)
     if (CheckArgs(ptBoolReg, ptBoolReg)) Then
      CPU.move_membool_membool(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // mov(reg char, imm char)
     if (CheckArgs(ptCharReg, ptChar)) Then
      CPU.move_memchar_immchar(getRegisterAddress(Args[0]), Args[1].ImmChar) Else

     // mov(reg char, reg char)
     if (CheckArgs(ptCharReg, ptCharReg)) Then
      CPU.move_memchar_memchar(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // mov(reg int, imm int)
     if (CheckArgs(ptIntReg, ptInt)) Then
      CPU.move_memint_immint(getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // mov(reg int, reg int)
     if (CheckArgs(ptIntReg, ptIntReg)) Then
      CPU.move_memint_memint(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // mov(reg float, imm float)
     if (CheckArgs(ptFloatReg, ptFloat)) Then
      CPU.move_memfloat_immfloat(getRegisterAddress(Args[0]), Args[1].ImmFloat) Else

     // mov(reg float, reg float)
     if (CheckArgs(ptFloatReg, ptFloatReg)) Then
      CPU.move_memfloat_memfloat(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // mov(reg float, imm int)
     if (CheckArgs(ptFloatReg, ptInt)) Then
      CPU.move_memfloat_immint(getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // mov(reg float, reg int)
     if (CheckArgs(ptFloatReg, ptIntReg)) Then
      CPU.move_memfloat_memint(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // mov(reg string, imm string)
     if (CheckArgs(ptStringReg, ptString)) Then
      CPU.move_memstring_immstring(getRegisterAddress(Args[0]), Args[1].ImmString) Else

     // mov(reg string, reg string)
     if (CheckArgs(ptStringReg, ptStringReg)) Then
      CPU.move_memstring_memstring(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // mov(invalid)
      InvalidOpcodeException;
    End;

    { jmp, tjmp, fjmp, call }
    o_jmp, o_tjmp, o_fjmp, o_call:
    Begin
     if (not CheckArgs(ptInt)) Then // jumps and calls have to be constant (known at compile-time)
      InvalidOpcodeException;

     // write a special marker
     While (CompiledData.Position mod 4 <> 0) Do
      CPU.do_nop;

     CompiledData.write_uint32(JUMP_MARKER);
     CompiledData.write_uint8(ord(Opcode));
     CompiledData.write_int32(int32(CurrentBytecodePosition) + Args[0].ImmInt);

     // calls
     if (Opcode = o_call) Then
     Begin
      For I := 1 To CPU.get_bccall_size Do
       CompiledData.write_uint8(0);
     End Else

     // jumps
     if (Opcode in [o_fjmp, o_tjmp]) Then
     Begin
      For I := 1 To CPU.get_bcconditionaljump_size Do
       CompiledData.write_uint8(0);
     End;
    End;

    { icall }
    o_icall:
    Begin
     // icall(imm string)
     if (CheckArgs(ptString)) Then
     Begin
      if (Args[0].ImmString = 'vm.throw') Then
      Begin
       // @TODO - write me!
       Continue;
      End;

      if (Args[0].ImmString = 'vm.save_exception_state') Then
      Begin
       // @TODO - write me!
       Continue;
      End;

      if (Args[0].ImmString = 'vm.restore_exception_state') Then
      Begin
       // @TODO - write me!
       Continue;
      End;

      if (Args[0].ImmString = 'vm.set_exception_handler') Then
      Begin
       // @TODO - write me!
       Continue;
      End;

      if (Args[0].ImmString = 'vm.restore_exception_handler') Then
      Begin
       // @TODO - write me!
       Continue;
      End;

      if (Args[0].ImmString = 'vm.get_last_exception') Then
      Begin
       // @TODO - write me!
       Continue;
      End;

      icall := VM^.FindInternalCall(Args[0].ImmString);

      if (icall = nil) Then // error: icall not found
       raise Exception.CreateFmt('Unknown internal call: %s', [Args[0].ImmString]);

      CPU.do_icall(icall, ParamsMV, ResultMV);
     End Else

     // icall(invalid)
      InvalidOpcodeException;
    End;

    { ret }
    o_ret:
    Begin
     CPU.do_bcret;
    End;

    { if_e, if_ne, if_g, if_l, if_ge, if_le }
    o_if_e, o_if_ne, o_if_g, o_if_l, o_if_ge, o_if_le:
    Begin
     Case Opcode of
      o_if_e : CompareOperation := co_equal;
      o_if_ne: CompareOperation := co_different;
      o_if_g : CompareOperation := co_greater;
      o_if_l : CompareOperation := co_lower;
      o_if_ge: CompareOperation := co_greater_equal;
      o_if_le: CompareOperation := co_lower_equal;
     End;

     // opcode(imm int, imm int)
     if (CheckArgs(ptInt, ptInt)) Then
      CPU.compare_immint_immint(CompareOperation, Args[0].ImmInt, Args[1].ImmInt) Else

     // opcode(imm int, reg int)
     if (CheckArgs(ptInt, ptIntReg)) Then
      CPU.compare_immint_memint(CompareOperation, Args[0].ImmInt, getRegisterAddress(Args[1])) Else

     // opcode(imm int, imm float)
     if (CheckArgs(ptInt, ptFloat)) Then
      CPU.compare_immint_immfloat(CompareOperation, Args[0].ImmInt, Args[1].ImmFloat) Else

     // opcode(imm int, reg float)
     if (CheckArgs(ptInt, ptFloatReg)) Then
      CPU.compare_immint_memfloat(CompareOperation, Args[0].ImmInt, getRegisterAddress(Args[1])) Else

     // opcode(imm float, imm float)
     if (CheckArgs(ptFloat, ptFloat)) Then
      CPU.compare_immfloat_immfloat(CompareOperation, Args[0].ImmFloat, Args[1].ImmFloat) Else

     // opcode(imm float, reg float)
     if (CheckArgs(ptFloat, ptFloatReg)) Then
      CPU.compare_immfloat_memfloat(CompareOperation, Args[0].ImmFloat, getRegisterAddress(Args[1])) Else

     // opcode(imm float, imm int)
     if (CheckArgs(ptFloat, ptInt)) Then
      CPU.compare_immfloat_immint(CompareOperation, Args[0].ImmFloat, Args[1].ImmInt) Else

     // opcode(imm float, reg int)
     if (CheckArgs(ptFloat, ptIntReg)) Then
      CPU.compare_immfloat_memint(CompareOperation, Args[0].ImmFloat, getRegisterAddress(Args[1])) Else

     // opcode(reg int, imm int)
     if (CheckArgs(ptIntReg, ptInt)) Then
      CPU.compare_memint_immint(CompareOperation, getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // opcode(reg int, reg int)
     if (CheckArgs(ptIntReg, ptIntReg)) Then
      CPU.compare_memint_memint(CompareOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(reg int, imm float)
     if (CheckArgs(ptIntReg, ptFloat)) Then
      CPU.compare_memint_immfloat(CompareOperation, getRegisterAddress(Args[0]), Args[1].ImmFloat) Else

     // opcode(reg int, reg float)
     if (CheckArgs(ptIntReg, ptFloatReg)) Then
      CPU.compare_memint_memfloat(CompareOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(reg float, imm float)
     if (CheckArgs(ptFloatReg, ptFloat)) Then
      CPU.compare_memfloat_immfloat(CompareOperation, getRegisterAddress(Args[0]), Args[1].ImmFloat) Else

     // opcode(reg float, reg float)
     if (CheckArgs(ptFloatReg, ptFloatReg)) Then
      CPU.compare_memfloat_memfloat(CompareOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(reg float, imm int)
     if (CheckArgs(ptFloatReg, ptInt)) Then
      CPU.compare_memfloat_immint(CompareOperation, getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // opcode(reg float, reg int)
     if (CheckArgs(ptFloatReg, ptIntReg)) Then
      CPU.compare_memfloat_memint(CompareOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(invalid)
      InvalidOpcodeException;
    End;

    { strjoin }
    o_strjoin:
    Begin
     // strjoin(reg string, imm string)
     if (CheckArgs(ptStringReg, ptString)) Then
      CPU.strjoin_memstring_immstring(getRegisterAddress(Args[0]), Args[1].ImmString) Else

     // strjoin(reg string, reg string)
     if (CheckArgs(ptStringReg, ptStringReg)) Then
      CPU.strjoin_memstring_memstring(getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // strjoin(invalid)
      InvalidOpcodeException;
    End;

    { or, xor, and }
    o_or, o_xor, o_and:
    Begin
     Case Opcode of
      o_or : BitwiseOperation := bo_or;
      o_xor: BitwiseOperation := bo_xor;
      o_and: BitwiseOperation := bo_and;
     End;

     // opcode(reg bool, imm bool)
     if (CheckArgs(ptBoolReg, ptBool)) Then
      CPU.bitwise_membool_immbool(BitwiseOperation, getRegisterAddress(Args[0]), Args[1].ImmBool) Else

     // opcode(reg bool, reg bool)
     if (CheckArgs(ptBoolReg, ptBoolReg)) Then
      CPU.bitwise_membool_membool(BitwiseOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(reg int, imm int)
     if (CheckArgs(ptIntReg, ptInt)) Then
      CPU.bitwise_memint_immint(BitwiseOperation, getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // opcode(reg int, reg int)
     if (CheckArgs(ptIntReg, ptIntReg)) Then
      CPU.bitwise_memint_memint(BitwiseOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(invalid)
      InvalidOpcodeException;
    End;

    { shl, shr }
    o_shl, o_shr:
    Begin
     Case Opcode of
      o_shl: BitwiseOperation := bo_shl;
      o_shr: BitwiseOperation := bo_shr;
     End;

     // opcode(reg int, imm int)
     if (CheckArgs(ptIntReg, ptInt)) Then
      CPU.bitwise_memint_immint(BitwiseOperation, getRegisterAddress(Args[0]), Args[1].ImmInt) Else

     // opcode(reg int, reg int)
     if (CheckArgs(ptIntReg, ptIntReg)) Then
      CPU.bitwise_memint_memint(BitwiseOperation, getRegisterAddress(Args[0]), getRegisterAddress(Args[1])) Else

     // opcode(invalid)
      InvalidOpcodeException;
    End;

    else
     UnknownOpcodeException;
   End;
  End;

  jit_debug('First phase finished; compiled code size = %d bytes', [CompiledData.Size]);

  (* Phase 2: jump and call resolving *)
  jit_debug('Phase 2: jump and call resolving...');
  CompiledData.Position := 0;

  While (CompiledData.Position < CompiledData.Size) Do
  Begin
   if (CompiledData.read_uint32 = JUMP_MARKER) Then // found a jump marker?
   Begin
    Opcode  := TOpcode_E(CompiledData.read_uint8);
    JumpPos := CompiledData.read_int32;
    Jump    := JumpTable.FindJumpByBytecodeAddress(JumpPos);

    if (Jump = nil) Then
    Begin
     jit_debug('JumpTable.FindJumpByBytecodeAddress() returned ''nil'', PANIC!');

     raise Exception.CreateFmt('Found a jump to an invalid bytecode area (0x%x)!', [JumpPos]);
    End;

    JumpPos               := CompiledData.Position;
    CompiledData.Position := int64(JumpPos) - sizeof(int32) - sizeof(uint8) - sizeof(uint32);

    // unconditional jump (i.e. always taken)
    if (Opcode = o_jmp) Then
    Begin
     CPU.do_bcjump(Jump^.JumpAddress);
    End Else

    // conditional jump
    if (Opcode in [o_tjmp, o_fjmp]) Then
    Begin
     Inc(JumpPos, CPU.get_bcconditionaljump_size);
     CPU.do_bccondjump(Jump^.JumpAddress, Opcode);
    End Else

    // call (always taken)
    if (Opcode = o_call) Then
    Begin
     Inc(JumpPos, CPU.get_bccall_size);
     CPU.do_bccall(Jump^.JumpAddress);
    End Else

    // invalid opcode
     raise Exception.CreateFmt('This was not supposed to happen: invalid opcode in jump resolver (opcode = 0x%x)', [ord(Opcode)]);

    if (CompiledData.Position > JumpPos) Then
     raise Exception.CreateFmt('This was not supposed to happen: ''CompiledData.Position > JumpPos'' (the JIT CPU generated %d byte(s) too much)', [CompiledData.Position-JumpPos]);

    // fill left space with NOP-s
    While (CompiledData.Position < JumpPos) Do
     CPU.do_nop;

    CompiledData.Position := 0;
   End;
  End;

  jit_debug('CPU post compilation...');
  CPU.post_compilation;
 Finally
  CPU.getCompiledData.SaveToFile('jit_compiled');
  Reader.Free;
 End;

 Result := csDone;
End;
End.
