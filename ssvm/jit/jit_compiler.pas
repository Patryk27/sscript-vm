(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)

{$I jit_cpu.inc}

Unit JIT_Compiler;

 Interface
 Uses VM, Stack, BCReader, Opcodes, JIT_Abstract_CPU,

 {$I jit_cpu.inc}

 {$IF CPU = CPU_x86}
  CPU_x86
 {$ENDIF};

 { TJITCompiler }
 Type TJITCompiler =
      Class
       Private
        VM : PVM;
        CPU: TJITAbstractCPU;

       Private
        Function getRegisterAddress(const Arg: TOpcodeArg): uint64;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile: TJITCompiledState;

        Property getCPU: TJITAbstractCPU read CPU;
       End;

 Implementation
Uses SysUtils, TypInfo;

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
 VM  := fVM;
 CPU := TJITCPU.Create(VM);
End;

(* TJITCompiler.Destroy *)
Destructor TJITCompiler.Destroy;
Begin
 inherited Destroy;
End;

(* TJITCompiler.Compile *)
Function TJITCompiler.Compile: TJITCompiledState;
Var Reader: TBytecodeReader;
    Opcode: TOpcode_E;
    Args  : TOpcodeArgArray;

    icall             : PCall;
    ResultMV, ParamsMV: PMixedValue;

    { InvalidOpcodeException }
    Procedure InvalidOpcodeException;
    Const BoolRegsNames: Array[1..5] of String = ('eb1', 'eb2', 'eb3', 'eb4', 'if');
          IntRegsNames : Array[1..5] of String = ('ei1', 'ei2', 'ei3', 'ei4', 'stp');
          BoolTable    : Array[Boolean] of String = ('false', 'true');
    Var Msg: String = '';
        I  : uint8;
    Begin
     Msg := 'Invalid opcode: '+Copy(GetEnumName(TypeInfo(Opcode), ord(Opcode)), 3, 50)+'(';

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
Begin
 Result := csJITFailed;

 New(ResultMV);
 ParamsMV := AllocMem(256*sizeof(TMixedValue));

 Reader := TBytecodeReader.Create(VM^.LoaderData.CodeData);

 CPU.pre_compilation;

 Try
  While (Reader.AnyOpcodeLeft) Do
  Begin
   { fetch opcode }
   Opcode := Reader.FetchOpcode;
   Args   := Reader.FetchArguments(Opcode);

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
      CPU.bcpush_bool_immbool(Args[0].ImmBool) Else

     // push(reg bool)
     if (CheckArgs(ptBoolReg)) Then
      CPU.bcpush_bool_regbool(getRegisterAddress(Args[0])) Else

     // push(imm int)
     if (CheckArgs(ptInt)) Then
      CPU.bcpush_int_immint(Args[0].ImmInt) Else

     // push(reg int)
     if (CheckArgs(ptIntReg)) Then
      CPU.bcpush_int_memint(getRegisterAddress(Args[0])) Else

     // push(imm float)
     if (CheckArgs(ptFloat)) Then
      CPU.bcpush_float_immfloat(Args[0].ImmFloat) Else

     // push(reg float)
     if (CheckArgs(ptFloatReg)) Then
      CPU.bcpush_float_memfloat(getRegisterAddress(Args[0])) Else

     // push(invalid)
      InvalidOpcodeException;
    End;

    { pop }
    o_pop:
    Begin
     // pop(reg int)
     if (CheckArgs(ptIntReg)) Then
      CPU.bcpop_int_reg(getRegisterAddress(Args[0])) Else

     // pop(invalid)
      InvalidOpcodeException;
    End;

    { add, sub, mul, div }
    o_add, o_sub, o_mul, o_div:
    Begin
     Case Opcode of
      o_add: ArithmeticOperation := aoAdd;
      o_sub: ArithmeticOperation := aoSub;
      o_mul: ArithmeticOperation := aoMul;
      o_div: ArithmeticOperation := aoDiv;
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

     // opcode(invalid)
      InvalidOpcodeException;
    End;

    { mov }
    o_mov:
    Begin
     // mov(reg bool, imm bool)
     if (CheckArgs(ptBoolReg, ptBool)) Then
      CPU.move_membool_immbool(getRegisterAddress(Args[0]), Args[1].ImmBool) Else

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

     // mov(invalid)
      InvalidOpcodeException;
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

    { or, xor, and }
    o_or, o_xor, o_and:
    Begin
     Case Opcode of
      o_or : BitwiseOperation := boOr;
      o_xor: BitwiseOperation := boXor;
      o_and: BitwiseOperation := boAnd;
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

    else
     UnknownOpcodeException;
   End;
  End;
 Finally
  CPU.getCompiledData.SaveToFile('jit_compiled');
  Reader.Free;
 End;

 CPU.post_compilation;

 Result := csDone;
End;
End.
