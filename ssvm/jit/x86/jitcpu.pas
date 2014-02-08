(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITCPU;

 Interface
 Uses VMStruct, VMStack, VMTypes, VMICall, JITAbstractCPU, JITOpcodes, JITOpcodeList, JITAsm, JITJumpTable, Opcodes, Stream;

 {$MACRO ON}
 {$DEFINE ov := override}

 { TJumpToResolve }
 Type TJumpToResolve =
      Record
       Opcode      : TJITOpcodeKind;
       JumpAddress : uint32;
       DataPosition: uint32;
      End;

 { TJumpsToResolveArray }
 Type TJumpsToResolveArray = Array of TJumpToResolve;

 { TJITCPU }
 Type TJITCPU =
      Class (TJITAbstractCPU)
       Private
        JAsm          : TJITAsm;
        JumpTable     : TJITJumpTable;
        JumpsToResolve: TJumpsToResolveArray;

       Private
        Procedure ResolveJumps;

        Procedure int_compare(const Opcode: TJITOpcodeKind; const Number0, Number1: int64; const Addr0, Addr1: VMReference);

        Procedure FetchBoolStackval(const Dest: TRegister8; const StackvalPos: int32);
        Procedure FetchCharStackval(const Dest: TRegister8; const StackvalPos: int32);
        Procedure FetchIntStackval(const DestLo, DestHi: TRegister32; const StackvalPos: int32);
        Procedure FetchFloatStackval(const StackvalPos: int32);

        Procedure MoveConstantFloatToMemory(const DestMemory: VMReference; const Value: VMFloat);

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile(const OpcodeList: TJITOpcodeList): Pointer; ov;

       Public
        Function AllocateFloatConstants: Boolean; ov;
        Function AllocateStringConstants: Boolean; ov;
        Function isRegNative(const Kind: TBytecodeRegister; const ID: uint8): Boolean; ov;
       End;

 Implementation
Uses SysUtils, Variants;

Const asm_jmp_size     = 5;
      asm_condjmp_size = 13;
      asm_call_size    = 20;

// ------------------------ //
{$I routines.pas}
// ------------------------ //

(* TJITCPU.ResolveJumps *)
{
 Resolves jumps; called at the end of the compilation process.
}
Procedure TJITCPU.ResolveJumps;
Var Jump   : TJumpToResolve;
    JumpRec: TJITJumpRecord;
    Data   : TStream;
    RetEIP : uint32;
Begin
 Data := JAsm.getData;

 For Jump in JumpsToResolve Do
 Begin
  if (not JumpTable.FindJumpByBytecodeAddress(Jump.JumpAddress, JumpRec)) Then
   raise Exception.Create('Unexpected state: invalid JIT jump!');

  Data.Position := Jump.DataPosition;

  Case Jump.Opcode of
   // unconditional jump
   jo_jmp:
   Begin
    JAsm.jmp(JumpRec.CodeAddress - Data.Position - 5);
   End;

   // conditional jump
   jo_tjmp, jo_fjmp:
   Begin
    JAsm.cmp_mem8_imm8(@getVM^.Regs.b[5], ord(Jump.Opcode = jo_tjmp));
    JAsm.je(int32(JumpRec.CodeAddress) - int32(Data.Position) - 6);
   End;

   // call
   jo_call:
   Begin
    RetEIP := uint32(Data.Memory) + Data.Position + 20;

    JAsm.mov_reg32_imm32(reg_eax, uint32(getVM));
    JAsm.mov_reg32_imm32(reg_edx, RetEIP);
    JAsm.call_internalproc(@r__push_reference);

    JAsm.jmp(JumpRec.CodeAddress - Data.Position - 5);
   End;
  End;
 End;

 Data.Position := 0;
End;

(* TJITCPU.int_compare *)
{
 Compares two ints - by theirs values or numbers at given addresses.
}
Procedure TJITCPU.int_compare(const Opcode: TJITOpcodeKind; const Number0, Number1: int64; const Addr0, Addr1: VMReference);
Var CompareMode: (cmConstConst, cmConstMem, cmMemConst, cmMemMem);
    CmpResult  : Boolean;

    LabelTrue, LabelFalse, LabelOut   : uint32;
    TrueChange, FalseChange, OutChange: Array[0..2] of uint32;

    Data: TStream;

    Tmp, TmpPos: uint32;
Begin
 Data := JAsm.getData;

 For Tmp := Low(FalseChange) To High(FalseChange) Do
 Begin
  TrueChange[Tmp]  := 0;
  FalseChange[Tmp] := 0;
  OutChange[Tmp]   := 0;
 End;

 if (Addr0 = nil) Then
 Begin
  if (Addr1 = nil) Then
   CompareMode := cmConstConst Else
   CompareMode := cmConstMem;
 End Else
 Begin
  if (Addr1 = nil) Then
   CompareMode := cmMemConst Else
   CompareMode := cmMemMem;
 End;

 if (CompareMode = cmConstConst) Then // optimization: both operands are constant
 Begin
  CmpResult := False;

  Case Opcode of
   jo_iicmpe : CmpResult := (Number0 = Number1);
   jo_iicmpne: CmpResult := (Number0 <> Number1);
   jo_iicmpg : CmpResult := (Number0 > Number1);
   jo_iicmpl : CmpResult := (Number0 < Number1);
   jo_iicmpge: CmpResult := (Number0 >= Number1);
   jo_iicmple: CmpResult := (Number0 <= Number1);
  End;

  JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], ord(CmpResult));
  Exit;
 End;

 if (CompareMode = cmMemMem) and (Addr0 = Addr1) Then // optimization: both addresses are the same (comparing the same number with itself)
 Begin
  CmpResult := Opcode in [jo_iicmpe, jo_iicmpge, jo_iicmple];

  JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], ord(CmpResult));
  Exit;
 End;

 Case CompareMode of
  // const, mem
  cmConstMem:
  Begin
   JAsm.mov_reg32_imm32(reg_eax, lo(Number0));
   JAsm.cmp_reg32_mem32(reg_eax, Addr1+0);
  End;

  // mem, const
  cmMemConst:
  Begin
   JAsm.cmp_mem32_imm32(Addr0+0, lo(Number1));
  End;

  // mem, mem
  cmMemMem:
  Begin
   JAsm.mov_reg32_mem32(reg_eax, Addr1+0);
   JAsm.cmp_mem32_reg32(Addr0+0, reg_eax);
  End;
 End;

 Case Opcode of // first branch
  jo_iicmpe : FalseChange[0] := JAsm.jne(0);
  jo_iicmpne: TrueChange[0] := JAsm.jne(0);

  jo_iicmpg, jo_iicmpge:
  Begin
   TrueChange[0]  := JAsm.jg(0);
   FalseChange[0] := JAsm.jl(0);
  End;

  jo_iicmpl, jo_iicmple:
  Begin
   TrueChange[0]  := JAsm.jl(0);
   FalseChange[0] := JAsm.jg(0);
  End;

  else
   raise Exception.CreateFmt('TJITCPU.int_compare() -> unknown opcode=%d', [ord(Opcode)]);
 End;

 Case CompareMode of
  // const, mem
  cmConstMem:
  Begin
   JAsm.mov_reg32_imm32(reg_eax, hi(Number0));
   JAsm.cmp_reg32_mem32(reg_eax, Addr1+4);
  End;

  // mem, const
  cmMemConst:
  Begin
   JAsm.cmp_mem32_imm32(Addr0+4, hi(Number1));
  End;

  // mem, mem
  cmMemMem:
  Begin
   JAsm.mov_reg32_mem32(reg_eax, Addr1+4);
   JAsm.cmp_mem32_reg32(Addr0+4, reg_eax);
  End;
 End;

 Case Opcode of // second branch
  jo_iicmpe : FalseChange[1] := JAsm.jne(0);
  jo_iicmpne: FalseChange[1] := JAsm.je(0);
  jo_iicmpg : FalseChange[1] := JAsm.jna(0);
  jo_iicmpge: FalseChange[1] := JAsm.jnae(0);
  jo_iicmpl : FalseChange[1] := JAsm.jnb(0);
  jo_iicmple: FalseChange[1] := JAsm.jnbe(0);
 End;

 // @true:
 LabelTrue := Data.Position;
 JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], 1);
 OutChange[2] := JAsm.jmp(0);

 // @false:
 LabelFalse := Data.Position;
 JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], 0);

 // @out:
 LabelOut := Data.Position;

 { replace dummy addresses with their real values }
 TmpPos := Data.Position;

 // replace @true
 For Tmp in TrueChange Do
  if (Tmp > 0) Then
  Begin
   Data.Position := Tmp;
   Data.write_int32(LabelTrue - Tmp - 4);
  End;

 // replace @false
 For Tmp in FalseChange Do
  if (Tmp > 0) Then
  Begin
   Data.Position := Tmp;
   Data.write_int32(LabelFalse - Tmp - 4);
  End;

 // replace @out
 For Tmp in OutChange Do
  if (Tmp > 0) Then
  Begin
   Data.Position := Tmp;
   Data.write_uint32(LabelOut - Tmp - 4);
  End;

 Data.Position := TmpPos;
End;

(* TJITCPU.FetchBoolStackval *)
{
 Loads stackval from VM stack and moves it into given x86 register.
}
Procedure TJITCPU.FetchBoolStackval(const Dest: TRegister8; const StackvalPos: int32);
Begin
 With JAsm do
 Begin
  mov_reg32_imm32(reg_eax, uint32(getVM));
  mov_reg32_imm32(reg_edx, StackvalPos);
  call_internalproc(@r__stackval_fetch_bool);
  mov_reg8_reg8(Dest, reg_al);
 End;
End;

(* TJITCPU.FetchCharStackval *)
{
 Loads stackval from VS stack and moves it into given x86 register.
}
Procedure TJITCPU.FetchCharStackval(const Dest: TRegister8; const StackvalPos: int32);
Begin
 With JAsm do
 Begin
  mov_reg32_imm32(reg_eax, uint32(getVM));
  mov_reg32_imm32(reg_edx, StackvalPos);
  call_internalproc(@r__stackval_fetch_char);
  mov_reg8_reg8(Dest, reg_al);
 End;
End;

(* TJITCPU.FetchIntStackval *)
{
 Loads stackval from VM stack and moves it into 2 given x86 registers.
}
Procedure TJITCPU.FetchIntStackval(const DestLo, DestHi: TRegister32; const StackvalPos: int32);
Begin
 With JAsm do
 Begin
  mov_reg32_imm32(reg_eax, uint32(getVM));
  mov_reg32_imm32(reg_edx, StackvalPos);
  call_internalproc(@r__stackval_fetch_int);
  mov_reg32_reg32(DestLo, reg_eax);
  mov_reg32_reg32(DestHi, reg_edx);
 End;
End;

(* TJITCPU.FetchFloatStackval *)
{
 Loads stackval from VM stack and pushes it onto x86 FPU stack.
}
Procedure TJITCPU.FetchFloatStackval(const StackvalPos: int32);
Begin
 With JAsm do
 Begin
  mov_reg32_imm32(reg_eax, uint32(getVM));
  mov_reg32_imm32(reg_edx, StackvalPos);
  call_internalproc(@r__stackval_fetch_float);
 End;
End;

(* TJITCPU.MoveConstantFloatToMemory *)
Procedure TJITCPU.MoveConstantFloatToMemory(const DestMemory: VMReference; const Value: VMFloat);
Var Data: Packed Record
           A, B: uint32;
           C   : uint16;
          End absolute Value;
Begin
 {$IF sizeof(VMFloat) <> 10}
  {$FATAL sizeof(VMFloat) <> 10 (!!!)}
 {$ENDIF}

 With JAsm do
 Begin
  mov_mem32_imm32(DestMemory+0, Data.A);
  mov_mem32_imm32(DestMemory+4, Data.B);
  mov_mem16_imm16(DestMemory+8, Data.C);
 End;
End;

(* TJITCPU.Create *)
Constructor TJITCPU.Create(const fVM: PVM);
Begin
 inherited Create(fVM);

 JAsm      := TJITAsm.Create;
 JumpTable := TJITJumpTable.Create;
End;

(* TJITCPU.Destroy *)
Destructor TJITCPU.Destroy;
Begin
 JAsm.Free;
 JumpTable.Free;

 inherited Destroy;
End;

(* TJITCPU.Compile *)
Function TJITCPU.Compile(const OpcodeList: TJITOpcodeList): Pointer;
Var OpcodeID  : uint32;
    Opcode    : TJITOpcode;
    Arg0, Arg1: TJITOpcodeArg;
    icall     : PInternalCall;

    ResultMV, ParamsMV: PMixedValue;

    TmpInt: Int64;

    I: Integer;

  { InvalidOpcodeException }
  Procedure InvalidOpcodeException;
  Begin
   raise Exception.CreateFmt('TJITCPU.Compile() failed; invalid JIT opcode (opcode index=%d)', [OpcodeID]);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg0: TJITOpcodeArgKind): Boolean; inline;
  Begin
   Result := (Opcode.Args[0].Kind = Arg0);
  End;

  { CheckArgs }
  Function CheckArgs(const Arg0, Arg1: TJITOpcodeArgKind): Boolean; inline;
  Begin
   Result := (Opcode.Args[0].Kind = Arg0) and (Opcode.Args[1].Kind = Arg1);
  End;

Begin
 if (OpcodeList.getSize = 0) Then
  raise Exception.Create('TJITCPU.Compile() -> OpcodeList.getSize() = 0 (shouldn''t happen!)');

 if (JAsm.getData.Size > 0) Then
  raise Exception.CreateFmt('TJITCPU.Compile() called more than once on one TJITCPU instance (JAsm.getData.Size() = %d)', [JAsm.getData.Size]);

 Result := nil;

 ResultMV := JITMemAlloc(sizeof(TMixedValue)); // used in icall-s
 ParamsMV := JITMemAlloc(sizeof(TMixedValue)); // ditto

 Try
  (* Compile *)
  For OpcodeID := 0 To OpcodeList.getSize-1 Do
  Begin
   Opcode := OpcodeList[OpcodeID];
   Arg0   := Opcode.Args[0];
   Arg1   := Opcode.Args[1];

   JumpTable.AddJump(OpcodeID, JAsm.getData.Size);

   With JAsm do
   Case Opcode.Kind of

    {$I x86_stack.pas}
    {$I x86_mov.pas}
    {$I x86_int_math.pas}
    {$I x86_float_math.pas}

    { jmp, tjmp, fjmp, call }
    jo_jmp, jo_tjmp, jo_fjmp, jo_call:
    Begin
     if (Arg0.Kind <> joa_constant) Then // check parameter type
      InvalidOpcodeException;

     {$DEFINE JTR := JumpsToResolve}
     {$DEFINE Last := JTR[High(JTR)]}
     SetLength(JTR, Length(JTR)+1);
     Last.Opcode       := Opcode.Kind;
     Last.JumpAddress  := Arg0.Constant;
     Last.DataPosition := JumpTable.getLast.CodeAddress;
     {$UNDEF JTR}
     {$UNDEF Last}

     Case Opcode.Kind of
      jo_jmp          : I := asm_jmp_size;
      jo_call         : I := asm_call_size;
      jo_tjmp, jo_fjmp: I := asm_condjmp_size;

      else
       InvalidOpcodeException;
     End;

     For I := 1 To I Do
      nop;
    End;

    { ret }
    jo_ret:
    Begin
     mov_reg32_imm32(reg_eax, uint32(getVM));
     call_internalproc(@r__pop_reference);
     jmp(reg_eax);
    End;

    { icall }
    jo_icall:
    Begin
     if (Arg0.Kind <> joa_memory) Then // check parameter type
      InvalidOpcodeException;

     // fetch pointer
     icall := Arg0.MemoryAddr;

     // prepare parameter list
     mov_reg32_imm32(reg_eax, uint32(getVM));
     mov_reg32_imm32(reg_edx, uint32(icall));
     mov_reg32_imm32(reg_ecx, uint32(ParamsMV));
     call_internalproc(@r__create_icall_parameter_list);

     // clean result
     mov_reg32_imm32(reg_eax, uint32(ResultMV));
     call_internalproc(@r__clean_mixedvalue);

     // call icall
     mov_reg32_imm32(reg_eax, uint32(getVM));
     mov_reg32_imm32(reg_edx, uint32(ParamsMV));
     mov_reg32_imm32(reg_ecx, uint32(ResultMV));
     call_internalproc(icall^.Handler);

     // apply result; eax is preserved from the previous call, so we don't have to assign it again
     mov_reg32_imm32(reg_edx, uint32(ResultMV));
     call_internalproc(@r__apply_mixedvalue);
    End;

    { stop }
    jo_stop:
    Begin
     ret;
    End;

    { invalid opcode }
    else
     raise Exception.CreateFmt('TJITCPU.Compile() -> invalid opcode (index=%d, kind=#%d)', [OpcodeID, ord(Opcode.Kind)]);
   End;
  End;

  (* Resolve jumps *)
  ResolveJumps;
 Finally
 End;

 // post compilation
 JAsm.getData.Position := 0;
 JAsm.post_compilation;

 // do other "post" things
 JAsm.getData.SaveToFile('jit_compiled.o'); // @note: debug only

 Result := JAsm.getData.getMemoryPosition;
End;

(* TJITCPU.AllocateFloatConstants *)
Function TJITCPU.AllocateFloatConstants: Boolean;
Begin
 Result := False;
End;

(* TJITCPU.AllocateStringConstants *)
Function TJITCPU.AllocateStringConstants: Boolean;
Begin
 Result := True;
End;

(* TJITCPU.isRegNative *)
Function TJITCPU.isRegNative(const Kind: TBytecodeRegister; const ID: uint8): Boolean;
Begin
 Result := False;
End;
End.
