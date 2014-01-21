(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITCPU;

 Interface
 Uses VM, Stack, Opcodes, VMTypes, JITAbstractCPU, JITOpcodes, JITOpcodeList, JITAsm, JITJumpTable, Stream;

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
        Procedure FetchIntStackval(const DestLo, DestHi: TRegister32; const StackvalPos: int32);

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile(const OpcodeList: TJITOpcodeList): Pointer; ov;

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

 if (CompareMode = cmConstConst) Then // both operands are known
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

 if (CompareMode = cmMemMem) and (Addr0 = Addr1) Then // both addresses are the same (comparing the same number with itself)
 Begin
  CmpResult := Opcode in [jo_iicmpe, jo_iicmpge, jo_iicmple];

  JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], ord(CmpResult));
  Exit;
 End;

 Case CompareMode of
  // const, mem
  cmConstMem:
  Begin
   JAsm.mov_reg32_imm32(reg_eax, lo(Number0)); // mov eax, lo(Number0)
   JAsm.cmp_reg32_mem32(reg_eax, Addr1+0); // cmp eax, dword [Addr1+0]
  End;

  // mem, const
  cmMemConst:
  Begin
   JAsm.cmp_mem32_imm32(Addr0+0, lo(Number1)); // cmp [Addr0+0], lo(Number1)
  End;

  // mem, mem
  cmMemMem:
  Begin
   JAsm.mov_reg32_mem32(reg_eax, Addr1+0); // mov eax, [Addr1+0]
   JAsm.cmp_mem32_reg32(Addr0+0, reg_eax); // cmp [Addr0+0], eax
  End;
 End;

 Case Opcode of // first branch
  jo_iicmpe : FalseChange[0] := JAsm.jne(0); // jne @false
  jo_iicmpne: TrueChange[0] := JAsm.jne(0); // jne @true

  jo_iicmpg, jo_iicmpge:
  Begin
   TrueChange[0]  := JAsm.jg(0); // jg @true
   FalseChange[0] := JAsm.jl(0); // jl @false
  End;

  jo_iicmpl, jo_iicmple:
  Begin
   TrueChange[0]  := JAsm.jl(0); // jl @true
   FalseChange[0] := JAsm.jg(0); // jg @false
  End;

  else
   raise Exception.CreateFmt('TJITCPU.int_compare() -> unknown opcode=%d', [ord(Opcode)]);
 End;

 Case CompareMode of
  // const, mem
  cmConstMem:
  Begin
   JAsm.mov_reg32_imm32(reg_eax, hi(Number0)); // mov eax, hi(Number0)
   JAsm.cmp_reg32_mem32(reg_eax, Addr1+4); // cmp eax, dword [Addr1+4]
  End;

  // mem, const
  cmMemConst:
  Begin
   JAsm.cmp_mem32_imm32(Addr0+4, hi(Number1)); // cmp [Addr0+4], hi(Number1)
  End;

  // mem, mem
  cmMemMem:
  Begin
   JAsm.mov_reg32_mem32(reg_eax, Addr1+4); // mov eax, [Addr1+0]
   JAsm.cmp_mem32_reg32(Addr0+4, reg_eax); // cmp [Addr0+4], eax
  End;
 End;

 Case Opcode of // second branch
  jo_iicmpe : FalseChange[1] := JAsm.jne(0); // jne @false
  jo_iicmpne: FalseChange[1] := JAsm.je(0); // je @false
  jo_iicmpg : FalseChange[1] := JAsm.jna(0); // jna @false
  jo_iicmpge: FalseChange[1] := JAsm.jnae(0); // jnae @false
  jo_iicmpl : FalseChange[1] := JAsm.jnb(0); // jnb @false
  jo_iicmple: FalseChange[1] := JAsm.jnbe(0); // jnbe @false
 End;

 // @true:
 LabelTrue := Data.Position;
 JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], 1); // mov byte [IF-register-address], 1
 OutChange[2] := JAsm.jmp(0); // jmp @out

 // @false:
 LabelFalse := Data.Position;
 JAsm.mov_mem8_imm8(@getVM^.Regs.b[5], 0); // mov byte [IF-register-address], 0

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

(* TJITCPU.FetchIntStackval *)
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
    icall     : PCall;

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
    { bpush }
    jo_bpush:
    Begin
     mov_reg32_imm32(reg_eax, uint32(getVM));

     Case Arg0.Kind of
      // memory
      joa_memory:
      Begin
       mov_reg32_imm32(reg_edx, 0);
       mov_reg8_mem8(reg_dl, Arg0.MemoryAddr);
      End;

      // constant
      joa_constant:
      Begin
       mov_reg32_imm32(reg_edx, ord(Boolean(Arg0.Constant)));
      End;
     End;

     call_internalproc(@r__push_bool);
    End;

    { ipush }
    jo_ipush:
    Begin
     mov_reg32_imm32(reg_eax, uint32(getVM));

     Case Arg0.Kind of
      // memory
      joa_memory:
      Begin
       mov_reg32_mem32(reg_edx, Arg0.MemoryAddr+0);
       mov_reg32_mem32(reg_ecx, Arg0.MemoryAddr+4);
      End;

      // constant
      joa_constant:
      Begin
       TmpInt := Arg0.Constant;

       mov_reg32_imm32(reg_edx, lo(TmpInt));
       mov_reg32_imm32(reg_ecx, hi(TmpInt));
      End;

      // invalid
      else
       InvalidOpcodeException;
     End;

     call_internalproc(@r__push_int);
    End;

    { spush }
    jo_spush:
    Begin
     mov_reg32_imm32(reg_eax, uint32(getVM));

     if (Arg0.Kind = joa_memory) Then
      mov_reg32_imm32(reg_edx, uint32(Arg0.MemoryAddr)) Else
      InvalidOpcodeException;

     call_internalproc(@r__push_string);
    End;

    { vpush }
    jo_vpush:
    Begin
     if (Arg0.Kind <> joa_stackval) Then
      InvalidOpcodeException;

     mov_reg32_imm32(reg_eax, uint32(getVM));
     mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
     call_internalproc(@r__push_stackval);
    End;

    { bpop, cpop, ipop, fpop, spop, rpop }
    jo_bpop..jo_rpop:
    Begin
     // opcode(mem)
     if (CheckArgs(joa_memory)) Then
     Begin
      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, uint32(Arg0.MemoryAddr));

      Case Opcode.Kind of
       jo_bpop: call_internalproc(@r__pop_bool_reg);
       jo_cpop: call_internalproc(@r__pop_char_reg);
       jo_ipop: call_internalproc(@r__pop_int_reg);
       jo_fpop: call_internalproc(@r__pop_float_reg);
       jo_spop: call_internalproc(@r__pop_string_reg);
       jo_rpop: call_internalproc(@r__pop_reference_reg);
      End;
     End Else

      InvalidOpcodeException;
    End;

    { bbmov }
    jo_bbmov:
    Begin
     // bbmov(mem, const)
     if (CheckArgs(joa_memory, joa_constant)) Then
     Begin
      mov_mem8_imm8(Arg0.MemoryAddr, Arg1.Constant);
     End Else

      InvalidOpcodeException;
    End;

    { iimov }
    jo_iimov:
    Begin
     // iimov(mem, const)
     if (CheckArgs(joa_memory, joa_constant)) Then
     Begin
      TmpInt := Arg1.Constant;

      mov_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
      mov_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
     End Else

     // iimov(mem, mem)
     if (CheckArgs(joa_memory, joa_memory)) Then
     Begin
      mov_reg32_mem32(reg_eax, Arg0.MemoryAddr+0);
      mov_reg32_mem32(reg_ebx, Arg0.MemoryAddr+4);
      mov_mem32_reg32(Arg1.MemoryAddr+0, reg_eax);
      mov_mem32_reg32(Arg1.MemoryAddr+4, reg_ebx);
     End Else

     // iimov(stackval, const)
     if (CheckArgs(joa_stackval, joa_constant)) Then
     Begin
      TmpInt := Arg1.Constant;

      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_imm32(hi(TmpInt));
      push_imm32(lo(TmpInt));

      call_internalproc(@r__set_stackval_int);
     End Else

     // iimov(stackval, mem)
     if (CheckArgs(joa_stackval, joa_memory)) Then
     Begin
      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_mem32(Arg1.MemoryAddr+4);
      push_mem32(Arg1.MemoryAddr+0);

      call_internalproc(@r__set_stackval_int);
     End Else

      InvalidOpcodeException;
    End;

    { iiadd, iisub }
    jo_iiadd, jo_iisub:
    Begin
     // opcode(mem, const)
     if (CheckArgs(joa_memory, joa_constant)) Then
     Begin
      TmpInt := Arg1.Constant;

      if (Opcode.Kind = jo_iiadd) Then
      Begin
       add_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
       adc_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
      End Else
      Begin
       sub_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
       sbb_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
      End;
     End Else

     // opcode(mem, mem)
     if (CheckArgs(joa_memory, joa_memory)) Then
     Begin
      mov_reg32_mem32(reg_eax, Arg1.MemoryAddr+0);
      mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+4);

      if (Opcode.Kind = jo_iiadd) Then
      Begin
       add_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
       adc_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
      End Else
      Begin
       sub_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
       sbb_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
      End;
     End Else

     // opcode(mem, stackval)
     if (CheckArgs(joa_memory, joa_stackval)) Then
     Begin
      FetchIntStackval(reg_eax, reg_edx, Arg1.StackvalPos);

      if (Opcode.Kind = jo_iiadd) Then
      Begin
       add_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
       adc_mem32_reg32(Arg0.MemoryAddr+4, reg_edx);
      End Else
      Begin
       sub_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
       sbb_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
      End;
     End Else

     // opcode(stackval, const)
     if (CheckArgs(joa_stackval, joa_constant)) Then
     Begin
      TmpInt := Arg1.Constant;

      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_imm32(hi(TmpInt));
      push_imm32(lo(TmpInt));

      if (Opcode.Kind = jo_iiadd) Then
       call_internalproc(@r__add_stackval_int) Else
       call_internalproc(@r__sub_stackval_int);
     End Else

     // opcode(stackval, mem)
     if (CheckArgs(joa_stackval, joa_memory)) Then
     Begin
      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_mem32(Arg1.MemoryAddr+4);
      push_mem32(Arg1.MemoryAddr+0);

      if (Opcode.Kind = jo_iiadd) Then
       call_internalproc(@r__add_stackval_int) Else
       call_internalproc(@r__sub_stackval_int);
     End Else

      InvalidOpcodeException;
    End;

    { iimul }
    jo_iimul:
    Begin
     // iimul(mem, mem/const)
     if (Arg0.Kind = joa_memory) and (Arg1.Kind in [joa_memory, joa_constant]) Then
     Begin
      mov_reg32_mem32(reg_edi, Arg0.MemoryAddr+4);
      mov_reg32_mem32(reg_esi, Arg0.MemoryAddr+0);

      if (Arg1.Kind = joa_memory) Then
      Begin
       mov_reg32_mem32(reg_ecx, Arg1.MemoryAddr+4);
       mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+0);
      End Else
      Begin
       TmpInt := Arg1.Constant;

       mov_reg32_imm32(reg_ecx, hi(TmpInt));
       mov_reg32_imm32(reg_ebx, lo(TmpInt));
      End;

      mov_reg32_reg32(reg_eax, reg_edi);
      mul_reg32(reg_ebx);
      xchg_reg32_reg32(reg_eax, reg_ebx);
      mul_reg32(reg_esi);
      xchg_reg32_reg32(reg_esi, reg_eax);
      add_reg32_reg32(reg_ebx, reg_edx);
      mul_reg32(reg_ecx);
      add_reg32_reg32(reg_ebx, reg_eax);

      mov_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
      mov_mem32_reg32(Arg0.MemoryAddr+0, reg_esi);
     End Else

     // iimul(stackval, const)
     if (CheckArgs(joa_stackval, joa_constant)) Then
     Begin
      TmpInt := Arg1.Constant;

      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_imm32(hi(TmpInt));
      push_imm32(lo(TmpInt));

      call_internalproc(@r__mul_stackval_int);
     End Else

     // iimul(stackval, mem)
     if (CheckArgs(joa_stackval, joa_memory)) Then
     Begin
      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_mem32(Arg1.MemoryAddr+4);
      push_mem32(Arg1.MemoryAddr+0);

      call_internalproc(@r__mul_stackval_int);
     End Else

      InvalidOpcodeException;
    End;

    { iidiv, iimod }
    jo_iidiv, jo_iimod:
    Begin
     // opcode(mem, mem/int)
     if (Arg0.Kind = joa_memory) and (Arg1.Kind in [joa_memory, joa_constant]) Then
     Begin
      mov_reg32_imm32(reg_eax, uint32(Arg0.MemoryAddr));

      if (Arg1.Kind = joa_memory) Then
      Begin
       mov_reg32_mem32(reg_edx, Arg1.MemoryAddr+0);
       mov_reg32_mem32(reg_ecx, Arg1.MemoryAddr+4);
      End Else
      Begin
       TmpInt := Arg1.Constant;

       mov_reg32_imm32(reg_edx, lo(TmpInt));
       mov_reg32_imm32(reg_ecx, hi(TmpInt));
      End;

      if (Opcode.Kind = jo_iidiv) Then
       call_internalproc(@r__div_imem_iconst) Else
       call_internalproc(@r__mod_imem_iconst);
     End Else

     // opcode(stackval, const)
     if (CheckArgs(joa_stackval, joa_constant)) Then
     Begin
      TmpInt := Arg1.Constant;

      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_imm32(hi(TmpInt));
      push_imm32(lo(TmpInt));

      if (Opcode.Kind = jo_iidiv) Then
       call_internalproc(@r__div_stackval_int) Else
       call_internalproc(@r__mod_stackval_int);
     End Else

     // opcode(stackval, mem)
     if (CheckArgs(joa_stackval, joa_memory)) Then
     Begin
      mov_reg32_imm32(reg_eax, uint32(getVM));
      mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
      push_mem32(Arg1.MemoryAddr+4);
      push_mem32(Arg1.MemoryAddr+0);

      if (Opcode.Kind = jo_iidiv) Then
       call_internalproc(@r__div_stackval_int) Else
       call_internalproc(@r__mod_stackval_int);
     End Else

      InvalidOpcodeException;
    End;

    { iicmp* }
    jo_iicmpe, jo_iicmpne, jo_iicmpg, jo_iicmpl, jo_iicmpge, jo_iicmple:
    Begin
     Case Arg0.Kind of
      // iicmp*(const, const/mem)
      joa_constant:
      Begin
       if (Arg1.Kind = joa_constant) Then
        int_compare(Opcode.Kind, Arg0.Constant, Arg1.Constant, nil, nil) Else

       if (Arg1.Kind = joa_memory) Then
        int_compare(Opcode.Kind, Arg0.Constant, 0, nil, Arg1.MemoryAddr) Else

        InvalidOpcodeException;
      End;

      // iicmp*(mem, const/mem)
      joa_memory:
      Begin
       if (Arg1.Kind = joa_constant) Then
        int_compare(Opcode.Kind, 0, Arg1.Constant, Arg0.MemoryAddr, nil) Else

       if (Arg1.Kind = joa_memory) Then
        int_compare(Opcode.Kind, 0, 0, Arg0.MemoryAddr, Arg1.MemoryAddr) Else

        InvalidOpcodeException;
      End;

      else
       InvalidOpcodeException;
     End;
    End;

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
     raise Exception.CreateFmt('TJITCPU.Compile() -> invalid opcode (kind=#%d)', [ord(Opcode.Kind)]);
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

(* TJITCPU.isRegNative *)
Function TJITCPU.isRegNative(const Kind: TBytecodeRegister; const ID: uint8): Boolean;
Begin
 Result := False;
End;
End.
