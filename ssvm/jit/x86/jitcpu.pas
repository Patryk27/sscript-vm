(*
 Copyright © by Patryk Wychowaniec, 2013
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
   Case Opcode.ID of
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

     Case Arg0.Kind of
      // memory
      joa_memory:
      Begin
       mov_reg32_imm32(reg_edx, uint32(Arg0.MemoryAddr));
      End;

      // invalid
      else
       InvalidOpcodeException;
     End;

     call_internalproc(@r__push_string);
    End;

    { bbmov }
    jo_bbmov:
    Begin
     if (Arg0.Kind = joa_memory) Then
     Begin
      mov_mem8_imm8(Arg0.MemoryAddr, Arg1.Constant);
     End Else

      InvalidOpcodeException;
    End;

    { iimov }
    jo_iimov:
    Begin
     if (Arg0.Kind = joa_memory) Then
     Begin
      TmpInt := Arg1.Constant;

      mov_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
      mov_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
     End Else

      InvalidOpcodeException;
    End;

    { iiadd }
    jo_iiadd:
    Begin
     // iiadd(mem, mem)
     if (Arg0.Kind = joa_memory) and (Arg1.Kind = joa_memory) Then
     Begin
      mov_reg32_mem32(reg_eax, Arg1.MemoryAddr+0);
      mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+4);
      add_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
      adc_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
     End Else

     // iiadd(mem, int)
     if (Arg0.Kind = joa_memory) and (Arg1.Kind = joa_constant) Then
     Begin
      TmpInt := Arg1.Constant;

      add_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
      adc_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
     End Else

      InvalidOpcodeException;
    End;

    { iisub }
    jo_iisub:
    Begin
     // iisub(mem, mem)
     if (Arg0.Kind = joa_memory) and (Arg1.Kind = joa_memory) Then
     Begin
      mov_reg32_mem32(reg_eax, Arg1.MemoryAddr+4);
      mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+0);
      sub_mem32_reg32(Arg0.MemoryAddr+4, reg_eax);
      sbb_mem32_reg32(Arg0.MemoryAddr+0, reg_ebx);
     End Else

     // iisub(mem, int)
     if (Arg0.Kind = joa_memory) and (Arg1.Kind = joa_constant) Then
     Begin
      TmpInt := Arg1.Constant;

      sub_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
      sbb_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
     End Else

      InvalidOpcodeException;
    End;

    { iimul }
    jo_iimul:
    Begin
     // iimul(mem, mem/int)
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

      if (Opcode.ID = jo_iidiv) Then
       call_internalproc(@r__div_imem_iconst) Else
       call_internalproc(@r__mod_imem_iconst);
     End Else

      InvalidOpcodeException;
    End;

    { jmp, tjmp, fjmp, call }
    jo_jmp, jo_tjmp, jo_fjmp, jo_call:
    Begin
     if (Arg0.Kind <> joa_constant) Then // check parameter type
      InvalidOpcodeException;

     {$DEFINE JTR := JumpsToResolve}
     {$DEFINE Last := JTR[High(JTR)]}
     SetLength(JTR, Length(JTR)+1);
     Last.Opcode       := Opcode.ID;
     Last.JumpAddress  := Arg0.Constant;
     Last.DataPosition := JumpTable.getLast.CodeAddress;
     {$UNDEF JTR}
     {$UNDEF Last}

     Case Opcode.ID of
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
     raise Exception.CreateFmt('TJITCPU.Compile() -> invalid opcode (kind=#%d)', [ord(Opcode.ID)]);
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
