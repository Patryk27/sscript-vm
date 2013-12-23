(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JITCPU;

 // @TODO: ei1 -> eax:ebx, es1 -> ecx, eb1 -> edx?
 // Ew.zrobić to dynamicznie - te najczęściej używane rejestry lądują w fizycznych rejestrach CPU, a reszta sio do pamięci; najlepiej jeszcze, aby taki "najczęściej używany" wybierany był przez kompilator lub maszynę na poziomie każdej funkcji.

 Interface
 Uses VM, Stack, Opcodes, VMTypes, JITAbstractCPU, JITOpcodes, JITOpcodeList, JITAsm;

 {$MACRO ON}
 {$DEFINE ov := override}

 { TJITCPU }
 Type TJITCPU =
      Class (TJITAbstractCPU)
       Private
        JAsm: TJITAsm;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile(const OpcodeList: TJITOpcodeList): Pointer; ov;

        Function hasNativeReg(const Kind: TBytecodeRegister; const ID: uint8): Boolean; ov;
       End;

 Implementation
Uses SysUtils, Variants;

// ------------------------ //
{$I routines.pas}
// ------------------------ //

(* TJITCPU.Create *)
Constructor TJITCPU.Create(const fVM: PVM);
Begin
 inherited Create(fVM);

 JAsm := TJITAsm.Create;
End;

(* TJITCPU.Destroy *)
Destructor TJITCPU.Destroy;
Begin
 JAsm.Free;

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
  For OpcodeID := 0 To OpcodeList.getSize-1 Do
  Begin
   Opcode := OpcodeList[OpcodeID];
   Arg0   := Opcode.Args[0];
   Arg1   := Opcode.Args[1];

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
 Finally
 End;

 // post compilation
 JAsm.getData.Position := 0;
 JAsm.post_compilation;

 // do other "post" things
 JAsm.getData.SaveToFile('jit_compiled.o');

 Result := JAsm.getData.getMemoryPosition;
End;

(* TJITCPU.hasNativeReg *)
Function TJITCPU.hasNativeReg(const Kind: TBytecodeRegister; const ID: uint8): Boolean;
Begin
 {Case Kind of
  reg_ei: Result := (ID = 1); // "ei1"

  else
   Result := False;
 End;}

 Result := False;
End;
End.
