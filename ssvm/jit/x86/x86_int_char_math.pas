 { icadd, icsub }
 jo_icadd, jo_icsub:
 Begin
  // opcode(mem, const)
  if (CheckArgs(joa_memory, joa_constant)) Then
  Begin
   TmpInt := VMInt(ord(VMChar(Arg1.Constant)));

   if (Opcode.Kind = jo_icadd) Then
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
   mov_reg32_imm32(reg_eax, 0);
   mov_reg8_mem8(reg_al, Arg1.MemoryAddr);

   if (Opcode.Kind = jo_icadd) Then
   Begin
    add_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
    adc_mem32_imm32(Arg0.MemoryAddr+4, 0);
   End Else
   Begin
    sub_mem32_imm32(Arg0.MemoryAddr+4, 0);
    sbb_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
   End;
  End Else

  // opcode(invalid)
  Begin
   InvalidOpcodeException;
  End;
 End;

 { icmul }
 jo_icmul:
 Begin
  // icmul(mem, const/mem)
  if (CheckArgs([joa_memory], [joa_constant, joa_memory])) Then
  Begin
   mov_reg32_mem32(reg_edi, Arg0.MemoryAddr+4);
   mov_reg32_mem32(reg_esi, Arg0.MemoryAddr+0);

   // mem
   if (Arg1.Kind = joa_memory) Then
   Begin
    mov_reg32_imm32(reg_ecx, 0);
    mov_reg32_imm32(reg_ebx, 0);
    mov_reg8_mem8(reg_bl, Arg1.MemoryAddr);
   End Else

   // const
   if (Arg1.Kind = joa_constant) Then
   Begin
    TmpInt := VMInt(ord(VMChar(Arg1.Constant)));

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

  // icmul(invalid)
  Begin
   InvalidOpcodeException;
  End;
 End;

 { icdiv, icmod }
 jo_icdiv, jo_icmod:
 Begin
  // opcode(mem, const/mem)
  if (CheckArgs([joa_memory], [joa_memory, joa_constant])) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(Arg0.MemoryAddr));

   if (Arg1.Kind = joa_memory) Then
   Begin
    mov_reg32_imm32(reg_edx, 0);
    mov_reg32_imm32(reg_ecx, 0);
    mov_reg8_mem8(reg_dl, Arg1.MemoryAddr);
   End Else
   Begin
    TmpInt := VMInt(ord(VMChar(Arg1.Constant)));

    mov_reg32_imm32(reg_edx, lo(TmpInt));
    mov_reg32_imm32(reg_ecx, hi(TmpInt));
   End;

   if (Opcode.Kind = jo_icdiv) Then
    call_internalproc(@r__div_imem_iconst) Else
    call_internalproc(@r__mod_imem_iconst);
  End Else

  // opcode(invalid)
  Begin
   InvalidOpcodeException;
  End;
 End;
