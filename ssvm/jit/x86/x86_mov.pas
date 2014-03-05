 { bbmov }
 jo_bbmov:
 Begin
  // bbmov(mem, const)
  if (CheckArgs(joa_memory, joa_constant)) Then
  Begin
   mov_mem8_imm8(Arg0.MemoryAddr, Arg1.Constant);
  End Else

  // bbmov(mem, mem)
  if (CheckArgs(joa_memory, joa_memory)) Then
  Begin
   mov_reg8_mem8(reg_al, Arg1.MemoryAddr);
   mov_mem8_reg8(Arg0.MemoryAddr, reg_al);
  End Else

  // bbmov(mem, stackval)
  if (CheckArgs(joa_memory, joa_stackval)) Then
  Begin
   FetchBoolStackval(reg_al, Arg1.StackvalPos);
   mov_mem8_reg8(Arg0.MemoryAddr, reg_al);
  End Else

  // bbmov(stackval, const)
  if (CheckArgs(joa_stackval, joa_constant)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
   mov_reg32_imm32(reg_ecx, Arg1.Constant);

   call_internalproc(@r__set_stackval_bool);
  End Else

  // bbmov(stackval, mem)
  if (CheckArgs(joa_stackval, joa_memory)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
   mov_reg32_imm32(reg_ecx, 0);
   mov_reg8_mem8(reg_cl, Arg1.MemoryAddr);

   call_internalproc(@r__set_stackval_bool);
  End Else

   InvalidOpcodeException;
 End;

 { ccmov }
 jo_ccmov:
 Begin
  // ccmov(mem, const)
  if (CheckArgs(joa_memory, joa_constant)) Then
  Begin
   mov_mem8_imm8(Arg0.MemoryAddr, ord(Char(Arg1.Constant)));
  End Else

  // ccmov(mem, mem)
  if (CheckArgs(joa_memory, joa_memory)) Then
  Begin
   mov_reg8_mem8(reg_al, Arg1.MemoryAddr);
   mov_mem8_reg8(Arg0.MemoryAddr, reg_al);
  End Else

  // ccmov(mem, stackval)
  if (CheckArgs(joa_memory, joa_stackval)) Then
  Begin
   FetchCharStackval(reg_al, Arg1.StackvalPos);
   mov_mem8_reg8(Arg0.MemoryAddr, reg_al);
  End Else

  // ccmov(stackval, const)
  if (CheckArgs(joa_stackval, joa_constant)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
   mov_reg32_imm32(reg_ecx, ord(Char(Arg1.Constant)));

   call_internalproc(@r__set_stackval_char);
  End Else

  // ccmov(stackval, mem)
  if (CheckArgs(joa_stackval, joa_memory)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
   mov_reg32_imm32(reg_ecx, 0);
   mov_reg8_mem8(reg_cl, Arg1.MemoryAddr);

   call_internalproc(@r__set_stackval_char);
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
   mov_reg32_mem32(reg_eax, Arg1.MemoryAddr+0);
   mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+4);
   mov_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
   mov_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
  End Else

  // iimov(mem, stackval)
  if (CheckArgs(joa_memory, joa_stackval)) Then
  Begin
   FetchIntStackval(reg_eax, reg_edx, Arg1.StackvalPos);
   mov_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
   mov_mem32_reg32(Arg0.MemoryAddr+4, reg_edx);
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

 { ffmov }
 jo_ffmov:
 Begin
  // ffmov(mem, const)
  if (CheckArgs(joa_memory, joa_constant)) Then
  Begin
   MoveConstantFloatToMemory(Arg0.MemoryAddr, Arg1.Constant);
  End Else

  // ffmov(mem, mem)
  if (CheckArgs(joa_memory, joa_memory)) Then
  Begin
   // @TODO: it can be also done using 3 mov-s; consider it?
   fld_memfloat(Arg1.MemoryAddr);
   fstp_memfloat(Arg0.MemoryAddr);
  End Else

  // ffmov(mem, stackval)
  if (CheckArgs(joa_memory, joa_stackval)) Then
  Begin
   FetchFloatStackval(Arg1.StackvalPos);
   fstp_memfloat(Arg0.MemoryAddr);
  End Else

  // ffmov(stackval, const)
  if (CheckArgs(joa_stackval, joa_constant)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
   fld_memfloat(AllocateFloat(Arg1.Constant));

   call_internalproc(@r__set_stackval_float);
  End Else

  // ffmov(stackval, mem)
  if (CheckArgs(joa_stackval, joa_memory)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);
   fld_memfloat(Arg1.MemoryAddr);

   call_internalproc(@r__set_stackval_float);
  End Else

   InvalidOpcodeException;
 End;

 { ssmov }
 jo_ssmov:
 Begin
  // ssmov(mem, mem)
  if (CheckArgs(joa_memory, joa_memory)) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, uint32(Arg1.MemoryAddr));
   call_internalproc(@r__clone_string);
   mov_mem32_reg32(Arg0.MemoryAddr, reg_eax);
  End Else

   InvalidOpcodeException;
 End;
