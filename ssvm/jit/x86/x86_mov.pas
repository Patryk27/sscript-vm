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

 { ffmov }
 jo_ffmov:
 Begin
  // ffmov(mem, const)
  if (CheckArgs(joa_memory, joa_constant)) Then
  Begin
   MoveConstantFloatToMemory(Arg0.MemoryAddr, Arg1.Constant);
  End Else

  // ffmov(mem, mem) @TODO

   InvalidOpcodeException;
 End;
