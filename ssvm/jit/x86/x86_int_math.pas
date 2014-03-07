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
  // iimul(mem, mem/const/stackval)
  if (CheckArgs([joa_memory], [joa_memory, joa_constant, joa_stackval])) Then
  Begin
   mov_reg32_mem32(reg_edi, Arg0.MemoryAddr+4);
   mov_reg32_mem32(reg_esi, Arg0.MemoryAddr+0);

   if (Arg1.Kind = joa_memory) Then
   Begin
    mov_reg32_mem32(reg_ecx, Arg1.MemoryAddr+4);
    mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+0);
   End Else

   if (Arg1.Kind = joa_constant) Then
   Begin
    TmpInt := Arg1.Constant;

    mov_reg32_imm32(reg_ecx, hi(TmpInt));
    mov_reg32_imm32(reg_ebx, lo(TmpInt));
   End Else

   Begin
    FetchIntStackval(reg_ebx, reg_ecx, Arg1.StackvalPos);
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
  if (CheckArgs([joa_memory], [joa_memory, joa_constant])) Then
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

  // opcode(mem, stackval)
  if (CheckArgs(joa_memory, joa_stackval)) Then
  Begin
   FetchIntStackval(reg_edx, reg_ecx, Arg1.StackvalPos);
   mov_reg32_imm32(reg_eax, uint32(Arg0.MemoryAddr));

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
