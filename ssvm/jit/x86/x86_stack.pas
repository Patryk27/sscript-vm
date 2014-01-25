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

 { fpush }
 jo_fpush:
 Begin
  mov_reg32_imm32(reg_eax, uint32(getVM));

  Case Arg0.Kind of
   joa_constant: fld_memfloat(AllocateFloat(Arg0.Constant));
   joa_memory  : fld_memfloat(Arg0.MemoryAddr);

   else
    InvalidOpcodeException;
  End;

  call_internalproc(@r__push_float);
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
