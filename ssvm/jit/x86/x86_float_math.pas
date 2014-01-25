 { ffadd, ffsub, ffmul, ffdiv }
 jo_ffadd, jo_ffsub, jo_ffmul, jo_ffdiv:
 Begin
  // opcode(mem, mem/const/stackval)
  if (Arg0.Kind = joa_memory) and (Arg1.Kind in [joa_memory, joa_constant, joa_stackval]) Then
  Begin
   fld_memfloat(Arg0.MemoryAddr);

   Case Arg1.Kind of
    joa_memory  : fld_memfloat(Arg1.MemoryAddr);
    joa_constant: fld_memfloat(AllocateFloat(Arg1.Constant));
    joa_stackval: FetchFloatStackval(Arg1.StackvalPos);
   End;

   Case Opcode.Kind of
    jo_ffadd: faddp_st0(reg_st1);
    jo_ffsub: fsubp_st0(reg_st1);
    jo_ffmul: fmulp_st0(reg_st1);
    jo_ffdiv: fdivp_st0(reg_st1);
   End;

   fstp_memfloat(Arg0.MemoryAddr);
  End Else

  // opcode(stackval, const/mem)
  if (Arg0.Kind = joa_stackval) and (Arg1.Kind in [joa_constant, joa_memory]) Then
  Begin
   mov_reg32_imm32(reg_eax, uint32(getVM));
   mov_reg32_imm32(reg_edx, Arg0.StackvalPos);

   if (Arg1.Kind = joa_constant) Then
    fld_memfloat(AllocateFloat(Arg1.Constant)) Else
    fld_memfloat(Arg1.MemoryAddr);

   Case Opcode.Kind of
    jo_ffadd: call_internalproc(@r__add_stackval_float);
    jo_ffsub: call_internalproc(@r__sub_stackval_float);
    jo_ffmul: call_internalproc(@r__mul_stackval_float);
    jo_ffdiv: call_internalproc(@r__div_stackval_float);
   End;
  End Else

   InvalidOpcodeException;
 End;
