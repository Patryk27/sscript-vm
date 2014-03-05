 { fiadd, fisub, fimul, fidiv }
 jo_fiadd, jo_fisub, jo_fimul, jo_fidiv:
 Begin
  // opcode(mem, mem/const)
  if (Arg0.Kind = joa_memory) and (Arg1.Kind in [joa_memory, joa_constant]) Then
  Begin
   fld_memfloat(Arg0.MemoryAddr);

   Case Arg1.Kind of
    joa_memory  : fild_memint(Arg1.MemoryAddr);
    joa_constant: fld_memfloat(AllocateFloat(Arg1.Constant));
   End;

   Case Opcode.Kind of
    jo_fiadd: faddp_st0(reg_st1);
    jo_fisub: fsubp_st0(reg_st1);
    jo_fimul: fmulp_st0(reg_st1);
    jo_fidiv: fdivp_st0(reg_st1);
   End;

   fstp_memfloat(Arg0.MemoryAddr);
  End Else

   InvalidOpcodeException;
 End;
