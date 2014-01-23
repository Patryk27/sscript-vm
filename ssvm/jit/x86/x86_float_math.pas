 { ffadd, ffsub, ffmul, ffdiv }
 jo_ffadd, jo_ffsub, jo_ffmul, jo_ffdiv:
 Begin
  // arg0
  if (Arg0.Kind = joa_memory) Then
   fld_memfloat(Arg0.MemoryAddr) Else
   InvalidOpcodeException;

  // arg1
  Case Arg1.Kind of
   joa_memory  : fld_memfloat(Arg1.MemoryAddr);
   joa_constant: fld_memfloat(AllocateFloat(Arg1.Constant));
  // joa_stackval: @TODO
   else
    InvalidOpcodeException;
  End;

  Case Opcode.Kind of
   jo_ffadd: faddp_st0(reg_st1);
   jo_ffsub: fsubp_st0(reg_st1);
   jo_ffmul: fmulp_st0(reg_st1);
   jo_ffdiv: fdivp_st0(reg_st1);
  End;

  fstp_memfloat(Arg0.MemoryAddr);
 End;
