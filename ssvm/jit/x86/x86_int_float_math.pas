 { ifadd, ifsub, ifmul, ifdiv }
 jo_ifadd, jo_ifsub, jo_ifmul, jo_ifdiv:
 Begin
  // opcode(mem, mem/const)
  if (Arg0.Kind = joa_memory) and (Arg1.Kind in [joa_memory, joa_constant]) Then
  Begin
   fild_memint(Arg0.MemoryAddr);

   Case Arg1.Kind of
    joa_memory  : fld_memfloat(Arg1.MemoryAddr);
    joa_constant: fld_memfloat(AllocateFloat(Arg1.Constant));
   End;

   Case Opcode.Kind of
    jo_ifadd: faddp_st0(reg_st1);
    jo_ifsub: fsubp_st0(reg_st1);
    jo_ifmul: fmulp_st0(reg_st1);
    jo_ifdiv: fdivp_st0(reg_st1);
   End;

   fistp_memint(Arg0.MemoryAddr);
  End Else

   InvalidOpcodeException;
 End;
