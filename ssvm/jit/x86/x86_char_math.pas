 { ccadd, ciadd, ccsub, cisub }
 jo_ccadd, jo_ciadd, jo_ccsub, jo_cisub:
 Begin
  // opcode(mem, const/mem/stackval)
  if (CheckArgs([joa_memory], [joa_constant, joa_memory, joa_stackval])) Then
  Begin
   if (Arg1.Kind in [joa_memory, joa_stackval]) Then // if both operands are memory, the second one has to be loaded into register (no double-mem-ref on x86 :()
   Begin
    if (Arg1.Kind = joa_memory) Then
     mov_reg8_mem8(reg_al, Arg1.MemoryAddr) Else
     FetchCharStackval(reg_al, Arg1.StackvalPos);

    Case Opcode.Kind of
     jo_ccadd: add_mem8_reg8(Arg0.MemoryAddr, reg_al);
     jo_ccsub: sub_mem8_reg8(Arg0.MemoryAddr, reg_al);
    End;
   End Else
   Begin
    Case Opcode.Kind of
     jo_ccadd: add_mem8_imm8(Arg0.MemoryAddr, ord(Char(Arg1.Constant)));
     jo_ccsub: sub_mem8_imm8(Arg0.MemoryAddr, ord(Char(Arg1.Constant)));
    End;
   End;
  End Else

   InvalidOpcodeException;
 End;

 { ccmul, cimul }
 jo_ccmul, jo_cimul:
 Begin
  // opcode(mem, const/mem/stackval)
  if (CheckArgs([joa_memory], [joa_constant, joa_memory, joa_stackval])) Then
  Begin
   Case Arg1.Kind of
    // constant
    joa_constant:
    Begin
     mov_reg16_imm16(reg_ax, ord(Char(Arg1.Constant)));
    End;

    // memory
    joa_memory:
    Begin
     mov_reg16_imm16(reg_ax, 0);
     mov_reg8_mem8(reg_al, Arg1.MemoryAddr);
    End;

    // stackval
    joa_stackval:
    Begin
     mov_reg16_imm16(reg_ax, 0);
     FetchCharStackval(reg_al, Arg1.StackvalPos);
    End;
   End;

   mul_mem8(Arg0.MemoryAddr);
   mov_mem8_reg8(Arg0.MemoryAddr, reg_al);
  End Else

   InvalidOpcodeException;
 End;

 { ccdiv, cidiv, ccmod, cimod }
 jo_ccdiv, jo_cidiv, jo_ccmod, jo_cimod:
 Begin
  // opcode(mem, const/mem/stackval)
  if (CheckArgs([joa_memory], [joa_constant, joa_memory, joa_stackval])) Then
  Begin
   mov_reg16_imm16(reg_ax, 0);
   mov_reg8_mem8(reg_al, Arg0.MemoryAddr);

   Case Arg1.Kind of
    // constant
    joa_constant:
    Begin
     mov_reg8_imm8(reg_dl, ord(Char(Arg1.Constant)));
     div_reg8(reg_dl);
    End;

    // memory
    joa_memory:
    Begin
     div_mem8(Arg1.MemoryAddr);
    End;

    // stackval
    joa_stackval:
    Begin
     FetchCharStackval(reg_dl, Arg1.StackvalPos);
     div_reg8(reg_dl);
    End;
   End;

   if (Opcode.Kind = jo_ccdiv) Then
    mov_mem8_reg8(Arg0.MemoryAddr, reg_al) Else
    mov_mem8_reg8(Arg0.MemoryAddr, reg_ah);
  End Else

   InvalidOpcodeException;
 End;
