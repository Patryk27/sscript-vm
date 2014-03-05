 { jmp, tjmp, fjmp, call }
 jo_jmp, jo_tjmp, jo_fjmp, jo_call:
 Begin
  if (Arg0.Kind <> joa_constant) Then // check parameter type
   InvalidOpcodeException;

  {$DEFINE JTR := JumpsToResolve}
  {$DEFINE Last := JTR[High(JTR)]}
  SetLength(JTR, Length(JTR)+1);
  Last.Opcode       := Opcode.Kind;
  Last.JumpAddress  := Arg0.Constant;
  Last.DataPosition := JumpTable.getLast.CodeAddress;
  {$UNDEF JTR}
  {$UNDEF Last}

  Case Opcode.Kind of
   jo_jmp          : I := asm_jmp_size;
   jo_call         : I := asm_call_size;
   jo_tjmp, jo_fjmp: I := asm_condjmp_size;

   else
    InvalidOpcodeException;
  End;

  For I := 1 To I Do
   nop;
 End;

 { ret }
 jo_ret:
 Begin
  mov_reg32_imm32(reg_eax, uint32(getVM));
  call_internalproc(@r__pop_reference);
  jmp(reg_eax);
 End;

 { icall }
 jo_icall:
 Begin
  if (Arg0.Kind <> joa_memory) Then // check parameter type
   InvalidOpcodeException;

  // fetch pointer
  icall := Arg0.MemoryAddr;

  // prepare parameter list
  mov_reg32_imm32(reg_eax, uint32(getVM));
  mov_reg32_imm32(reg_edx, uint32(icall));
  mov_reg32_imm32(reg_ecx, uint32(ParamsMV));
  call_internalproc(@r__create_icall_parameter_list);

  // clean result
  mov_reg32_imm32(reg_eax, uint32(ResultMV));
  call_internalproc(@r__clean_mixedvalue);

  // call icall
  mov_reg32_imm32(reg_eax, uint32(getVM));
  mov_reg32_imm32(reg_edx, uint32(ParamsMV));
  mov_reg32_imm32(reg_ecx, uint32(ResultMV));
  call_internalproc(icall^.Handler);

  // apply result; eax is preserved from the previous call, so we don't have to assign it again
  mov_reg32_imm32(reg_edx, uint32(ResultMV));
  call_internalproc(@r__apply_mixedvalue);
 End;

 { stop }
 jo_stop:
 Begin
  ret;
 End;
