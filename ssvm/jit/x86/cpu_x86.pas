(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 Part of the JIT compiler for x86 architecture.
*)

{
 @TODO:
 - shl/shr without calling any external procedure
}

{$MACRO ON}
{$DEFINE ov := override}

Unit CPU_x86;

 Interface
 Uses VM, Stack, VMTypes, VMStrings, Opcodes, JIT_Abstract_CPU;

 Type TRegister32  = (reg_eax=0, reg_ecx, reg_edx, reg_ebx, reg_esp, reg_ebp, reg_esi, reg_edi);
 Type TRegister16  = (reg_ax=0, reg_cx, reg_dx, reg_bx, reg_sp, reg_bp, reg_si, reg_di);
 Type TRegister8   = (reg_al=0, reg_cl, reg_dl, reg_bl, reg_ah, reg_ch, reg_dh, reg_bh);
 Type TRegisterFPU = (reg_st0=0, reg_st1, reg_st2, reg_st3, reg_st4, reg_st5, reg_st6, reg_st7);

{ TModRM }
Type TModRM =
     Bitpacked Record // ModR/M byte structure
      RM  : 0..7;
      Reg : 0..7;
      Mode: 0..3;

      {
       Mode 0: [reg]
       Mode 1: [reg+disp8]
       Mode 2: [reg+disp32]
       Mode 3: reg
      }
     End;

 { TJITCPU }
 Type TJITCPU =
      Class(TJITAbstractCPU)
       Private
        Procedure emit_modrm(const Value: TModRM);

       Private
    { >> CPU << }

        // mov [mem], ...
        Procedure asm_mov_mem8_imm8(const Mem: uint32; const Value: int8);

        Procedure asm_mov_mem16_imm16(const Mem: uint32; const Value: int16);
        Procedure asm_mov_mem16_reg16(const Mem: uint32; const Reg: TRegister16);

        Procedure asm_mov_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_mov_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // mov reg, ...
        Procedure asm_mov_reg8_mem8(const Reg: TRegister8; const Mem: uint32);

        Procedure asm_mov_reg16_mem16(const Reg: TRegister16; const Mem: uint32);

        Procedure asm_mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
        Procedure asm_mov_reg32_reg32(const RegA, RegB: TRegister32);
        Procedure asm_mov_reg32_mem32(const Reg: TRegister32; const Mem: uint32);

        // xchg
        Procedure asm_xchg_reg32_reg32(const RegA, RegB: TRegister32);

        // add
        Procedure asm_add_reg32_reg32(const RegA, RegB: TRegister32);
        Procedure asm_add_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_add_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // adc
        Procedure asm_adc_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_adc_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // sub
        Procedure asm_sub_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_sub_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // sbb
        Procedure asm_sbb_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_sbb_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // mul
        Procedure asm_mul_reg32(const Reg: TRegister32);

        // imul
        Procedure asm_imul_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
        Procedure asm_imul_reg32_reg32(const RegA, RegB: TRegister32);

        // cmp
        Procedure asm_cmp_mem8_imm8(const Mem: uint32; const Value: int8);

        Procedure asm_cmp_reg32_imm32(const Reg: TRegister32; const Value: int32);
        Procedure asm_cmp_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
        Procedure asm_cmp_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // call
        Procedure asm_call_internalproc(const Proc: Pointer; const TrashedRegister: TRegister32 = reg_eax);

        // ret
        Procedure asm_ret;

        // nop
        Procedure asm_nop;

        // or
        Procedure asm_or_mem8_imm8(const Mem: uint32; const Value: int8);
        Procedure asm_or_mem8_reg8(const Mem: uint32; const Reg: TRegister8);

        Procedure asm_or_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_or_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // xor
        Procedure asm_xor_mem8_imm8(const Mem: uint32; const Value: int8);
        Procedure asm_xor_mem8_reg8(const Mem: uint32; const Reg: TRegister8);

        Procedure asm_xor_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_xor_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // and
        Procedure asm_and_mem8_imm8(const Mem: uint32; const Value: int8);
        Procedure asm_and_mem8_reg8(const Mem: uint32; const Reg: TRegister8);

        Procedure asm_and_mem32_imm32(const Mem: uint32; const Value: int32);
        Procedure asm_and_mem32_reg32(const Mem: uint32; const Reg: TRegister32);

        // jumps
        Function asm_jmp(const Address: int32): uint32;
        Procedure asm_jmp(const Reg: TRegister32);

        Function asm_jne(const Address: int32): uint32;
        Function asm_jna(const Address: int32): uint32;
        Function asm_jnae(const Address: int32): uint32;
        Function asm_jnb(const Address: int32): uint32;
        Function asm_jnbe(const Address: int32): uint32;
        Function asm_je(const Address: int32): uint32;
        Function asm_jg(const Address: int32): uint32;
        Function asm_jl(const Address: int32): uint32;

    { >> FPU << }

        // fld
        Procedure asm_fld_memfloat(const Mem: uint32);

        // fild
        Procedure asm_fild_memint(const Mem: uint32);

        // fstp
        Procedure asm_fstp_memfloat(const Mem: uint32);

        // faddp
        Procedure asm_faddp_st0(const Reg: TRegisterFPU);

        // fsubp
        Procedure asm_fsubp_st0(const Reg: TRegisterFPU);

        // fmulp
        Procedure asm_fmulp_st0(const Reg: TRegisterFPU);

        // fdivp
        Procedure asm_fdivp_st0(const Reg: TRegisterFPU);

        // compare
        Procedure generic_int_compare(const Operation: TCompareOperation; const Number0, Number1: int64; const Addr0, Addr1: uint32; const isFirstConstant, isSecondConstant: Boolean);
//        Procedure generic_float_compare;

       Public
        // move
        Procedure move_membool_immbool(const MemAddr: uint64; const Value: Boolean); ov;

        Procedure move_memint_immint(const MemAddr: uint64; const Value: int64); ov;
        Procedure move_memint_memint(const MemAddrDst, MemAddrSrc: uint64); ov;

        Procedure move_memfloat_immfloat(const MemAddr: uint64; const Value: Float); ov;
        Procedure move_memfloat_memfloat(const MemAddrDst, MemAddrSrc: uint64); ov;
        Procedure move_memfloat_immint(const MemAddr: uint64; const Value: int64); ov;
        Procedure move_memfloat_memint(const MemAddrDst, MemAddrSrc: uint64); ov;

        // arithmetic
        Procedure arithmetic_memint_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64); ov;
        Procedure arithmetic_memint_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); ov;
        Procedure arithmetic_memfloat_immfloat(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: Float); ov;
        Procedure arithmetic_memfloat_memfloat(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); ov;
        Procedure arithmetic_memfloat_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64); ov;
        Procedure arithmetic_memfloat_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); ov;

        // bitwise
        Procedure bitwise_membool_immbool(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: Boolean); ov;
        Procedure bitwise_membool_membool(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64); ov;
        Procedure bitwise_memint_immint(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: int64); ov;
        Procedure bitwise_memint_memint(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64); ov;

        // compare
        Procedure compare_immint_immint(const Operation: TCompareOperation; const Value0, Value1: int64); ov;
        Procedure compare_immint_memint(const Operation: TCompareOperation; const Value0: int64; const NumberPnt1: uint64); ov;

        Procedure compare_memint_immint(const Operation: TCompareOperation; const NumberPnt0: uint64; const Value1: int64); ov;
        Procedure compare_memint_memint(const Operation: TCompareOperation; const NumberPnt0, NumberPnt1: uint64); ov;

        // bcpush
        Procedure bcpush_immbool(const Value: Boolean); ov;
        Procedure bcpush_immint(const Value: uint64); ov;
        Procedure bcpush_immfloat(const Value: Float); ov;
        Procedure bcpush_immstring(const Value: String); ov;

        Procedure bcpush_reg(const RegType: TBytecodeRegister; const RegAddr: uint64); ov;

        // bcpop
        Procedure bcpop_reg(const RegType: TBytecodeRegister; const RegAddr: uint64); ov;

        // icall
        Procedure do_icall(const icall: PCall; const ParamsMV, ResultMV: PMixedValue); ov;

        // jumps
        Procedure do_bcjump(const Address: uint64); ov;
        Procedure do_bccondjump(const Address: uint64; const Opcode: TOpcode_E); ov;
        Procedure do_bccall(const Address: uint64); ov;
        Procedure do_bcret; ov;

        // other
        Procedure do_nop; ov;
        Procedure do_stop; ov;

        Procedure pre_compilation; ov;
        Procedure post_compilation; ov;

        // half-properties
        Function get_bccall_size: uint8; ov;
        Function get_bcconditionaljump_size: uint8; ov;
       End;

 Implementation
Uses SysUtils, Stream;

// -------------------------------------------------------------------------- //
{$I routines.pas}
// -------------------------------------------------------------------------- //

(* TJITCPU.emit_modrm *)
Procedure TJITCPU.emit_modrm(const Value: TModRM);
Begin
 emit_uint8(puint8(@Value)^);
End;

(* TJITCPU.asm_mov_mem8_imm8 *)
{
 mv byte [mem], value
}
Procedure TJITCPU.asm_mov_mem8_imm8(const Mem: uint32; const Value: int8);
Begin
 emit_uint8($C6);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_uint8(Value);
End;

(* TJITCPU.asm_mov_mem16_imm16 *)
{
 mov word [mem], value
}
Procedure TJITCPU.asm_mov_mem16_imm16(const Mem: uint32; const Value: int16);
Begin
 emit_uint8($66);
 emit_uint8($C7);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITCPU.asm_mov_mem16_reg16 *)
{
 mov word [mem], reg
}
Procedure TJITCPU.asm_mov_mem16_reg16(const Mem: uint32; const Reg: TRegister16);
Var ModRM: TModRM;
Begin
 emit_uint8($66);

 if (Reg = reg_ax) Then
 Begin
  emit_uint8($A3);
  emit_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  emit_uint8($89);
  emit_modrm(ModRM);
  emit_int32(Mem);
 End;
End;

(* TJITCPU.asm_mov_mem32_imm32 *)
{
 mov dword [mem], value
}
Procedure TJITCPU.asm_mov_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($C7);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITCPU.asm_mov_mem32_reg32 *)
{
 mov dword [mem], reg
}
Procedure TJITCPU.asm_mov_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_eax) Then
 Begin
  emit_uint8($A3);
  emit_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  emit_uint8($89);
  emit_modrm(ModRM);
  emit_int32(Mem);
 End;
End;

(* TJITCPU.asm_mov_reg8_mem8 *)
{
 mov reg, byte [mem]
}
Procedure TJITCPU.asm_mov_reg8_mem8(const Reg: TRegister8; const Mem: uint32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_al) Then
 Begin
  emit_uint8($A0);
  emit_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  emit_uint8($8A);
  emit_modrm(ModRM);
  emit_uint32(Mem);
 End;
End;

(* TJITCPU.asm_mov_reg16_mem16 *)
{
 mov reg, word [mem]
}
Procedure TJITCPU.asm_mov_reg16_mem16(const Reg: TRegister16; const Mem: uint32);
Var ModRM: TModRM;
Begin
 emit_uint8($66);

 if (Reg = reg_ax) Then
 Begin
  emit_uint8($A1);
  emit_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  emit_uint8($8B);
  emit_modrm(ModRM);
  emit_int32(Mem);
 End;
End;

(* TJITCPU.asm_mov_reg32_imm32 *)
{
 mov reg, value
}
Procedure TJITCPU.asm_mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
Begin
 emit_uint8($B8+ord(Reg));
 emit_int32(Value);
End;

(* TJITCPU.asm_mov_reg32_reg32 *)
{
 mov regA, regB
}
Procedure TJITCPU.asm_mov_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 emit_uint8($89);
 emit_modrm(ModRM);
End;

(* TJITCPU.asm_mov_reg32_mem32 *)
{
 mov reg, dword [mem]
}
Procedure TJITCPU.asm_mov_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_eax) Then
 Begin
  emit_uint8($A1);
  emit_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  emit_uint8($8B);
  emit_modrm(ModRM);
  emit_int32(Mem);
 End;
End;

(* TJITCPU.asm_xchg_reg32_reg32 *)
{
 xchg regA, regB
}
Procedure TJITCPU.asm_xchg_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegA);
 ModRM.RM   := ord(RegB);

 emit_uint8($87);
 emit_modrm(ModRM);
End;

(* TJITCPU.asm_add_reg32_reg32 *)
{
 add regA, regB
}
Procedure TJITCPU.asm_add_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 emit_uint8($01);
 emit_modrm(ModRM);
End;

(* TJITCPU.asm_add_mem32_imm32 *)
{
 add dword [mem], value
}
Procedure TJITCPU.asm_add_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITCPU.asm_add_mem32_reg32 *)
{
 add dword [mem], reg
}
Procedure TJITCPU.asm_add_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($01);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_add_mem32_imm32 *)
{
 adc dword [mem], value
}
Procedure TJITCPU.asm_adc_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($15);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITCPU.asm_adc_mem32_reg32 *)
{
 adc dword [mem], reg
}
Procedure TJITCPU.asm_adc_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($11);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_sub_mem32_imm32 *)
{
 sub dword [mem], imm
}
Procedure TJITCPU.asm_sub_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($2D);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITCPU.asm_sub_mem32_reg32 *)
{
 sub dword [mem], reg
}
Procedure TJITCPU.asm_sub_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($29);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_sbb_mem32_imm32 *)
{
 sbb dword [mem], imm
}
Procedure TJITCPU.asm_sbb_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($1D);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITCPU.asm_sbb_mem32_reg32 *)
{
 sbb dword [mem], reg
}
Procedure TJITCPU.asm_sbb_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($19);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_mul_reg32 *)
{
 mul reg
}
Procedure TJITCPU.asm_mul_reg32(const Reg: TRegister32);
Begin
 emit_uint8($F7);
 emit_uint8($E0+ord(Reg));
End;

(* TJITCPU.asm_imul_reg32_mem32 *)
{
 imul reg, dword [mem]
}
Procedure TJITCPU.asm_imul_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($0F);
 emit_uint8($AF);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_imul_reg32_reg32 *)
{
 imul regA, regB
}
Procedure TJITCPU.asm_imul_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegA);
 ModRM.RM   := ord(RegB);

 emit_uint8($0F);
 emit_uint8($AF);
 emit_modrm(ModRM);
End;

(* TJITCPU.asm_cmp_mem8_imm8 *)
{
 cmp byte [mem], value
}
Procedure TJITCPU.asm_cmp_mem8_imm8(const Mem: uint32; const Value: int8);
Begin
 emit_uint8($80);
 emit_uint8($3D);
 emit_uint32(Mem);
 emit_int8(Value);
End;

(* TJITCPU.asm_cmp_reg32_imm32 *)
{
 cmp reg, value
}
Procedure TJITCPU.asm_cmp_reg32_imm32(const Reg: TRegister32; const Value: int32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_eax) Then
 Begin
  emit_uint8($3D);
  emit_int32(Value);
 End Else
 Begin
  ModRM.Mode := 3;
  ModRM.Reg  := 7;
  ModRM.RM   := ord(Reg);

  emit_uint8($81);
  emit_modrm(ModRM);
  emit_int32(Value);
 End;
End;

(* TJITCPU.asm_cmp_reg32_mem32 *)
{
 cmp reg, dword [mem]
}
Procedure TJITCPU.asm_cmp_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($3B);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_cmp_mem32_reg32 *)
{
 cmp dword [mem], reg
}
Procedure TJITCPU.asm_cmp_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($39);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_call_internalproc *)
{
 mov TrashedRegister, Proc
 call TrashedRegister
}
Procedure TJITCPU.asm_call_internalproc(const Proc: Pointer; const TrashedRegister: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 2;
 ModRM.RM   := ord(TrashedRegister);

 asm_mov_reg32_imm32(TrashedRegister, uint32(Proc));

 emit_uint8($FF);
 emit_modrm(ModRM);
End;

(* TJITCPU.asm_ret *)
{
 ret
}
Procedure TJITCPU.asm_ret;
Begin
 emit_uint8($C3);
End;

(* TJITCPU.asm_nop *)
{
 nop
}
Procedure TJITCPU.asm_nop;
Begin
 emit_uint8($90);
End;

(* TJITCPU.asm_or_mem8_imm8 *)
{
 or byte [mem], imm
}
Procedure TJITCPU.asm_or_mem8_imm8(const Mem: uint32; const Value: int8);
Begin
 emit_uint8($80);
 emit_uint8($0D);
 emit_uint32(Mem);
 emit_uint8(Value);
End;

(* TJITCPU.asm_or_mem8_reg8 *)
{
 or byte [mem], reg
}
Procedure TJITCPU.asm_or_mem8_reg8(const Mem: uint32; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($08);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPu.asm_or_mem32_imm32 *)
{
 or dword [mem], imm
}
Procedure TJITCPU.asm_or_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($0D);
 emit_uint32(Mem);
 emit_uint32(Value);
End;

(* TJITCPU.asm_or_mem32_reg32 *)
{
 or dword [mem], reg
}
Procedure TJITCPU.asm_or_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($09);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_xor_mem8_imm *)
{
 xor byte [mem], value
}
Procedure TJITCPU.asm_xor_mem8_imm8(const Mem: uint32; const Value: int8);
Begin
 emit_uint8($80);
 emit_uint8($35);
 emit_uint32(Mem);
 emit_uint8(Value);
End;

(* TJITCPU.asm_xor_mem8_reg8 *)
{
 xor byte [mem], reg
}
Procedure TJITCPU.asm_xor_mem8_reg8(const Mem: uint32; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($30);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_xor_mem32_imm32 *)
{
 xor dword [mem], imm
}
Procedure TJITCPU.asm_xor_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($35);
 emit_uint32(Mem);
 emit_uint32(Value);
End;

(* TJITCPU.asm_xor_mem32_reg32 *)
{
 xor dword [mem], reg
}
Procedure TJITCPU.asm_xor_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($31);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_and_mem8_imm8 *)
{
 and byte [mem], value
}
Procedure TJITCPU.asm_and_mem8_imm8(const Mem: uint32; const Value: int8);
Begin
 emit_uint8($80);
 emit_uint8($25);
 emit_uint32(Mem);
 emit_uint8(Value);
End;

(* TJITCPU.asm_and_mem8_reg8 *)
{
 and byte [mem], reg
}
Procedure TJITCPU.asm_and_mem8_reg8(const Mem: uint32; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($20);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_and_mem32_imm32 *)
{
 and dword [mem], imm
}
Procedure TJITCPU.asm_and_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($25);
 emit_uint32(Mem);
 emit_uint32(Value);
End;

(* TJITCPU.asm_and_mem32_reg32 *)
{
 and dword [mem], reg
}
Procedure TJITCPU.asm_and_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($21);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_jmp *)
{
 jmp address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jmp(const Address: int32): uint32;
Begin
 emit_uint8($E9);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_jmp *)
{
 jmp reg
}
Procedure TJITCPU.asm_jmp(const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 4;
 ModRM.RM   := ord(Reg);

 emit_uint8($FF);
 emit_modrm(ModRM);
End;

(* TJITCPU.asm_jne *)
{
 jne address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jne(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($85);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_jna *)
{
 jna address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jna(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($86);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* JITCPU.asm_jnae *)
{
 jnae address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jnae(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($82);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_jnb *)
{
 jnb address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jnb(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($83);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_jnbe *)
{
 jnbe address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jnbe(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($87);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_je *)
{
 je address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_je(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($84);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_jg *)
{
 jg address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jg(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($8F);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_jl *)
{
 jl address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITCPU.asm_jl(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($8C);
 Result := getCompiledData.Position;
 emit_int32(Address);
End;

(* TJITCPU.asm_fld_memfloat *)
{
 fld tword [mem]
}
Procedure TJITCPU.asm_fld_memfloat(const Mem: uint32);
Begin
 emit_uint8($DB);
 emit_uint8($2D);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_fild_memint *)
{
 fild qword [mem]
}
Procedure TJITCPU.asm_fild_memint(const Mem: uint32);
Begin
 emit_uint8($DF);
 emit_uint8($2D);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_fstp_memfloat *)
{
 fstp tword [mem]
}
Procedure TJITCPU.asm_fstp_memfloat(const Mem: uint32);
Begin
 emit_uint8($DB);
 emit_uint8($3D);
 emit_uint32(Mem);
End;

(* TJITCPU.asm_faddp_st0 *)
{
 faddp st0, reg
}
Procedure TJITCPU.asm_faddp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($C0 + ord(Reg));
End;

(* TJITCPU.asm_fsubp_st0 *)
{
 fsubp st0, reg
}
Procedure TJITCPU.asm_fsubp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($E8 + ord(Reg));
End;

(* TJITCPU.asm_fmulp_st0 *)
{
 fmulp st0, reg
}
Procedure TJITCPU.asm_fmulp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($C8 + ord(Reg));
End;

(* TJITCPU.asm_fdivp_st0 *)
{
 fdivp st0, reg
}
Procedure TJITCPU.asm_fdivp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($F8 + ord(Reg));
End;

(* TJITCPU.generic_int_compare *)
Procedure TJITCPU.generic_int_compare(const Operation: TCompareOperation; const Number0, Number1: int64; const Addr0, Addr1: uint32; const isFirstConstant, isSecondConstant: Boolean);
Var LabelTrue, LabelFalse, LabelOut   : uint32;
    TrueChange, FalseChange, OutChange: Array[0..2] of uint32;
    Tmp, TmpPos                       : uint32;
    CData                             : TStream;
    CompareMode                       : (cmConstConst, cmConstMem, cmMemConst, cmMemMem);
Begin
 CData := getCompiledData;

 For Tmp := Low(FalseChange) To High(FalseChange) Do
 Begin
  TrueChange[Tmp]  := 0;
  FalseChange[Tmp] := 0;
  OutChange[Tmp]   := 0;
 End;

 if (isFirstConstant) Then
 Begin
  if (isSecondConstant) Then
   CompareMode := cmConstConst Else
   CompareMode := cmConstMem;
 End Else
 Begin
  if (isSecondConstant) Then
   CompareMode := cmMemConst Else
   CompareMode := cmMemMem;
 End;

 if (CompareMode = cmConstConst) Then // special optimization case: as both numbers are constant known here, we can compare them right here
 Begin
  Case Operation of
   co_equal        : Tmp := ord(Number0 = Number1);
   co_different    : Tmp := ord(Number0 <> Number1);
   co_greater      : Tmp := ord(Number0 > Number1);
   co_greater_equal: Tmp := ord(Number0 >= Number1);
   co_lower        : Tmp := ord(Number0 < Number1);
   co_lower_equal  : Tmp := ord(Number0 <= Number1);
  End;

  asm_mov_mem8_imm8(uint32(@getVM^.Regs.b[5]), Tmp);
  Exit;
 End;

 if (CompareMode = cmMemMem) and (Addr0 = Addr1) Then // special optimization case: both addresses are the same (comparing the same number with itself)
 Begin
  Tmp := ord(Operation in [co_equal, co_greater_equal, co_lower_equal]);

  asm_mov_mem8_imm8(uint32(@getVM^.Regs.b[5]), Tmp);
  Exit;
 End;

 Case CompareMode of
  // const, const
  cmConstConst:
  Begin
   asm_mov_reg32_imm32(reg_eax, lo(Number0)); // mov eax, lo(Number0)
   asm_cmp_reg32_imm32(reg_eax, lo(Number1)); // cmp eax, lo(Number1)
  End;

  // const, mem
  cmConstMem:
  Begin
   asm_mov_reg32_imm32(reg_eax, lo(Number0)); // mov eax, lo(Number0)
   asm_cmp_reg32_mem32(reg_eax, Addr1+0); // cmp eax, dword [Addr1+0]
  End;

  // mem, const
  cmMemConst:
  Begin
   asm_mov_reg32_mem32(reg_eax, Addr0+0); // mov eax, [Addr0+0]
   asm_cmp_reg32_imm32(reg_eax, lo(Number1)); // cmp eax, lo(Number1)
  End;

  // mem, mem
  cmMemMem:
  Begin
   asm_mov_reg32_mem32(reg_eax, Addr1+0); // mov eax, [Addr1+0]
   asm_cmp_mem32_reg32(Addr0+0, reg_eax); // cmp [Addr0+0], eax
  End;
 End;

 Case Operation of // first branch
  co_equal    : FalseChange[0] := asm_jne(0); // jne @false
  co_different: TrueChange[0] := asm_jne(0); // jne @true

  co_greater, co_greater_equal:
  Begin
   TrueChange[0]  := asm_jg(0); // jg @true
   FalseChange[0] := asm_jl(0); // jl @false
  End;

  co_lower, co_lower_equal:
  Begin
   TrueChange[0]  := asm_jl(0); // jl @true
   FalseChange[0] := asm_jg(0); // jg @false
  End;

  else
   raise Exception.CreateFmt('TJITCPU.compare_memint_memint() -> unknown operation: %d', [ord(Operation)]);
 End;

 Case CompareMode of
  // const, const
  cmConstConst:
  Begin
   asm_mov_reg32_imm32(reg_eax, hi(Number0)); // mov eax, hi(Number0)
   asm_cmp_reg32_imm32(reg_eax, hi(Number1)); // cmp eax, hi(Number1)
  End;

  // const, mem
  cmConstMem:
  Begin
   asm_mov_reg32_imm32(reg_eax, hi(Number0)); // mov eax, hi(Number0)
   asm_cmp_reg32_mem32(reg_eax, Addr1+4); // cmp eax, dword [Addr1+4]
  End;

  // mem, const
  cmMemConst:
  Begin
   asm_mov_reg32_mem32(reg_eax, Addr0+4); // mov eax, [Addr0+4]
   asm_cmp_reg32_imm32(reg_eax, hi(Number1)); // cmp eax, hi(Number1)
  End;

  // mem, mem
  cmMemMem:
  Begin
   asm_mov_reg32_mem32(reg_eax, Addr1+4); // mov eax, [Addr1+0]
   asm_cmp_mem32_reg32(Addr0+4, reg_eax); // cmp [Addr0+4], eax
  End;
 End;

 Case Operation of // second branch
  co_equal        : FalseChange[1] := asm_jne(0); // jne @false
  co_different    : FalseChange[1] := asm_je(0); // je @false
  co_greater      : FalseChange[1] := asm_jna(0); // jna @false
  co_greater_equal: FalseChange[1] := asm_jnae(0); // jnae @false
  co_lower        : FalseChange[1] := asm_jnb(0); // jnb @false
  co_lower_equal  : FalseChange[1] := asm_jnbe(0); // jnbe @false
 End;

 // @true:
 LabelTrue := CData.Position;
 asm_mov_mem8_imm8(uint32(@getVM^.Regs.b[5]), 1); // mov byte [IF-register-address], 1
 OutChange[2] := asm_jmp(0); // jmp @out

 // @false:
 LabelFalse := CData.Position;
 asm_mov_mem8_imm8(uint32(@getVM^.Regs.b[5]), 0); // mov byte [IF-register-address], 0

 // @out:
 LabelOut := CData.Position;

 { replace dummy address with their real values }
 TmpPos := CData.Position;

 // replace @true
 For Tmp in TrueChange Do
  if (Tmp > 0) Then
  Begin
   CData.Position := Tmp;
   CData.write_int32(LabelTrue - Tmp - 4);
  End;

 // replace @false
 For Tmp in FalseChange Do
  if (Tmp > 0) Then
  Begin
   CData.Position := Tmp;
   CData.write_int32(LabelFalse - Tmp - 4);
  End;

 // replace @out
 For Tmp in OutChange Do
  if (Tmp > 0) Then
  Begin
   CData.Position := Tmp;
   CData.write_uint32(LabelOut - Tmp - 4);
  End;

 CData.Position := TmpPos;
End;

// -------------------------------------------------------------------------- //
(* TJITCPU.move_membool_immbool *)
{
 mov byte [MemAddr], Value
}
Procedure TJITCPU.move_membool_immbool(const MemAddr: uint64; const Value: Boolean);
Begin
 asm_mov_mem8_imm8(MemAddr, ord(Value));
End;

(* TJITCPU.move_memint_immint *)
{
 mov int32 [MemAddr+0], lo(Value)
 mov int32 [MemAddr+4], hi(Value)
}
Procedure TJITCPU.move_memint_immint(const MemAddr: uint64; const Value: int64);
Begin
 asm_mov_mem32_imm32(MemAddr+0, lo(Value));
 asm_mov_mem32_imm32(MemAddr+4, hi(Value));
End;

(* TJITCPU.move_memint_memint *)
{
 mov eax, [MemAddressSrc+0]
 mov ebx, [MemAddressSrc+4]
 mov [MemAddressDst+0], eax
 mov [MemAddressDst+4], ebx
}
Procedure TJITCPU.move_memint_memint(const MemAddrDst, MemAddrSrc: uint64);
Begin
 asm_mov_reg32_mem32(reg_eax, MemAddrSrc+0);
 asm_mov_reg32_mem32(reg_ebx, MemAddrSrc+4);
 asm_mov_mem32_reg32(MemAddrDst+0, reg_eax);
 asm_mov_mem32_reg32(MemAddrDst+4, reg_ebx);
End;

(* TJITCPU.move_memfloat_immfloat *)
{
 fld tword [Value]
 fstp tword [MemAddr]
}
Procedure TJITCPU.move_memfloat_immfloat(const MemAddr: uint64; const Value: Float);
Begin
 move_memfloat_memfloat(MemAddr, AllocateFloat(Value));
End;

(* TJITCPU.move_memfloat_memfloat *)
{
 mov eax, dword [MemAddrSrc+0]
 mov ebx, dword [MemAddrSrc+4]
 mov cx, word [MemAddrSrc+8]
 mov dword [MemAddrDst+0], eax
 mov dword [MemAddrDst+4], ebx
 mov word [MemAddrDst+8], cx
}
Procedure TJITCPU.move_memfloat_memfloat(const MemAddrDst, MemAddrSrc: uint64);
Begin
 asm_mov_reg32_mem32(reg_eax, MemAddrSrc+0);
 asm_mov_reg32_mem32(reg_ebx, MemAddrSrc+4);
 asm_mov_reg16_mem16(reg_cx, MemAddrSrc+8);
 asm_mov_mem32_reg32(MemAddrDst+0, reg_eax);
 asm_mov_mem32_reg32(MemAddrDst+4, reg_ebx);
 asm_mov_mem16_reg16(MemAddrDst+8, reg_cx);
End;

(* TJITCPU.move_memfloat_immint *)
{
 mov dword [MemAddrSrc+0], first 4 bits of Value
 mov dword [MemAddrSrc+4], next 4 bits of Value
 mov word [MemAddrSrc+8], next(last) 2 bits of Value
}
Procedure TJITCPU.move_memfloat_immint(const MemAddr: uint64; const Value: int64);
Type PFloat = ^TFloat;
     TFloat =
     Packed Record
      P1, P2: int32;
      P3    : int16;
     End;
Var Float: PFloat;
Begin
 PExtended(@Float)^ := Value;

 asm_mov_mem32_imm32(MemAddr+0, Float^.P1);
 asm_mov_mem32_imm32(MemAddr+4, Float^.P2);
 asm_mov_mem16_imm16(MemAddr+8, Float^.P3);
End;

(* TJITCPU.move_memfloat_memint *)
{
 fild qword [MemAddrSrc]
 fstp tword [MemAddrDst]
}
Procedure TJITCPU.move_memfloat_memint(const MemAddrDst, MemAddrSrc: uint64);
Begin
 asm_fild_memint(MemAddrSrc);
 asm_fstp_memfloat(MemAddrDst);
End;

(* TJITCPU.arithmetic_memint_immint *)
{
 Add:
 > add [MemAddrDst+0], lo(Value)
 > adc [MemAddrDst+4], hi(Value)

 Sub:
 > sub [MemAddrDst+0], lo(Value)
 > sbb [MemAddrDst+4], hi(Value)

 Mul:
 > mov edi, [MemAddrDst+4]
 > mov esi, [MemAddrDst+0]
 > mov ecx, hi(Value)
 > mov ebx, lo(Value)
 >
 > mov eax, edi
 > mul ebx
 > xchg eax, ebx
 > mul esi
 > xchg esi, eax
 > add ebx, edx
 > mul ecx
 > add ebx, eax
 >
 > mov [MemAddrDst+4], ebx
 > mov [MemAddrDst+0], esi

 Div:
 > mov eax, MemAddrDst
 > mov edx, lo(Value)
 > mov ecx, hi(Value)
 > mov ebx, r__div_memint_immint
 > call ebx
}
Procedure TJITCPU.arithmetic_memint_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64);
Begin
 Case Operation of
  { add }
  ao_add:
  Begin
   asm_add_mem32_imm32(MemAddrDst+0, lo(Value));
   asm_adc_mem32_imm32(MemAddrDst+4, hi(Value));
  End;

  { sub }
  ao_sub:
  Begin
   asm_sub_mem32_imm32(MemAddrDst+0, lo(Value));
   asm_sbb_mem32_imm32(MemAddrDst+4, hi(Value));
  End;

  { mul }
  ao_mul:
  Begin
   asm_mov_reg32_mem32(reg_edi, MemAddrDst+4);
   asm_mov_reg32_mem32(reg_esi, MemAddrDst+0);
   asm_mov_reg32_imm32(reg_ecx, hi(Value));
   asm_mov_reg32_imm32(reg_ebx, lo(Value));

   asm_mov_reg32_reg32(reg_eax, reg_edi);
   asm_mul_reg32(reg_ebx);
   asm_xchg_reg32_reg32(reg_eax, reg_ebx);
   asm_mul_reg32(reg_esi);
   asm_xchg_reg32_reg32(reg_esi, reg_eax);
   asm_add_reg32_reg32(reg_ebx, reg_edx);
   asm_mul_reg32(reg_ecx);
   asm_add_reg32_reg32(reg_ebx, reg_eax);

   asm_mov_mem32_reg32(MemAddrDst+4, reg_ebx);
   asm_mov_mem32_reg32(MemAddrDst+0, reg_esi);
  End;

  { div }
  ao_div:
  Begin
   asm_mov_reg32_imm32(reg_eax, MemAddrDst);
   asm_mov_reg32_imm32(reg_edx, lo(Value));
   asm_mov_reg32_imm32(reg_ecx, hi(Value));
   asm_call_internalproc(@r__div_memint_immint, reg_ebx);
  End;

  { invalid operation }
  else
   raise Exception.CreateFmt('TJITCPU.arithmetic_memint_immint() -> invalid operation: %d', [ord(Operation)]);
 End;
End;

(* TJITCPU.arithmetic_memint_memint *)
{
 Add:
 > mov eax, [MemAddrSrc+0]
 > mov ebx, [MemAddrSrc+4]
 > add [MemAddrDst+0], eax
 > adc [MemAddrDst+4], ebx

 Sub:
 > mov eax, [MemAddrSrc+0]
 > mov ebx, [MemAddrSrc+4]
 > sub [MemAddrDst+0], eax
 > sbb [MemAddrDst+4], ebx

 Mul:
 > mov edi, [MemAddrDst+4]
 > mov esi, [MemAddrDst+0]
 > mov ecx, [MemAddrSrc+4]
 > mov ebx, [MemAddrSrc+0]
 >
 > mov eax, edi
 > mul ebx
 > xchg eax, ebx
 > mul esi
 > xchg esi, eax
 > add ebx, edx
 > mul ecx
 > add ebx, eax
 >
 > mov [MemAddrDst+4], ebx
 > mov [MemAddrDst+0], esi

 Div:
 > mov eax, MemAddrDst
 > mov edx, [MemAddrSrc+0]
 > mov ecx, [MemAddrSrc+4]
 > mov ebx, r__div_memint_immint
 > call ebx
}
Procedure TJITCPU.arithmetic_memint_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64);
Begin
 Case Operation of
  { add }
  ao_add:
  Begin
   asm_mov_reg32_mem32(reg_eax, MemAddrSrc+0);
   asm_mov_reg32_mem32(reg_ebx, MemAddrSrc+4);
   asm_add_mem32_reg32(MemAddrDst+0, reg_eax);
   asm_adc_mem32_reg32(MemAddrDst+4, reg_ebx);
  End;

  { sub }
  ao_sub:
  Begin
   asm_mov_reg32_mem32(reg_eax, MemAddrSrc+0);
   asm_mov_reg32_mem32(reg_ebx, MemAddrSrc+4);
   asm_sub_mem32_reg32(MemAddrDst+0, reg_eax);
   asm_sbb_mem32_reg32(MemAddrDst+4, reg_ebx);
  End;

  { mul }
  ao_mul:
  Begin
   asm_mov_reg32_mem32(reg_edi, MemAddrDst+4);
   asm_mov_reg32_mem32(reg_esi, MemAddrDst+0);
   asm_mov_reg32_mem32(reg_ecx, MemAddrSrc+4);
   asm_mov_reg32_mem32(reg_ebx, MemAddrSrc+0);

   asm_mov_reg32_reg32(reg_eax, reg_edi);
   asm_mul_reg32(reg_ebx);
   asm_xchg_reg32_reg32(reg_eax, reg_ebx);
   asm_mul_reg32(reg_esi);
   asm_xchg_reg32_reg32(reg_esi, reg_eax);
   asm_add_reg32_reg32(reg_ebx, reg_edx);
   asm_mul_reg32(reg_ecx);
   asm_add_reg32_reg32(reg_ebx, reg_eax);

   asm_mov_mem32_reg32(MemAddrDst+4, reg_ebx);
   asm_mov_mem32_reg32(MemAddrDst+0, reg_esi);
  End;

  { div }
  ao_div:
  Begin
   asm_mov_reg32_imm32(reg_eax, MemAddrDst);
   asm_mov_reg32_mem32(reg_edx, MemAddrSrc+0);
   asm_mov_reg32_mem32(reg_ecx, MemAddrSrc+4);
   asm_call_internalproc(@r__div_memint_immint, reg_ebx);
  End;

  { invalid operation }
  else
   raise Exception.CreateFmt('TJITCPU.arithmetic_memint_memint() -> invalid operation: %d', [ord(Operation)]);
 End;
End;

(* TJITCPU.arithmetic_memfloat_immfloat *)
Procedure TJITCPU.arithmetic_memfloat_immfloat(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: Float);
Begin
 arithmetic_memfloat_memfloat(Operation, MemAddrDst, AllocateFloat(Value));
End;

(* TJITCPU.arithmetic_memfloat_memfloat *)
{
 fld [MemAddrDst]
 fld [MemAddrSrc]

 Add:
 > faddp st0, st1

 Sub:
 > fsubp st0, st1

 Mul:
 > fmulp st0, st1

 Div:
 > fdivp st0, st1

 fstp [MemAddrDst]
}
Procedure TJITCPU.arithmetic_memfloat_memfloat(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64);
Begin
 asm_fld_memfloat(MemAddrDst);
 asm_fld_memfloat(MemAddrSrc);

 Case Operation of
  { add }
  ao_add: asm_faddp_st0(reg_st1);

  { sub }
  ao_sub: asm_fsubp_st0(reg_st1);

  { mul }
  ao_mul: asm_fmulp_st0(reg_st1);

  { div }
  ao_div: asm_fdivp_st0(reg_st1);

  { invalid operation }
  else
   raise Exception.CreateFmt('TJITCPU.arithmetic_memfloat_memfloat() -> invalid operation: %d', [ord(Operation)]);
 End;

 asm_fstp_memfloat(MemAddrDst);
End;

(* TJITCPU.arithmetic_memfloat_immint *)
Procedure TJITCPU.arithmetic_memfloat_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64);
Begin
 arithmetic_memfloat_immfloat(Operation, MemAddrDst, Float(Value));
End;

(* TJITCPU.arithmetic_memfloat_memint *)
{
 fld [MemAddrDst]
 fild [MemAddrSrc]

 Add:
 > faddp st0, st1

 Sub:
 > fsubp st0, st1

 Mul:
 > fmulp st0, st1

 Div:
 > fdivp st0, st1

 fstp [MemAddrDst]
}
Procedure TJITCPU.arithmetic_memfloat_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64);
Begin
 asm_fld_memfloat(MemAddrDst);
 asm_fild_memint(MemAddrSrc);

 Case Operation of
  { add }
  ao_add: asm_faddp_st0(reg_st1);

  { sub }
  ao_sub: asm_fsubp_st0(reg_st1);

  { mul }
  ao_mul: asm_fmulp_st0(reg_st1);

  { div }
  ao_div: asm_fdivp_st0(reg_st1);

  { invalid operation }
  else
   raise Exception.CreateFmt('TJITCPU.arithmetic_memfloat_memint() -> invalid operation: %d', [ord(Operation)]);
 End;

 asm_fstp_memfloat(MemAddrDst);
End;

(* TJITCPU.bitwise_membool_immbool *)
{
 Or:
 > or [MemAddrDst], Value

 Xor:
 > xor [MemAddrDst], Value

 And:
 > and [MemAddrDst], Value
}
Procedure TJITCPU.bitwise_membool_immbool(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: Boolean);
Begin
 Case Operation of
  { or }
  bo_or: asm_or_mem8_imm8(MemAddrDst, ord(Value));

  { xor }
  bo_xor: asm_xor_mem8_imm8(MemAddrDst, ord(Value));

  { and }
  bo_and: asm_and_mem8_imm8(MemAddrDst, ord(Value));

  { invalid operation }
  else
   raise Exception.CreateFmt('TJITCPU.bitwise_membool_immbool() -> invalid operation: %d', [ord(Operation)]);
 End;
End;

(* TJITCPU.bitwise_membool_membool *)
{
 mov al, byte [MemAddrSrc]

 Or:
 > or byte [MemAddrDst], al

 Xor:
 > xor byte [MemAddrDst], al

 And:
 > and byte [MemAddrDst], al
}
Procedure TJITCPU.bitwise_membool_membool(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64);
Begin
 asm_mov_reg8_mem8(reg_al, MemAddrSrc);

 Case Operation of
  { or }
  bo_or: asm_or_mem8_reg8(MemAddrDst, reg_al);

  { xor }
  bo_xor: asm_xor_mem8_reg8(MemAddrDst, reg_al);

  { and }
  bo_and: asm_and_mem8_reg8(MemAddrDst, reg_al);

  { invalid operation }
  else
   raise Exception.CreateFmt('TJITCPU.bitwise_membool_membool() -> invalid operation: %d', [ord(Operation)]);
 End;
End;

(* TJITCPU.bitwise_memint_immint *)
{
 Or:
 > or [MemAddrDst+0], lo(Value)
 > or [MemAddrDst+4], hi(Value)

 Xor:
 > xor [MemAddrDst+0], lo(Value)
 > xor [MemAddrDst+4], hi(Value)

 And:
 > and [MemAddrDst+0], lo(Value)
 > and [MemAddrDst+4], hi(Value)

 Shl:
 > mov eax, MemAddrDst
 > mov edx, lo(Value)
 > mov ecx, hi(Value)
 > mov ebx, r__shl_memint_immint
 > call ebx

 Shr:
 > mov eax, MemAddrDst
 > mov edx, lo(Value)
 > mov ecx, hi(Value)
 > mov ebx, r__shr_memint_immint
 > call ebx
}
Procedure TJITCPU.bitwise_memint_immint(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: int64);
Var Proc: Procedure(const MemAddr: uint32; const Value: int32) of object;
Begin
 if (Operation in [bo_shl, bo_shr]) Then
 Begin
  asm_mov_reg32_imm32(reg_eax, MemAddrDst);
  asm_mov_reg32_imm32(reg_edx, lo(Value));
  asm_mov_reg32_imm32(reg_ecx, hi(Value));

  if (Operation = bo_shl) Then
   asm_call_internalproc(@r__shl_memint_immint, reg_ebx) Else
   asm_call_internalproc(@r__shr_memint_immint, reg_ebx);
 End Else
 Begin
  Case Operation of
   bo_or : Proc := @asm_or_mem32_imm32;
   bo_xor: Proc := @asm_xor_mem32_imm32;
   bo_and: Proc := @asm_and_mem32_imm32;

   else
    raise Exception.CreateFmt('TJITCPU.bitwise_memint_immint() -> invalid operation: %d', [ord(Operation)]);
  End;

  Proc(MemAddrDst+0, lo(Value));
 End;
 Proc(MemAddrDst+4, hi(Value));
End;

(* TJITCPU.bitwise_memint_memint *)
{
 Shl:
 > mov eax, MemAddrDst
 > mov edx, [MemAddrSrc+0]
 > mov ecx, [MemAddrSrc+4]
 > mov ebx, r__shl_memint_immint
 > call ebx

 Shr:
 > mov eax, MemAddrDst
 > mov edx, [MemAddrSrc+0]
 > mov ecx, [MemAddrSrc+4]
 > mov ebx, r__shr_memint_immint
 > call ebx

 -- For any other operations: --

 mov eax, [MemAddrSrc+0]
 mov ebx, [MemAddrSrc+4]

 Or:
 > or [MemAddrDst+0], eax
 > or [MemAddrDst+4], ebx

 Xor:
 > xor [MemAddrDst+0], eax
 > xor [MemAddrDst+4], ebx

 And:
 > and [MemAddrDst+0], eax
 > and [MemAddrDst+4], ebx
}
Procedure TJITCPU.bitwise_memint_memint(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64);
Var Proc: Procedure(const MemAddr: uint32; const Reg: TRegister32) of object;
Begin
 if (Operation in [bo_shr, bo_shl]) Then
 Begin
  asm_mov_reg32_imm32(reg_eax, MemAddrDst);
  asm_mov_reg32_mem32(reg_edx, MemAddrSrc+0);
  asm_mov_reg32_mem32(reg_ecx, MemAddrSrc+4);

  if (Operation = bo_shl) Then
   asm_call_internalproc(@r__shl_memint_immint, reg_ebx) Else
   asm_call_internalproc(@r__shr_memint_immint, reg_ebx);
 End Else
 Begin
  asm_mov_reg32_mem32(reg_eax, MemAddrSrc+0);
  asm_mov_reg32_mem32(reg_ebx, MemAddrSrc+4);

  Case Operation of
   bo_or : Proc := @asm_or_mem32_reg32;
   bo_xor: Proc := @asm_xor_mem32_reg32;
   bo_and: Proc := @asm_and_mem32_reg32;

   else
    raise Exception.CreateFmt('TJITCPU.bitwise_memint_memint() -> invalid operation: %d', [ord(Operation)]);
  End;

  Proc(MemAddrDst+0, reg_eax);
  Proc(MemAddrDst+4, reg_ebx);
 End;
End;

(* TJITCPU.compare_immint_immint *)
Procedure TJITCPU.compare_immint_immint(const Operation: TCompareOperation; const Value0, Value1: int64);
Begin
 generic_int_compare(Operation, Value0, Value1, 0, 0, True, True);
End;

(* TJITCPU.compare_immint_memint *)
Procedure TJITCPU.compare_immint_memint(const Operation: TCompareOperation; const Value0: int64; const NumberPnt1: uint64);
Begin
 generic_int_compare(Operation, Value0, 0, 0, NumberPnt1, True, False);
End;

(* TJITCPU.compare_memint_immint *)
Procedure TJITCPU.compare_memint_immint(const Operation: TCompareOperation; const NumberPnt0: uint64; const Value1: int64);
Begin
 generic_int_compare(Operation, 0, Value1, NumberPnt0, 0, False, True);
End;

(* TJITCPU.compare_memint_memint *)
Procedure TJITCPU.compare_memint_memint(const Operation: TCompareOperation; const NumberPnt0, NumberPnt1: uint64);
Begin
 generic_int_compare(Operation, 0, 0, NumberPnt0, NumberPnt1, False, False);
End;

(* TJITCPU.bcpush_immbool *)
{
 mov eax, <VM instance address>
 mov edx, Value
 mov ebx, r__push_bool
 call ebx
}
Procedure TJITCPU.bcpush_immbool(const Value: Boolean);
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, ord(Value));
 asm_call_internalproc(@r__push_bool, reg_ebx);
End;

(* TJITCPU.bcpush_immint *)
{
 mov eax, <VM instance address>
 mov edx, lo(Value)
 mov ecx, hi(Value)
 mov ebx, r__push_int
 call ebx
}
Procedure TJITCPU.bcpush_immint(const Value: uint64);
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, lo(Value));
 asm_mov_reg32_imm32(reg_ecx, hi(Value));
 asm_call_internalproc(@r__push_int, reg_ebx);
End;

(* TJITCPU.bcpush_immfloat *)
Procedure TJITCPU.bcpush_immfloat(const Value: Float);
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM)); // mov eax, <VM instance address>
 asm_mov_reg32_imm32(reg_edx, AllocateFloat(Value)); // mov edx, <Value>
 asm_call_internalproc(@r__push_float_mem, reg_ebx);
End;

(* TJITCPU.bcpush_immstring *)
Procedure TJITCPU.bcpush_immstring(const Value: String);
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM)); // mov eax, <VM instance address>
 asm_mov_reg32_imm32(reg_edx, AllocateString(Value)); // mov edx, <Value>
 asm_call_internalproc(@r__push_string, reg_ebx);
End;

(* TJITCPU.bcpush_reg *)
{
 mov eax, <VM instance address>

 reg_eb:
 > mov edx, 0
 > mov dl, byte [RegAddr]
 > mov ebx, r__push_bool
 > call ebx

 reg_ec:
 > @TODO

 reg_ei:
 > mov edx, dword [RegAddr+0]
 > mov ecx, dword [RegAddr+4]
 > mov ebx, r__push_int
 > call ebx

 reg_ef:
 > mov edx, RegAddr
 > mov ecx, r__push_float_mem
 > call ecx

 reg_es:
 > @TODO

 reg_er:
 > mov edx, dword [RegAddr]
 > mov ecx, r__push_reference
 > call ebx
}
Procedure TJITCPU.bcpush_reg(const RegType: TBytecodeRegister; const RegAddr: uint64);
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));

 Case RegType of
  { eb* }
  reg_eb:
  Begin
   asm_mov_reg32_imm32(reg_edx, 0);
   asm_mov_reg8_mem8(reg_dl, RegAddr);
   asm_call_internalproc(@r__push_bool, reg_ebx);
  End;

  { ec* }
  reg_ec:
  Begin
   raise Exception.Create('TJITCPU.bcpush_reg() -> unimplemented!');
  End;

  { ei* }
  reg_ei:
  Begin
   asm_mov_reg32_mem32(reg_edx, RegAddr+0);
   asm_mov_reg32_mem32(reg_ecx, RegAddr+4);
   asm_call_internalproc(@r__push_int, reg_ebx);
  End;

  { ef* }
  reg_ef:
  Begin
   asm_mov_reg32_imm32(reg_edx, RegAddr);
   asm_call_internalproc(@r__push_float_mem, reg_ecx);
  End;

  { es* }
  reg_es:
  Begin
   raise Exception.Create('TJITCPU.bcpush_reg() -> unimplemented!');
  End;

  { er* }
  reg_er:
  Begin
   asm_mov_reg32_mem32(reg_edx, RegAddr);
   asm_call_internalproc(@r__push_reference, reg_ecx);
  End;

  { invalid register type }
  else
   raise Exception.CreateFmt('TJITCPU.bcpush_reg() -> invalid ''RegType'' = ''%d''', [ord(RegType)]);
 End;
End;

(* TJITCPU.bcpop_reg *)
{
 mov eax, <VM instance address>
 mov edx, RegAddr
 mov ecx, r__pop_<regtype>_reg
 call ecx
}
Procedure TJITCPU.bcpop_reg(const RegType: TBytecodeRegister; const RegAddr: uint64);
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, RegAddr);

 Case RegType of
  reg_eb: asm_call_internalproc(@r__pop_bool_reg, reg_ecx);
  reg_ec: asm_call_internalproc(@r__pop_char_reg, reg_ecx);
  reg_ei: asm_call_internalproc(@r__pop_int_reg, reg_ecx);
  reg_ef: asm_call_internalproc(@r__pop_float_reg, reg_ecx);
  reg_es: asm_call_internalproc(@r__pop_string_reg, reg_ecx);
  reg_er: asm_call_internalproc(@r__pop_reference_reg, reg_ecx);

  else
   raise Exception.CreateFmt('TJITCPU.bcpop_reg() -> invalid `RegType` = %d', [ord(RegType)]);
 End;
End;

(* TJITCPU.do_icall *)
{
 mov eax, <VM instance address>
 mov edx, icall
 mov ecx, ParamsMV
 mov ebx, r__create_icall_parameter_list
 call ebx

 mov eax, ResultMV
 mov ebx, r__clean_mixedvalue
 call ebx

 mov eax, <VM instance address>
 mov edx, ParamsMV
 mov ecx, ResultMV
 mov ebx, icall^.Handler
 call ebx

 mov eax, <VM instance address>
 mov edx, ResultMV
 mov ebx, r__apply_mixedvalue
 call ebx
}
Procedure TJITCPU.do_icall(const icall: PCall; const ParamsMV, ResultMV: PMixedValue);
Begin
 // prepare parameter list
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, uint32(icall));
 asm_mov_reg32_imm32(reg_ecx, uint32(ParamsMV));
 asm_call_internalproc(@r__create_icall_parameter_list, reg_ebx);

 // clean result (ResultMV)
 asm_mov_reg32_imm32(reg_eax, uint32(ResultMV));
 asm_call_internalproc(@r__clean_mixedvalue, reg_ebx);

 // call icall
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, uint32(ParamsMV));
 asm_mov_reg32_imm32(reg_ecx, uint32(ResultMV));
 asm_call_internalproc(icall^.Handler, reg_ebx);

 // apply result
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, uint32(ResultMV));
 asm_call_internalproc(@r__apply_mixedvalue, reg_ebx);
End;

(* TJITCPU.do_relative_jump *)
Procedure TJITCPU.do_bcjump(const Address: uint64);
Begin
 asm_jmp(Address - getCompiledData.Position - 5);
End;

(* TJITCPU.do_bccond_jump *)
Procedure TJITCPU.do_bccondjump(const Address: uint64; const Opcode: TOpcode_E);
Begin
 asm_cmp_mem8_imm8(uint32(@getVM^.Regs.b[5]), ord(Opcode = o_tjmp)); // cmp [IF-register-address], /1 or 0/

 asm_jne(5); // `5` is the size of `jmp`
 do_bcjump(Address);

 {
  @Note: we're using here a simple far-conditonal-jump trick:

  jne dont
  jmp Address

  dont:
  ...
 }
End;

(* TJITCPU.do_bccall *)
Procedure TJITCPU.do_bccall(const Address: uint64);
Var RetEIP: uint32;
Begin
 RetEIP := uint32(getCompiledData.Memory) + getCompiledData.Position + 22;

 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_mov_reg32_imm32(reg_edx, RetEIP);
 asm_call_internalproc(@r__push_reference, reg_ecx);

 do_bcjump(Address);
End;

(* TJITCPU.do_bcret *)
{
 mov eax, <VM instance address>
 mov edx, r__pop_reference
 call edx

 jmp eax
}
Procedure TJITCPU.do_bcret;
Begin
 asm_mov_reg32_imm32(reg_eax, uint32(getVM));
 asm_call_internalproc(@r__pop_reference, reg_edx);
 asm_jmp(reg_eax);
End;

(* TJITCPu.do_nop *)
{
 nop
}
Procedure TJITCPU.do_nop;
Begin
 asm_nop;
End;

(* TJITCPU.do_stop *)
{
 ret
}
Procedure TJITCPU.do_stop;
Begin
 asm_ret;
End;

(* TJITCPU.pre_compilation *)
Procedure TJITCPU.pre_compilation;
Begin
 // nothing here //
End;

(* TJITCPU.post_compilation *)
Procedure TJITCPU.post_compilation;
Begin
 // nothing here //
End;

(* TJITCPU.get_bccall_size *)
Function TJITCPU.get_bccall_size: uint8;
Begin
 Result := 13;
End;

(* TJITCPU.get_bcconditionaljump_size *)
Function TJITCPU.get_bcconditionaljump_size: uint8;
Begin
 Result := 9;
End;
End.
