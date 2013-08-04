(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 JIT Compiler for the x86 architecture.
*)

// @TODO: mul/div by 2, 4, 8, 16, 32, 64...
// @TODO: 'add reg, 1' -> 'inc reg'
// @TODO: 'mov reg, 0' -> 'xor reg, reg'
// @TODO: maybe instead of pushing @VM^.Stack and 'stp' wouldn't it be easier and better to push VM-address?
// @TODO: let the real x86 stack hold addresses of MixedValues? It would be A LOT better.

Unit JIT;

 Interface
 Uses JIT_Base, MTypes, Opcodes, VM, FGL;

 Type TRegister32  = (reg_eax=0, reg_ecx, reg_edx, reg_ebx, reg_esp, reg_ebp, reg_esi, reg_edi);
 Type TRegister16  = (reg_ax=0, reg_cx, reg_dx, reg_bx, reg_sp, reg_bp, reg_si, reg_di);
 Type TRegister8   = (reg_al=0, reg_cl, reg_dl, reg_bl, reg_ah, reg_ch, reg_dh, reg_bh);
 Type TRegisterFPU = (reg_st0=0, reg_st1, reg_st2, reg_st3, reg_st4, reg_st5, reg_st6, reg_st7);

 Type PJumpData = ^TJumpData;
      TJumpData = Record
                   BytecodeAddress, JumpAddress: uint32;
                  End;
 Type TJumpDataList = specialize TFPGList<PJumpData>;

 { TJumpTable }
 Type TJumpTable = Class
                    Private
                     List: TJumpDataList;

                    Public
                     Constructor Create;
                     Destructor Destroy; override;

                     Procedure AddJump(const Jump: TJumpData);
                     Procedure AddJump(const BytecodeAddress, JumpAddress: uint32);

                     Function FindJumpByBytecodeAddress(const Address: uint32): PJumpData;
                    End;

 { TJITCompiler }
 Type TJITCompiler = Class(TJITCompilerBase)
                      Private
                       Procedure asm_nop;

                       Procedure asm_push_imm32(const Value: int32);
                       Procedure asm_push_imm64(const Value: int64);

                       Procedure asm_push_mem16(const Mem: uint32);
                       Procedure asm_push_mem32(const Mem: uint32);
                       Procedure asm_push_mem64(const Mem: uint32);
                       Procedure asm_push_reg32(const Reg: TRegister32);

                       Procedure asm_mov_mem8_imm8(const Mem: uint32; const Value: int8);
                       Procedure asm_mov_reg8_mem8(const Reg: TRegister8; const Mem: uint32);
                       Procedure asm_mov_reg8_imm8(const Reg: TRegister8; const Value: int8);
                       Procedure asm_mov_mem8_reg8(const Mem: uint32; const Reg: TRegister8);
                       Procedure asm_mov_mem16_imm16(const Mem: uint32; const Value: int16);
                       Procedure asm_mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
                       Procedure asm_mov_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
                       Procedure asm_mov_reg32_reg32(const RegA: TRegister32; const RegB: TRegister32);
                       Procedure asm_mov_mem32_reg32(const Reg: TRegister32; const Reg2: TRegister32);
                       Procedure asm_mov_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
                       Procedure asm_mov_mem32_imm32(const Mem: uint32; const Value: int32);
                       Procedure asm_mov_mem32_imm64(const Mem: uint32; const Value: int64);

                       Procedure asm_absolute_call(const Addr: uint32);
                       Procedure asm_absolute_jmp(const Addr: uint32);
                       Procedure asm_relative_jne(const Addr: uint32);
                       Procedure asm_ret;

                       Procedure asm_add_reg32_imm32(const Reg: TRegister32; const Value: int32);
                       Procedure asm_add_mem32_imm32(const Mem: uint32; const Value: int32);
                       Procedure asm_sub_mem32_imm32(const Mem: uint32; const Value: int32);
                       Procedure asm_sub_reg32_reg32(const RegA: TRegister32; const RegB: TRegister32);
                       Procedure asm_sbb_reg32_imm32(const Reg: TRegister32; const Value: int32);
                       Procedure asm_sbb_reg32_imm8(const Reg: TRegister32; const Value: int8);
                       Procedure asm_cmp_reg32_reg32(const RegA, RegB: TRegister32);
                       Procedure asm_cmp_reg8_reg8(const RegA, RegB: TRegister8);
                       Procedure asm_cmp_mem32_imm32(const Mem: uint32; const Value: int32);
                       Procedure asm_sete_mem8(const Mem: uint32);
                       Procedure asm_setne_mem8(const Mem: uint32);
                       Procedure asm_setg_mem8(const Mem: uint32);
                       Procedure asm_setge_mem8(const Mem: uint32);
                       Procedure asm_setl_mem8(const Mem: uint32);
                       Procedure asm_setle_mem8(const Mem: uint32);
                       Procedure asm_setb_mem8(const Mem: uint32);
                       Procedure asm_setbe_mem8(const Mem: uint32);
                       Procedure asm_seta_mem8(const Mem: uint32);
                       Procedure asm_setae_mem8(const Mem: uint32);

                       Procedure asm_or_reg32_reg32(const RegA, RegB: TRegister32);
                       Procedure asm_and_reg32_reg32(const RegA, RegB: TRegister32);
                       Procedure asm_xor_reg32_reg32(const RegA, RegB: TRegister32);
                       Procedure asm_shr_reg32_cl(const RegA: TRegister32);

                       Procedure asm_not_reg32(const Reg: TRegister32);
                       Procedure asm_not_mem8(const Mem: uint32);
                       Procedure asm_not_mem32(const Mem: uint32);
                       Procedure asm_neg_reg32(const Reg: TRegister32);
                       Procedure asm_neg_mem32(const Mem: uint32);

                       Procedure asm_test_reg8_reg8(const RegA, RegB: TRegister8);

                       Procedure asm_fild_mem64(const Mem: uint32);
                       Procedure asm_fld_memfloat(const Mem: uint32);
                       Procedure asm_fstp_memfloat(const Mem: uint32);
                       Procedure asm_faddp_st0(const Reg: TRegisterFPU);
                       Procedure asm_fsubp_st0(const Reg: TRegisterFPU);
                       Procedure asm_fmulp_st0(const Reg: TRegisterFPU);
                       Procedure asm_fdivp_st0(const Reg: TRegisterFPU);
                       Procedure asm_fcomi_st0(const Reg: TRegisterFPU);
                       Procedure asm_fcompp;
                       Procedure asm_fstsw_ax;
                       Procedure asm_fchs;
                       Procedure asm_sahf;

                      Public
                       Procedure Compile; override;
                      End;

 Implementation
Uses Stack, mStrings, SysUtils;

Type TModRM = Bitpacked Record // ModR/M structure
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

// -------------------------------------------------------------------------- //
{$I routines.pas}
// -------------------------------------------------------------------------- //

(* TJumpTable.Create *)
Constructor TJumpTable.Create;
Begin
 List := TJumpDataList.Create;
End;

(* TJumpTable.Create *)
Destructor TJumpTable.Destroy;
Var Jump: PJumpData;
Begin
 inherited Destroy;

 For Jump in List Do
  Dispose(Jump);
 List.Free;
End;

(* TJumpTable.AddJump *)
Procedure TJumpTable.AddJump(const Jump: TJumpData);
Var PJump: PJumpData;
Begin
 New(PJump);
 PJump^ := Jump;
 List.Add(PJump);
End;

(* TJumpTable.AddJump *)
Procedure TJumpTable.AddJump(const BytecodeAddress, JumpAddress: uint32);
Var Jump: TJumpData;
Begin
 Jump.BytecodeAddress := BytecodeAddress;
 Jump.JumpAddress     := JumpAddress;
 AddJump(Jump);
End;

(* TJumpTable.FindJumpByBytecodeAddress *)
Function TJumpTable.FindJumpByBytecodeAddress(const Address: uint32): PJumpData;
Begin
 For Result in List Do
  if (Result^.BytecodeAddress = Address) Then
   Exit;

 Exit(nil);
End;

// -------------------------------------------------------------------------- //
(* TJITCompiler.asm_nop *)
{ nop }
Procedure TJITCompiler.asm_nop;
Begin
 CompiledData.write_uint8($90);
End;

(* TJITCompiler.asm_push_imm32 *)
{ push dword value }
Procedure TJITCompiler.asm_push_imm32(const Value: int32);
Begin
 CompiledData.write_uint8($68);
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_push_imm64 *)
{
 push hi(Value)
 push lo(Value)
}
Procedure TJITCompiler.asm_push_imm64(const Value: int64);
Begin
 asm_push_imm32(hi(Value));
 asm_push_imm32(lo(Value));
End;

(* TJITCompiler.asm_push_mem16 *)
{ push word [mem] }
Procedure TJITCompiler.asm_push_mem16(const Mem: uint32);
Begin
 CompiledData.write_uint8($66);
 CompiledData.write_uint8($FF);
 CompiledData.write_uint8($35);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_push_mem32 *)
{ push dword [mem] }
Procedure TJITCompiler.asm_push_mem32(const Mem: uint32);
Begin
 CompiledData.write_uint8($FF);
 CompiledData.write_uint8($35);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_push_mem64 *)
{
 push dword [mem+4]
 push dword [mem]
}
Procedure TJITCompiler.asm_push_mem64(const Mem: uint32);
Begin
 asm_push_mem32(Mem+4);
 asm_push_mem32(Mem);
End;

(* TJITCompiler.asm_push_reg32 *)
{ push dword reg }
Procedure TJITCompiler.asm_push_reg32(const Reg: TRegister32);
Begin
 CompiledData.write_uint8($50 + ord(Reg));
End;

(* TJITCompiler.asm_mov_mem8_imm8 *)
{ mov byte [mem], value }
Procedure TJITCompiler.asm_mov_mem8_imm8(const Mem: uint32; const Value: int8);
Begin
 CompiledData.write_uint8($C6);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
 CompiledData.write_uint8(Value);
End;

(* TJITCompiler.asm_mov_reg8_mem8 *)
{ mov reg, byte [mem] }
Procedure TJITCompiler.asm_mov_reg8_mem8(const Reg: TRegister8; const Mem: uint32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_al) Then
 Begin
  CompiledData.write_uint8($A0);
  CompiledData.write_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  CompiledData.write_uint8($8A);
  CompiledData.write_uint8(puint8(@ModRM)^);
  CompiledData.write_uint32(Mem);
 End;
End;

(* TJITCompiler.asm_mov_reg8_imm8 *)
{ mov reg, value }
Procedure TJITCompiler.asm_mov_reg8_imm8(const Reg: TRegister8; const Value: int8);
Begin
 CompiledData.write_uint8($B0+ord(Reg));
 CompiledData.write_int8(Value);
End;

(* TJITCompiler.asm_mov_mem8_reg8 *)
{ mov byte [mem], reg }
Procedure TJITCompiler.asm_mov_mem8_reg8(const Mem: uint32; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 0;

 CompiledData.write_uint8($88);
 CompiledData.write_uint8(puint8(@ModRM)^);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_mov_mem16_imm16 *)
{ mov word [mem], value }
Procedure TJITCompiler.asm_mov_mem16_imm16(const Mem: uint32; const Value: int16);
Begin
 CompiledData.write_uint8($66);
 CompiledData.write_uint8($C7);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
 CompiledData.write_int16(Value);
End;

(* TJITCompiler.asm_mov_reg32_imm32 *)
{ mov reg, value }
Procedure TJITCompiler.asm_mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
Begin
 CompiledData.write_uint8($B8+ord(Reg));
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_mov_reg32_mem32 *)
{ mov reg, dword [mem] }
Procedure TJITCompiler.asm_mov_reg32_mem32(const Reg: TRegister32; const Mem: uint32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_eax) Then
 Begin
  CompiledData.write_uint8($A1);
  CompiledData.write_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  CompiledData.write_uint8($8B);
  CompiledData.write_uint8(puint8(@ModRM)^);
  CompiledData.write_int32(Mem);
 End;
End;

(* TJITCompiler.asm_mov_reg32_reg32 *)
{ mov regA, regB }
Procedure TJITCompiler.asm_mov_reg32_reg32(const RegA: TRegister32; const RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 CompiledData.write_uint8($89);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_mov_mem32_reg32 *)
{ mov dword [reg], reg }
Procedure TJITCompiler.asm_mov_mem32_reg32(const Reg: TRegister32; const Reg2: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg2);
 ModRM.RM   := ord(Reg);

 CompiledData.write_uint8($89);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_mov_mem32_reg32 *)
{ mov dword [mem], reg }
Procedure TJITCompiler.asm_mov_mem32_reg32(const Mem: uint32; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 if (Reg = reg_eax) Then
 Begin
  // if register is 'eax', we can use a simpler opcode
  CompiledData.write_uint8($A3);
  CompiledData.write_uint32(Mem);
 End Else
 Begin
  ModRM.Mode := 0;
  ModRM.Reg  := ord(Reg);
  ModRM.RM   := 5;

  CompiledData.write_uint8($89);
  CompiledData.write_uint8(puint8(@ModRM)^);
  CompiledData.write_int32(Mem);
 End;
End;

(* TJITCompiler.asm_mov_mem32_imm32 *)
{ mov dword [mem], value }
Procedure TJITCompiler.asm_mov_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 CompiledData.write_uint8($C7);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_mov_mem32_imm64 *)
{
 mov dword [mem+0], lo(Value)
 mov dword [mem+4], hi(Value)
}
Procedure TJITCompiler.asm_mov_mem32_imm64(const Mem: uint32; const Value: int64);
Begin
 asm_mov_mem32_imm32(Mem, lo(Value));
 asm_mov_mem32_imm32(Mem+4, hi(Value));
End;

(* TJITCompiler.asm_absolute_call *)
{
 mov eax, addr
 call eax
}
Procedure TJITCompiler.asm_absolute_call(const Addr: uint32);
Begin
 asm_mov_reg32_imm32(reg_eax, Addr); // we need to do absolute call, not relative
 CompiledData.write_uint8($FF);
 CompiledData.write_uint8($D0);
End;

(* TJITCompiler.asm_absolute_jmp *)
{
 push addr
 ret
}
Procedure TJITCompiler.asm_absolute_jmp(const Addr: uint32);
Begin
 asm_push_imm32(Addr);
 asm_ret;
End;

(* TJITCompiler.asm_relative_jne *)
{ jne addr }
Procedure TJITCompiler.asm_relative_jne(const Addr: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($85);
 CompiledData.write_uint32(Addr);
End;

(* TJITCompiler.asm_ret *)
{ ret }
Procedure TJITCompiler.asm_ret;
Begin
 CompiledData.write_uint8($C3);
End;

(* TJITCompiler.asm_add_reg32_imm32 *)
{ add reg, imm }
Procedure TJITCompiler.asm_add_reg32_imm32(const Reg: TRegister32; const Value: int32);
Begin
 if (Value = 0) Then
  Exit;

 if (Reg = reg_eax) Then
 Begin
  CompiledData.write_uint8($05);
  CompiledData.write_int32(Value);
 End Else
 Begin
  CompiledData.write_uint8($81);
  CompiledData.write_uint8($C0 + ord(Reg));
  CompiledData.write_int32(Value);
 End;
End;

(* TJITCompiler.asm_add_mem32_imm32 *)
{ add dword [mem], imm }
Procedure TJITCompiler.asm_add_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 if (Value = 0) Then
  Exit;

 CompiledData.write_uint8($81);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_sub_mem32_imm32 *)
{ sub dword [mem], imm }
Procedure TJITCompiler.asm_sub_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 if (Value = 0) Then
  Exit;

 CompiledData.write_uint8($81);
 CompiledData.write_uint8($2D);
 CompiledData.write_uint32(Mem);
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_sub_reg32_reg32 *)
{ sub regA, regB }
Procedure TJITCompiler.asm_sub_reg32_reg32(const RegA: TRegister32; const RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 CompiledData.write_uint8($29);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_sbb_reg32_imm32 *)
{ sbb reg, value }
Procedure TJITCompiler.asm_sbb_reg32_imm32(const Reg: TRegister32; const Value: int32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 3;
 ModRM.RM   := ord(Reg);

 CompiledData.write_uint8($81);
 CompiledData.write_uint8(puint8(@ModRM)^);
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_sbb_reg32_imm8 *)
{ sbb reg, value }
Procedure TJITCompiler.asm_sbb_reg32_imm8(const Reg: TRegister32; const Value: int8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 3;
 ModRM.RM   := ord(Reg);

 CompiledData.write_uint8($83);
 CompiledData.write_uint8(puint8(@ModRM)^);
 CompiledData.write_int8(Value);
End;

(* TJITCompiler.asm_cmp_reg32_reg32 *)
{ cmp regA, regB }
Procedure TJITCompiler.asm_cmp_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.RM   := ord(RegA);
 ModRM.Reg  := ord(RegB);
 ModRM.Mode := 3;

 CompiledData.write_uint8($39);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_cmp_reg8_reg8 *)
{ cmp regA, regB }
Procedure TJITCompiler.asm_cmp_reg8_reg8(const RegA, RegB: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.RM   := ord(RegA);
 ModRM.Reg  := ord(RegB);
 ModRM.Mode := 3;

 CompiledData.write_uint8($38);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_cmp_mem32_imm32 *)
{ cmp dword [mem], value }
Procedure TJITCompiler.asm_cmp_mem32_imm32(const Mem: uint32; const Value: int32);
Begin
 CompiledData.write_uint8($81);
 CompiledData.write_uint8($3D);
 CompiledData.write_uint32(Mem);
 CompiledData.write_int32(Value);
End;

(* TJITCompiler.asm_sete_mem8 *)
{ sete [mem] }
Procedure TJITCompiler.asm_sete_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($94);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setne_mem8 *)
{ setne [mem] }
Procedure TJITCompiler.asm_setne_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($95);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setg_mem8 *)
{ setg [mem] }
Procedure TJITCompiler.asm_setg_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($9F);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setge_mem8 *)
{ setge [mem] }
Procedure TJITCompiler.asm_setge_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($9D);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setl_mem8 *)
{ setl [mem] }
Procedure TJITCompiler.asm_setl_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($9C);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setle_mem8 *)
{ setle [mem] }
Procedure TJITCompiler.asm_setle_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($9E);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setb_mem8 *)
{ setb [mem] }
Procedure TJITCompiler.asm_setb_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($92);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setbe_mem8 *)
{ setbe [mem] }
Procedure TJITCompiler.asm_setbe_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($96);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_seta_mem8 *)
{ seta [mem] }
Procedure TJITCompiler.asm_seta_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($97);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_setae_mem8 *)
{ setae [mem] }
Procedure TJITCompiler.asm_setae_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($0F);
 CompiledData.write_uint8($93);
 CompiledData.write_uint8($05);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_or_reg32_reg32 *)
{ or regA, regB }
Procedure TJITCompiler.asm_or_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegA);
 ModRM.RM   := ord(RegB);

 CompiledData.write_uint8($0B);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_and_reg32_reg32 *)
{ and regA, regB }
Procedure TJITCompiler.asm_and_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 CompiledData.write_uint8($21);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_xor_reg32_reg32 *)
{ xor regA, regB }
Procedure TJITCompiler.asm_xor_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 CompiledData.write_uint8($31);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_shr_reg32_cl *)
{ shr regA, cl }
Procedure TJITCompiler.asm_shr_reg32_cl(const RegA: TRegister32);
Begin
 CompiledData.write_uint8($D3);
 CompiledData.write_uint8($E8 + ord(RegA));
End;

(* TJITCompiler.asm_not_reg32 *)
{ not reg }
Procedure TJITCompiler.asm_not_reg32(const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 2;
 ModRM.RM   := ord(Reg);

 CompiledData.write_uint8($F7);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_not_mem8 *)
{ not byte [mem] }
Procedure TJITCompiler.asm_not_mem8(const Mem: uint32);
Begin
 CompiledData.write_uint8($F6);
 CompiledData.write_uint8($15);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_not_mem32 *)
{ not dword [mem] }
Procedure TJITCompiler.asm_not_mem32(const Mem: uint32);
Begin
 CompiledData.write_uint8($F7);
 CompiledData.write_uint8($15);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_neg_reg32 *)
{ neg reg }
Procedure TJITCompiler.asm_neg_reg32(const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 3;
 ModRM.RM   := ord(Reg);

 CompiledData.write_uint8($F7);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_neg_mem32 *)
{ neg dword [mem] }
Procedure TJITCompiler.asm_neg_mem32(const Mem: uint32);
Begin
 CompiledData.write_uint8($F7);
 CompiledData.write_uint8($1D);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_test_reg8_reg8 *)
{ test regA, regB }
Procedure TJITCompiler.asm_test_reg8_reg8(const RegA, RegB: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 CompiledData.write_uint8($84);
 CompiledData.write_uint8(puint8(@ModRM)^);
End;

(* TJITCompiler.asm_fild_mem64 *)
{ fild qword [mem] }
Procedure TJITCompiler.asm_fild_mem64(const Mem: uint32);
Begin
 CompiledData.write_uint8($DF);
 CompiledData.write_uint8($2D);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_fld_memfloat *)
{ fld tword [mem] }
Procedure TJITCompiler.asm_fld_memfloat(const Mem: uint32);
Begin
 CompiledData.write_uint8($DB);
 CompiledData.write_uint8($2D);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_fstp_memfloat *)
{ fstp [mem] }
Procedure TJITCompiler.asm_fstp_memfloat(const Mem: uint32);
Begin
 CompiledData.write_uint8($DB);
 CompiledData.write_uint8($3D);
 CompiledData.write_uint32(Mem);
End;

(* TJITCompiler.asm_faddp_st0 *)
{ faddp st(0), reg }
Procedure TJITCompiler.asm_faddp_st0(const Reg: TRegisterFPU);
Begin
 CompiledData.write_uint8($DE);
 CompiledData.write_uint8($C0 + ord(Reg));
End;

(* TJITCompiler.asm_fsubp_st0 *)
{ fsubp st(0), reg }
Procedure TJITCompiler.asm_fsubp_st0(const Reg: TRegisterFPU);
Begin
 CompiledData.write_uint8($DE);
 CompiledData.write_uint8($E8 + ord(Reg));
End;

(* TJITCompiler.asm_fmulp_st0 *)
{ fmulp st(0), reg }
Procedure TJITCompiler.asm_fmulp_st0(const Reg: TRegisterFPU);
Begin
 CompiledData.write_uint8($DE);
 CompiledData.write_uint8($C8 + ord(Reg));
End;

(* TJITCompiler.asm_fdivp_st0 *)
{ fdivp st(0), reg }
Procedure TJITCompiler.asm_fdivp_st0(const Reg: TRegisterFPU);
Begin
 CompiledData.write_uint8($DE);
 CompiledData.write_uint8($F8 + ord(Reg));
End;

(* TJITCompiler.asm_fcomi_st0 *)
{ fcomi st(0), reg }
Procedure TJITCompiler.asm_fcomi_st0(const Reg: TRegisterFPU);
Begin
 CompiledData.write_uint8($DB);
 CompiledData.write_uint8($F0+ord(Reg));
End;

(* TJITCompiler.asm_fcompp *)
{ fcompp }
Procedure TJITCompiler.asm_fcompp;
Begin
 CompiledData.write_uint8($DE);
 CompiledData.write_uint8($D9);
End;

(* TJITCompiler.asm_fstsw_ax *)
{ fstsw ax }
Procedure TJITCompiler.asm_fstsw_ax;
Begin
 CompiledData.write_uint8($9B);
 CompiledData.write_uint8($DF);
 CompiledData.write_uint8($E0);
End;

(* TJITCompiler.asm_fchs *)
{ fchs }
Procedure TJITCompiler.asm_fchs;
Begin
 CompiledData.write_uint8($D9);
 CompiledData.write_uint8($E0);
End;

(* TJITCompiler.asm_sahf *)
{ sahf }
Procedure TJITCompiler.asm_sahf;
Begin
 CompiledData.write_uint8($9E);
End;

// -------------------------------------------------------------------------- //
(* TJITCompiler.Compile *)
Procedure TJITCompiler.Compile;
Const SpecialMarker64: uint64 = $C0A0F0E0B0A0B0E;
      StackSizes     : Array[TOpcodeArgType] of uint8 = (0, 0, 0, 0, 0, 0, 4, 4, 8, 0, 4, 0);
Var Opcode: TOpcode_E;
    Args  : TOpcodeArgArray;

    icall             : PCall;
    ResultMV, ParamsMV: PMixedValue;

    JumpTable         : TJumpTable;
    Jump              : PJumpData;
    CurrentBytecodePos: uint32;

    Pos: uint32;

    isFloatOpcode: Boolean;
    LoadMode     : (cm8Bit, cm64Bit, cmFloat);
    ElemSize     : int32;

    { AddPrologCode }
    Procedure AddPrologCode;
    Begin
     asm_mov_mem32_imm64(getSTPRegMemAddr, 0); // stp = 0;
    End;

    { AddEpilogCode }
    Procedure AddEpilogCode;
    Begin
     asm_push_imm32(uint32(ResultMV));
     asm_absolute_call(uint32(@__release_memory));

     asm_push_imm32(uint32(ParamsMV));
     asm_absolute_call(uint32(@__release_memory));
    End;

    { LoadToReg }
    Procedure LoadToReg(const Arg: TOpcodeArg; const Reg8: TRegister8; const Reg32_1, Reg32_2: TRegister32);
    Begin
     Case Arg.ArgType of
      // bool register
      ptBoolReg:
       Case LoadMode of
        cm8Bit: asm_mov_reg8_mem8(Reg8, getRegMemAddr(Arg));

        cm64Bit:
        Begin
         asm_mov_reg32_imm32(Reg32_1, 0);
         asm_mov_reg32_imm32(Reg32_2, 0);
         asm_mov_reg8_mem8(Reg8, getRegMemAddr(Arg));
        End;
       End;

      // bool immediate
      ptBool:
       Case LoadMode of
        cm8Bit: asm_mov_reg8_imm8(Reg8, ord(Arg.ImmBool));

        cm64Bit:
        Begin
         asm_mov_reg32_imm32(Reg32_1, 0);
         asm_mov_reg32_imm32(Reg32_2, 0);
         asm_mov_reg8_imm8(Reg8, ord(Arg.ImmBool));
        End;
       End;

      // char register
      ptCharReg:
       Case LoadMode of
        cm8Bit: asm_mov_reg8_mem8(Reg8, getRegMemAddr(Arg));

        cm64Bit:
        Begin
         asm_mov_reg32_imm32(Reg32_1, 0);
         asm_mov_reg32_imm32(Reg32_2, 0);
         asm_mov_reg8_mem8(Reg8, getRegMemAddr(Arg));
        End;
       End;

      // char immediate
      ptChar:
       Case LoadMode of
        cm8Bit: asm_mov_reg8_imm8(Reg8, ord(Arg.ImmChar));

        cm64Bit:
        Begin
         asm_mov_reg32_imm32(Reg32_1, 0);
         asm_mov_reg32_imm32(Reg32_2, 0);
         asm_mov_reg8_imm8(Reg8, ord(Arg.ImmChar));
        End;
       End;

      // int register
      ptIntReg:
       Case LoadMode of
        cmFloat: asm_fild_mem64(getRegMemAddr(Arg));

        cm64Bit:
        Begin
         asm_mov_reg32_mem32(Reg32_1, getRegMemAddr(Arg)); // 64-bit number occupies two registers
         asm_mov_reg32_mem32(Reg32_2, getRegMemAddr(Arg)+4);
        End;
       End;

      // int immediate
      ptInt:
       Case LoadMode of
        cmFloat: asm_fild_mem64(AllocateInt64(Arg.ImmInt));

        cm64Bit:
        Begin
         asm_mov_reg32_imm32(Reg32_1, lo(Arg.ImmInt)); // 64-bit number occupies two registers
         asm_mov_reg32_imm32(Reg32_2, hi(Arg.ImmInt));
        End;
       End;

      // float reg
      ptFloatReg: asm_fld_memfloat(getRegMemAddr(Arg));

      // float immediate
      ptFloat: asm_fld_memfloat(AllocateFloat(Arg.ImmFloat));

      else
       raise Exception.CreateFmt('CompareLoad() unsupported case: Arg.ArgType = %d', [ord(Arg.ArgType)]);
     End;
    End;

    { PushParamOnStack }
    Function PushParamOnStack(const Arg: TOpcodeArg): uint8;
    Begin
     Case Arg.ArgType of
      ptBool        : asm_push_imm32(ord(Arg.ImmBool));
      ptBoolReg     : asm_push_mem32(getRegMemAddr(Arg));
      ptChar        : asm_push_imm32(ord(Arg.ImmChar));
      ptCharReg     : asm_push_mem32(getRegMemAddr(Arg));
      ptInt         : asm_push_imm64(Arg.ImmInt);
      ptIntReg      : asm_push_mem64(getRegMemAddr(Arg));
      ptFloat       : asm_fld_memfloat(AllocateFloat(Arg.ImmFloat));
      ptFloatReg    : asm_fld_memfloat(getRegMemAddr(Arg));
      ptString      : asm_push_imm32(AllocateString(Arg.ImmString));
      ptStringReg   : asm_push_mem32(getRegMemAddr(Arg));
      ptReferenceReg: asm_push_mem32(getRegMemAddr(Arg));

      else
       raise Exception.CreateFmt('PushParamOnStack() unsupported argument: %d', [ord(Arg.ArgType)]);
     End;

     Result := StackSizes[StripRegFromArgType(Arg.ArgType)];
    End;

    { isValueOrReg }
    Function isValueOrReg(const Typ: TOpcodeArgType): Boolean; inline;
    Begin
     Result := (Typ in [ptBool, ptBoolReg, ptChar, ptCharReg, ptInt, ptIntReg, ptFloat, ptFloatReg, ptString, ptStringReg, ptReferenceReg]);
    End;

Begin
 CompiledState := csDone;

 New(ResultMV);
 ParamsMV  := AllocMem(256*sizeof(TMixedValue));
 JumpTable := TJumpTable.Create;

 Try
 Try
  While (CompiledState = csDone) Do
  Begin
   if (not BCReader.AnyOpcodeLeft) Then
    Break;

   CurrentBytecodePos := uint32(BCReader.getBytecodeData.Memory) + BCReader.getBytecodeData.Position;
   JumpTable.AddJump(CurrentBytecodePos, CompiledData.Position);

   Opcode := BCReader.FetchOpcode; // fetch opcode
   Args   := BCReader.FetchArguments(Opcode); // fetch opcode arguments

   if (isLocationOpcode(Opcode)) Then // skip the "loc_" opcodes
    Continue;

   Case Opcode of
    { add, sub, mul, div, or, and, xor, shl, shr }
    o_add, o_sub, o_mul, o_div, o_or, o_and, o_xor, o_shl, o_shr:
    Begin
     // opcode(reg int, imm/reg int/char/stackval)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType in [ptInt, ptIntReg, ptChar, ptCharReg, ptStackval]) Then
     Begin
      if (Opcode in [o_add, o_sub, o_mul, o_div, o_shl, o_shr]) Then
      Begin
       Case Args[1].ArgType of
        ptInt    : asm_push_imm64(Args[1].ImmInt);
        ptIntReg : asm_push_mem64(getRegMemAddr(Args[1]));
        ptChar   : asm_push_imm64(ord(Args[1].ImmChar));
        ptCharReg:
        Begin
         asm_mov_reg32_imm32(reg_eax, 0);
         asm_mov_reg8_mem8(reg_al, getRegMemAddr(Args[1]));
         asm_push_imm32(0);
         asm_push_reg32(reg_eax);

         {
          @Note: we can't directly do "push byte [mem]", so we're basically doing this:

           1) mov eax, 0
           2) mov al, byte [Args[1].Register]
           3) push 0
           4) push eax

           1 -> we need to zero the 'eax' register, so it doesn't contain any garbage
           2 -> just loading a byte from memory here
           3, 4 -> a 64-bit value have to be pushed, so the high 32 bits must be empty, and the low 32 bits are our 8-bit value.
         }
        End;

        ptStackval:
        Begin
         asm_push_imm32(Args[1].StackvalPos);
         asm_push_imm32(getSTPRegMemAddr);
         asm_push_imm32(uint32(@VM^.Stack));
         asm_absolute_call(uint32(@__stackval_fetch_int));
         asm_push_reg32(reg_edx);
         asm_push_reg32(reg_eax);
        End;
       End;

       asm_push_mem64(getRegMemAddr(Args[0]));

       Case Opcode of
        o_add: asm_absolute_call(uint32(@__intadd_int_int));
        o_sub: asm_absolute_call(uint32(@__intsub_int_int));
        o_mul: asm_absolute_call(uint32(@__intmul_int_int));
        o_div: asm_absolute_call(uint32(@__intdiv_int_int));
        o_shl: asm_absolute_call(uint32(@__intshl_int_int)); // @TODO: it's a really lame solution
        o_shr: asm_absolute_call(uint32(@__intshr_int_int)); // @TODO: it's a really lame solution
       End;
      End Else
      Begin
       {
        @TODO: when right operand is known, we don't have to load it into register(s)
       }

       LoadMode := cm64Bit;

       LoadToReg(Args[0], reg_al, reg_eax, reg_ebx);
       LoadToReg(Args[1], reg_cl, reg_ecx, reg_edx);

       Case Opcode of
        o_or:
        Begin
         asm_or_reg32_reg32(reg_eax, reg_ecx);
         asm_or_reg32_reg32(reg_ebx, reg_edx);
        End;

        o_and:
        Begin
         asm_and_reg32_reg32(reg_eax, reg_ecx);
         asm_and_reg32_reg32(reg_ebx, reg_edx);
        End;

        o_xor:
        Begin
         asm_xor_reg32_reg32(reg_eax, reg_ecx);
         asm_xor_reg32_reg32(reg_ebx, reg_edx);
        End;
       End;
      End;

      asm_mov_mem32_reg32(getRegMemAddr(Args[0]), reg_eax); // copy result back to the first register
      asm_mov_mem32_reg32(getRegMemAddr(Args[0])+4, reg_edx);
     End Else

     // opcode(reg float, imm/reg int/float)
     if (Opcode in [o_add, o_sub, o_mul, o_div]) and
        (Args[0].ArgType = ptFloatReg) and (Args[1].ArgType in [ptInt, ptIntReg, ptFloat, ptFloatReg]) Then
     Begin
      Case Args[1].ArgType of
       ptInt     : asm_fild_mem64(AllocateInt64(Args[1].ImmInt));
       ptIntReg  : asm_fild_mem64(getRegMemAddr(Args[1]));
       ptFloat   : asm_fld_memfloat(AllocateFloat(Args[1].ImmFloat));
       ptFloatReg: asm_fld_memfloat(getRegMemAddr(Args[1]));
      End;

      asm_fld_memfloat(getRegMemAddr(Args[0])); // push register value

      Case Opcode of
       o_add: asm_faddp_st0(reg_st1);
       o_sub: asm_fsubp_st0(reg_st1);
       o_mul: asm_fmulp_st0(reg_st1);
       o_div: asm_fdivp_st0(reg_st1);
      End;

      asm_fstp_memfloat(getRegMemAddr(Args[0])); // copy result back to the first register
     End Else

     // opcode(stackval, imm/reg int/float)
     if (Args[0].ArgType = ptStackval) and (Args[1].ArgType in [ptInt, ptIntReg, ptFloat, ptFloatReg]) Then
     Begin
      // @TODO: there have to be any faster way to do this! ;c

      Case Args[1].ArgType of
       ptInt     : asm_push_imm64(Args[1].ImmInt);
       ptIntReg  : asm_push_mem64(getRegMemAddr(Args[1]));
       ptFloat   : asm_fld_memfloat(AllocateFloat(Args[1].ImmFloat));
       ptFloatReg: asm_fld_memfloat(getRegMemAddr(Args[1]));
      End;

      isFloatOpcode := (Args[1].ArgType in [ptFloat, ptFloatReg]);

      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));

      if (isFloatOpcode) Then
      Begin
       Case Opcode of
        o_add: asm_absolute_call(uint32(@__stackval_add_float));
        o_sub: asm_absolute_call(uint32(@__stackval_sub_float));
        o_mul: asm_absolute_call(uint32(@__stackval_mul_float));
        o_div: asm_absolute_call(uint32(@__stackval_div_float));

        else
         raise Exception.CreateFmt('invalid instruction: arithmetic_opcode(type(%d), type(%d))', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
       End;
      End Else
      Begin
       Case Opcode of
        o_add: asm_absolute_call(uint32(@__stackval_add_int));
        o_sub: asm_absolute_call(uint32(@__stackval_sub_int));
        o_mul: asm_absolute_call(uint32(@__stackval_mul_int));
        o_div: asm_absolute_call(uint32(@__stackval_div_int));
        o_shl: asm_absolute_call(uint32(@__stackval_shl_int));
        o_shr: asm_absolute_call(uint32(@__stackval_shr_int));

        else
         raise Exception.CreateFmt('invalid instruction: arithmetic_opcode(type(%d), type(%d))', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
       End;
      End;
     End Else

     // opcode(stackval, stackval)
     if (Args[0].ArgType = ptStackval) and (Args[1].ArgType = ptStackval) Then
     Begin
      asm_push_imm32(ord(Opcode));
      asm_push_imm32(Args[1].StackvalPos);
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stackval_opcode_stackval));
     End Else

      raise Exception.CreateFmt('invalid instruction: arithmetic_opcode(type(%d), type(%d))', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
    End;

    { mov }
    o_mov:
    Begin
     // mov(reg bool, imm bool)
     if (Args[0].ArgType = ptBoolReg) and (Args[1].ArgType = ptBool) Then
     Begin
      asm_mov_mem8_imm8(getRegMemAddr(Args[0]), ord(Args[1].ImmBool));
     End Else

     // mov(reg bool, reg bool)
     if (Args[0].ArgType = ptBoolReg) and (Args[1].ArgType = ptBoolReg) Then
     Begin
      asm_mov_reg8_mem8(reg_al, getRegMemAddr(Args[1]));
      asm_mov_mem8_reg8(getRegMemAddr(Args[0]), reg_al);
     End Else

     // mov(reg char, imm char)
     if (Args[0].ArgType = ptCharReg) and (Args[1].ArgType = ptChar) Then
     Begin
      asm_mov_mem8_imm8(getRegMemAddr(Args[0]), ord(Args[1].ImmChar));
     End Else

     // mov(reg char, imm int)
     if (Args[0].ArgType = ptCharReg) and (Args[1].ArgType = ptInt) Then
     Begin
      // @TODO: show (possibly) integer overflow warning
      asm_mov_mem8_imm8(getRegMemAddr(Args[0]), Args[1].ImmInt);
     End Else

     // mov(reg char, reg char)
     if (Args[0].ArgType = ptCharReg) and (Args[1].ArgType = ptCharReg) Then
     Begin
      asm_mov_reg8_mem8(reg_al, getRegMemAddr(Args[1]));
      asm_mov_mem8_reg8(getRegMemAddr(Args[0]), reg_al);
     End Else

     // mov(reg char, reg int)
     if (Args[0].ArgType = ptCharReg) and (Args[1].ArgType = ptIntReg) Then
     Begin
      // @TODO: show (possibly) integer overflow warning
      asm_mov_reg8_mem8(reg_al, getRegMemAddr(Args[1]));
      asm_mov_mem8_reg8(getRegMemAddr(Args[0]), reg_al);
     End Else

     // mov(reg int, imm int)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType = ptInt) Then
     Begin
      asm_mov_mem32_imm64(getRegMemAddr(Args[0]), Args[1].ImmInt);
     End Else

     // mov(reg int, reg int)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType = ptIntReg) Then
     Begin
      asm_mov_reg32_mem32(reg_eax, getRegMemAddr(Args[1])); // eax -> lo
      asm_mov_reg32_mem32(reg_ebx, getRegMemAddr(Args[1])+4); // ebx -> hi
      asm_mov_mem32_reg32(getRegMemAddr(Args[0]), reg_eax);
      asm_mov_mem32_reg32(getRegMemAddr(Args[0])+4, reg_ebx);
     End Else

     // mov(reg int, imm char)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType = ptChar) Then
     Begin
      asm_mov_mem32_imm64(getRegMemAddr(Args[0]), ord(Args[1].ImmChar));
     End Else

     // mov(reg int, reg char)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType = ptCharReg) Then
     Begin
      asm_mov_mem32_imm64(getRegMemAddr(Args[0]), 0); // we need to clear the `ei*` register, because assigning char-> int, we only set first one byte, not entire eight
      asm_mov_reg32_mem32(reg_eax, getRegMemAddr(Args[1]));
      asm_mov_mem32_reg32(getRegMemAddr(Args[0]), reg_eax);
     End Else

     // mov(reg float, imm int)
     if (Args[0].ArgType = ptFloatReg) and (Args[1].ArgType = ptInt) Then
     Begin
      asm_fild_mem64(AllocateInt64(Args[1].ImmInt));
      asm_fstp_memfloat(getRegMemAddr(Args[0]));
     End Else

     // mov(reg float, imm float)
     if (Args[0].ArgType = ptFloatReg) and (Args[1].ArgType = ptFloat) Then
     Begin
      asm_fld_memfloat(AllocateFloat(Args[1].ImmFloat));
      asm_fstp_memfloat(getRegMemAddr(Args[0]));
     End Else

     // mov(reg float, reg int)
     if (Args[0].ArgType = ptFloatReg) and (Args[1].ArgType = ptIntReg) Then
     Begin
      asm_fild_mem64(getRegMemAddr(Args[1])); // push 64-bit number to the FPU stack
      asm_fstp_memfloat(getRegMemAddr(Args[0])); // pop result to the first register
     End Else

     // mov(reg float, reg float)
     if (Args[0].ArgType = ptFloatReg) and (Args[1].ArgType = ptFloatReg) Then
     Begin
      asm_fld_memfloat(getRegMemAddr(Args[1])); // push float register value to the FPU stack
      asm_fstp_memfloat(getRegMemAddr(Args[0])); // pop result to the first register
     End Else

     // mov(reg string, imm string)
     if (Args[0].ArgType = ptStringReg) and (Args[1].ArgType = ptString) Then
     Begin
      asm_mov_mem32_imm32(getRegMemAddr(Args[0]), AllocateString(Args[1].ImmString));
     End Else

     // mov(reg string, reg string)
     if (Args[0].ArgType = ptStringReg) and (Args[1].ArgType = ptStringReg) Then
     Begin
      asm_mov_reg32_mem32(reg_eax, getRegMemAddr(Args[1]));
      asm_mov_mem32_reg32(getRegMemAddr(Args[0]), reg_eax);
     End Else

     // mov(reg, stackval)
     if (Args[1].ArgType = ptStackval) Then
     Begin
      asm_push_imm32(getRegMemAddr(Args[0]));
      asm_push_imm32(Args[1].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));

      Case Args[0].ArgType of
       ptBoolReg     : asm_absolute_call(uint32(@__boolreg_stackval_assign));
       ptCharReg     : asm_absolute_call(uint32(@__charreg_stackval_assign));
       ptIntReg      : asm_absolute_call(uint32(@__intreg_stackval_assign));
       ptFloatReg    : asm_absolute_call(uint32(@__floatreg_stackval_assign));
       ptStringReg   : asm_absolute_call(uint32(@__stringreg_stackval_assign));
       ptReferenceReg: asm_absolute_call(uint32(@__referencereg_stackval_assign));

       else
        raise Exception.CreateFmt('invalid instruction: mov(type(%d), stackval)', [ord(Args[0].ArgType)]);
      End;
     End Else

     // mov(stackval, value)
     if (Args[0].ArgType = ptStackval) Then
     Begin
      PushParamOnStack(Args[1]);
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));

      Case StripRegFromArgType(Args[1].ArgType) of
       ptBool        : asm_absolute_call(uint32(@__stackval_boolval_assign));
       ptChar        : asm_absolute_call(uint32(@__stackval_charval_assign));
       ptInt         : asm_absolute_call(uint32(@__stackval_intval_assign));
       ptFloat       : asm_absolute_call(uint32(@__stackval_floatval_assign));
       ptString      : asm_absolute_call(uint32(@__stackval_stringval_assign));
       ptReferenceReg: asm_absolute_call(uint32(@__stackval_referenceval_assign));

       else
        raise Exception.CreateFmt('invalid instruction: mov(stackval, type(%d))', [ord(Args[1].ArgType)]);
      End;
     End Else

     // @TODO: mov(stackval, stackval)

      raise Exception.CreateFmt('invalid instruction: mov(type(%d), type(%d))', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
    End;

    { push }
    o_push:
    Begin
     // push(imm/reg bool)
     if (Args[0].ArgType in [ptBool, ptBoolReg]) Then
     Begin
      if (Args[0].ArgType = ptBool) Then
       asm_push_imm32(ord(Args[0].ImmBool)) Else
       asm_push_mem32(getRegMemAddr(Args[0]));

      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_bool));
     End Else

     // push(imm/reg char)
     if (Args[0].ArgType in [ptChar, ptCharReg]) Then
     Begin
      if (Args[0].ArgType = ptChar) Then
       asm_push_imm32(ord(Args[0].ImmChar)) Else
       asm_push_mem32(getRegMemAddr(Args[0]));

      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_char));
     End Else

     // push(imm/reg int)
     if (Args[0].ArgType in [ptInt, ptIntReg]) Then
     Begin
      if (Args[0].ArgType = ptInt) Then
       asm_push_imm64(Args[0].ImmInt) Else
       asm_push_mem64(getRegMemAddr(Args[0]));

      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_int));
     End Else

     // push(imm/reg float)
     if (Args[0].ArgType in [ptFloat, ptFloatReg]) Then
     Begin
      if (Args[0].ArgType = ptFloat) Then
       asm_fld_memfloat(AllocateFloat(Args[0].ImmFloat)) Else
       asm_fld_memfloat(getRegMemAddr(Args[0]));

      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_float));
     End Else

     // push(imm/reg string)
     if (Args[0].ArgType in [ptString, ptStringReg]) Then
     Begin
      if (Args[0].ArgType = ptString) Then
       asm_push_imm32(uint32(CopyStringToPChar(Args[0].ImmString))) Else // push string pointer, not the string itself
       asm_push_mem32(getRegMemAddr(Args[0]));

      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_string));
     End Else

     // push(reg reference)
     if (Args[0].ArgType = ptReferenceReg) Then
     Begin
      asm_push_mem32(getRegMemAddr(Args[0]));
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_reference));
     End Else

     // push(stackval)
     if (Args[0].ArgType = ptStackVal) Then
     Begin
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_stackval));
     End Else

      raise Exception.CreateFmt('invalid instruction: push(type(%d))', [ord(Args[0].ArgType)]);
    End;

    { pop }
    o_pop:
    Begin
     asm_push_imm32(getRegMemAddr(Args[0]));
     asm_push_imm32(getSTPRegMemAddr);
     asm_push_imm32(uint32(@VM^.Stack));

     // pop(reg bool)
     if (Args[0].ArgType = ptBoolReg) Then
     Begin
      asm_absolute_call(uint32(@__stack_pop_bool_reg));
     End Else

     // pop(reg char)
     if (Args[0].ArgType = ptCharReg) Then
     Begin
      asm_absolute_call(uint32(@__stack_pop_char_reg));
     End Else

     // pop(reg int)
     if (Args[0].ArgType = ptIntReg) Then
     Begin
      asm_absolute_call(uint32(@__stack_pop_int_reg));
     End Else

     // pop(reg float)
     if (Args[0].ArgType = ptFloatReg) Then
     Begin
      asm_absolute_call(uint32(@__stack_pop_float_reg));
     End Else

     // pop(reg string)
     if (Args[0].ArgType = ptStringReg) Then
     Begin
      asm_absolute_call(uint32(@__stack_pop_string_reg));
     End Else

     // pop(reg reference)
     if (Args[0].ArgType = ptReferenceReg) Then
     Begin
      asm_absolute_call(uint32(@__stack_pop_reference_reg));
     End Else

      raise Exception.CreateFmt('invalid instruction: pop(type(%d))', [ord(Args[0].ArgType)]);
    End;

    { if_le, if_l, if_e, if_ne, if_g, if_ge }
    o_if_le, o_if_l, o_if_e, o_if_ne, o_if_g, o_if_ge:
    Begin
     // opcode(imm/reg bool/char/int/float, imm/reg bool/char/int/float)
     if (Args[0].ArgType in [ptBool, ptBoolReg, ptChar, ptCharReg, ptInt, ptIntReg, ptFloat, ptFloatReg]) and
        (Args[1].ArgType in [ptBool, ptBoolReg, ptChar, ptCharReg, ptInt, ptIntReg, ptFloat, ptFloatReg]) Then
     Begin
      {
       @TODO: for ints this can be optimized to only one opcode in some frequent cases:
         cmp(imm, imm)
         cmp(mem, imm)
         cmp(imm, mem)
      }

      LoadMode := cm8Bit;

      if (Args[0].ArgType in [ptInt, ptIntReg]) or (Args[1].ArgType in [ptInt, ptIntReg]) Then
       LoadMode := cm64Bit;

      if (Args[0].ArgType in [ptFloat, ptFloatReg]) or (Args[1].ArgType in [ptFloat, ptFloatReg]) Then
       LoadMode := cmFloat;

      if ((LoadMode = cmFloat) and (not (Args[0].ArgType in [ptFloat, ptFloatReg, ptInt, ptIntReg]))) or
         ((LoadMode = cmFloat) and (not (Args[1].ArgType in [ptFloat, ptFloatReg, ptInt, ptIntReg]))) Then // cannot compare float with non-float nor non-int
      Begin
       raise Exception.CreateFmt('cannot compare floats with non-floats nor non-ints: type(%d), type(%d)', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
      End;

      LoadToReg(Args[1], reg_cl, reg_ecx, reg_edx);
      LoadToReg(Args[0], reg_al, reg_eax, reg_ebx);

      if (LoadMode = cmFloat) Then
      Begin
       asm_fcompp();
       asm_fstsw_ax();
       asm_sahf();

       Case Opcode of
        o_if_le: asm_setbe_mem8(getIFRegMemAddr);
        o_if_l : asm_setb_mem8(getIFRegMemAddr);
        o_if_e : asm_sete_mem8(getIFRegMemAddr);
        o_if_ne: asm_setne_mem8(getIFRegMemAddr);
        o_if_g : asm_seta_mem8(getIFRegMemAddr);
        o_if_ge: asm_setae_mem8(getIFRegMemAddr);
       End;
      End Else
      Begin
       if (LoadMode = cm64Bit) Then
       Begin
        // @TODO (maybe converting to float and comparing as floats?)
        asm_cmp_reg32_reg32(reg_eax, reg_ecx);
       End Else
       Begin
        asm_cmp_reg8_reg8(reg_al, reg_cl);
       End;

       Case Opcode of
        o_if_le: asm_setle_mem8(getIFRegMemAddr);
        o_if_l : asm_setl_mem8(getIFRegMemAddr);
        o_if_e : asm_sete_mem8(getIFRegMemAddr);
        o_if_ne: asm_setne_mem8(getIFRegMemAddr);
        o_if_g : asm_setg_mem8(getIFRegMemAddr);
        o_if_ge: asm_setge_mem8(getIFRegMemAddr);
       End;
      End;
     End Else

     // opcode(imm/reg string, imm/reg string)
     if (Args[0].ArgType in [ptString, ptStringReg]) and (Args[1].ArgType in [ptString, ptStringReg]) Then
     Begin
      PushParamOnStack(Args[1]);
      PushParamOnStack(Args[0]);
      asm_push_imm32(ord(Opcode));
      asm_push_imm32(getIFRegMemAddr);
      asm_absolute_call(uint32(@__compare_string_string)); // @TODO: it's a bit lame solution; it can be done MUCH faster without actually calling '__compare_string_string'.
     End Else

     // opcode(stackval, stackval)
     if (Args[0].ArgType = ptStackval) and (Args[1].ArgType = ptStackval) Then
     Begin
      asm_push_imm32(ord(Opcode));
      asm_push_imm32(Args[1].StackvalPos);
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getIFRegMemAddr);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__compare_stackval_stackval));
     End Else

     // opcode(stackval, value)
     if (Args[0].ArgType = ptStackval) and (isValueOrReg(Args[1].ArgType)) Then
     Begin
      ElemSize        := PushParamOnStack(Args[1]);
      Args[1].ArgType := StripRegFromArgType(Args[1].ArgType);

      asm_push_imm32(ord(Args[1].ArgType));
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(ord(Opcode));
      asm_push_imm32(getIFRegMemAddr);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));

      asm_absolute_call(uint32(@__compare_stackval_value));
      if (not (Args[1].ArgType in [ptFloat, ptFloatReg])) Then // FPU has it own stack
       asm_add_reg32_imm32(reg_esp, -4 + ElemSize); // we need to do a small stack clean-up, because the last argument of '__compare_stackval_value' has pointer type (not the real argument's type, because it's unknown at this time), so compiler wouldn't free the stack entirely, and we don't want any garbage laying on the stack
     End Else

     // opcode(value, stackval)
     if (isValueOrReg(Args[0].ArgType)) and (Args[1].ArgType = ptStackval) Then
     Begin
      ElemSize        := PushParamOnStack(Args[0]);
      Args[0].ArgType := StripRegFromArgType(Args[0].ArgType);

      asm_push_imm32(ord(Args[0].ArgType));
      asm_push_imm32(Args[1].StackvalPos);
      asm_push_imm32(ord(Opcode));
      asm_push_imm32(getIFRegMemAddr);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));

      asm_absolute_call(uint32(@__compare_value_stackval));
      if (not (Args[1].ArgType in [ptFloat, ptFloatReg])) Then // FPU has it own stack
       asm_add_reg32_imm32(reg_esp, -4 + ElemSize); // we need to do a small stack clean-up, because the last argument of '__compare_stackval_value' has pointer type (not the real argument's type, because it's unknown at this time), so compiler wouldn't free the stack entirely, and we don't want any garbage laying on the stack
     End Else

      raise Exception.CreateFmt('invalid instruction: if_*(%d, %d)', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
    End;

    { icall }
    o_icall:
    Begin
     // icall(imm string)
     if (Args[0].ArgType = ptString) Then
     Begin
      icall := VM^.FindInternalCall(Args[0].ImmString);

      if (icall = nil) Then // error: specified icall not found
      Begin
       raise Exception.CreateFmt('unknown icall ''%s''', [Args[0].ImmString]);
      End Else
      Begin
       // prepare parameter list
       asm_push_imm32(getSTPRegMemAddr);
       asm_push_imm32(uint32(@VM^.Stack));
       asm_push_imm32(icall^.ParamCount);
       asm_push_imm32(uint32(ParamsMV));
       asm_absolute_call(uint32(@__create_icall_parameter_list));
       CompiledData.write_uint8($01); // cleanup stack: add esp, eax
       CompiledData.write_uint8($C4);

       // clean 'ResultMV'
       asm_push_imm32(uint32(ResultMV));
       asm_absolute_call(uint32(@__clean_mixedvalue));

       // call
       asm_push_imm32(uint32(ResultMV));
       asm_push_imm32(uint32(ParamsMV));
       asm_push_imm32(uint32(VM));
       asm_absolute_call(uint32(icall^.Handler));

       // push 'ResultMV' onto the stack (if necessary)
       asm_push_imm32(getSTPRegMemAddr);
       asm_push_imm32(uint32(@VM^.Stack));
       asm_push_imm32(uint32(ResultMV));
       asm_absolute_call(uint32(@__apply_mixedvalue));
      End;
     End Else
     Begin
      // error: unsupported icall calling
      raise Exception.Create('invalid icall calling');
     End;
    End;

    { call, jmp, fjmp, tjmp }
    o_call, o_jmp, o_fjmp, o_tjmp:
    Begin
     if (Args[0].ArgType <> ptInt) Then // error: jumps and calls have to be constant
     Begin
      raise Exception.Create('non-constant jump or call');
      Exit;
     End;

     // write special marker on a 4-byte boundary, which will be changed to the appropiate jump later
     While (CompiledData.Position mod 4 <> 0) Do
      asm_nop;

     CompiledData.write_uint64(SpecialMarker64);
     CompiledData.write_uint8(ord(Opcode));
     CompiledData.write_int32(CurrentBytecodePos+Args[0].ImmInt);

     if (Opcode = o_call) Then
     Begin
      For Pos := 1 To 15 Do
       asm_nop;
     End;

     if (Opcode in [o_fjmp, o_tjmp]) Then // conditional jumps need a bit more space
     Begin
      For Pos := 1 To 10 Do
       asm_nop;
     End;
    End;

    { ret }
    o_ret:
    Begin
     asm_push_imm32(getSTPRegMemAddr);
     asm_push_imm32(uint32(@VM^.Stack));
     asm_absolute_call(uint32(@__ret));
     CompiledData.write_uint8($FF); // jmp eax
     CompiledData.write_uint8($E0);
    End;

    { strjoin }
    o_strjoin:
    Begin
     // @TODO: strjoin(stackval, imm/reg string)
     // @TODO: strjoin(stackval, stackval)

     // strjoin(reg string, imm/reg string)
     if (Args[0].ArgType = ptStringReg) and (Args[1].ArgType in [ptString, ptStringReg]) Then
     Begin
      if (Args[1].ArgType = ptString) Then
       asm_push_imm32(AllocateString(Args[1].ImmString)) Else
       asm_push_mem32(getRegMemAddr(Args[1]));

      asm_push_imm32(getRegMemAddr(Args[0]));

      asm_absolute_call(uint32(@__stringconcat_reg_string));
     End Else

      raise Exception.CreateFmt('invalid opcode: strjoin(type(%d), type(%d))', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
    End;

    { neg }
    o_neg:
    Begin
     // neg(reg int)
     if (Args[0].ArgType = ptIntReg) Then
     Begin
      LoadMode := cm64Bit;
      LoadToReg(Args[0], reg_al, reg_eax, reg_ebx);

      asm_not_reg32(reg_ebx);
      asm_neg_reg32(reg_eax);
      asm_sbb_reg32_imm8(reg_ebx, -1);

      asm_mov_mem32_reg32(getRegMemAddr(Args[0]), reg_eax);
      asm_mov_mem32_reg32(getRegMemAddr(Args[0])+4, reg_ebx);
     End Else

     // neg(reg float)
     if (Args[0].ArgType = ptFloatReg) Then
     Begin
      asm_fld_memfloat(getRegMemAddr(Args[0]));
      asm_fchs;
      asm_fstp_memfloat(getRegMemAddr(Args[0]));
     End Else

     // neg(stackval)
     if (Args[0].ArgType = ptStackval) Then
     Begin
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stackval_neg));
     End Else

      raise Exception.CreateFmt('invalid opcode: neg(type(%d))', [ord(Args[0].ArgType)]);
    End;

    { not }
    o_not:
    Begin
     // not(reg bool)
     if (Args[0].ArgType = ptBoolReg) Then
     Begin
      asm_mov_reg8_mem8(reg_al, getRegMemAddr(Args[0]));
      asm_test_reg8_reg8(reg_al, reg_al);
      asm_sete_mem8(getRegMemAddr(Args[0]));
     End Else

     // not(reg int)
     if (Args[0].ArgType = ptIntReg) Then
     Begin
      asm_not_mem32(getRegMemAddr(Args[0]));
      asm_not_mem32(getRegMemAddr(Args[0])+4);
     End Else

     // not(stackval)
     if (Args[0].ArgType = ptStackval) Then
     Begin
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stackval_not));
     End Else

      raise Exception.CreateFmt('invalid opcode: not(type(%d))', [ord(Args[0].ArgType)]);
    End;

    { mod }
    o_mod:
    Begin
     // mod(reg int, imm/reg int/stackval)
     if (Args[0].ArgType = ptIntReg) and (Args[1].ArgType in [ptInt, ptIntReg, ptStackval]) Then
     Begin
      Case Args[1].ArgType of
       ptInt     : asm_push_imm64(Args[1].ImmInt);
       ptIntReg  : asm_push_mem64(getRegMemAddr(Args[1]));
       ptStackval:
       Begin
        asm_push_imm32(Args[1].StackvalPos);
        asm_push_imm32(getSTPRegMemAddr);
        asm_push_imm32(uint32(@VM^.Stack));
        asm_absolute_call(uint32(@__stackval_fetch_int));
        asm_push_reg32(reg_edx);
        asm_push_reg32(reg_eax);
       End;
      End;

      asm_push_mem64(getRegMemAddr(Args[0]));
      asm_absolute_call(uint32(@__intmod_int_int));

      asm_mov_mem32_reg32(getRegMemAddr(Args[0]), reg_eax); // copy result back to the first register
      asm_mov_mem32_reg32(getRegMemAddr(Args[0])+4, reg_edx);
     End Else

     // mod(stackval, imm/reg int)
     if (Args[0].ArgType = ptStackval) and (Args[1].ArgType in [ptInt, ptIntReg]) Then
     Begin
      Case Args[1].ArgType of
       ptInt   : asm_push_imm64(Args[1].ImmInt);
       ptIntReg: asm_push_mem64(getRegMemAddr(Args[1]));
      End;

      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stackval_mod_int));
     End Else

     // mod(stackval, stackval)
     if (Args[0].ArgType = ptStackval) and (Args[1].ArgType = ptStackval) Then
     Begin
      asm_push_imm32(ord(Opcode));
      asm_push_imm32(Args[1].StackvalPos);
      asm_push_imm32(Args[0].StackvalPos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stackval_opcode_stackval));
     End Else

      raise Exception.CreateFmt('invalid opcode: mod(type(%d), type(%d))', [ord(Args[0].ArgType), ord(Args[1].ArgType)]);
    End;

    { stop }
    o_stop:
    Begin
     AddEpilogCode;
     asm_ret;
    End;

    { nop }
    o_nop:
    Begin
    End;

    Else
    Begin
     CompiledState := csJITUnsupported;
     raise Exception.CreateFmt('unimplemented opcode: 0x%x', [ord(Opcode)]);
    End;
   End;
  End;

  if (CompiledState = csDone) Then
  Begin
   // ok, so the bytecode has been compiled to x86 machine code; now there's only one thing left: replacing dummy jumps and calls with real jumps and calls.
   CompiledData.Position := 0;

   While (CompiledData.Position < CompiledData.Size) Do
   Begin
    if (CompiledData.read_uint32 = lo(SpecialMarker64)) and
       (CompiledData.read_uint32 = hi(SpecialMarker64)) Then
    Begin
     Opcode := TOpcode_E(CompiledData.read_uint8);
     Pos    := CompiledData.read_int32;
     Jump   := JumpTable.FindJumpByBytecodeAddress(Pos); // find jump

     if (Jump = nil) Then // error: invalid/impossible jump
      raise Exception.CreateFmt('found a jump to an invalid bytecode boundary area! :: jmp(%d)', [Pos]);

     Pos                   := CompiledData.Position;
     CompiledData.Position := CompiledData.Position - sizeof(uint64) - sizeof(uint32) - sizeof(uint8);

     if (Opcode = o_call) Then
     Begin
      Inc(Pos, 15*sizeof(uint8));

      // function call
      asm_push_imm32(uint32(CompiledData.Memory) + Pos);
      asm_push_imm32(getSTPRegMemAddr);
      asm_push_imm32(uint32(@VM^.Stack));
      asm_absolute_call(uint32(@__stack_push_reference)); // push return address
      asm_absolute_jmp(uint32(CompiledData.Memory) + Jump^.JumpAddress);
     End Else
     if (Opcode = o_jmp) Then
     Begin
      // unconditional jump
      asm_absolute_jmp(uint32(CompiledData.Memory) + Jump^.JumpAddress);
     End Else
     Begin
      Inc(Pos, 10*sizeof(uint8));

      // conditonal jump
      Case Opcode of
       o_tjmp: asm_cmp_mem32_imm32(getIFRegMemAddr, 1);
       o_fjmp: asm_cmp_mem32_imm32(getIFRegMemAddr, 0);
      End;

      asm_relative_jne(7); // '7' is size of mov+jmp
      asm_absolute_jmp(uint32(CompiledData.Memory) + Jump^.JumpAddress);

      {
       @Note:
       As we need a far conditional jump, we're using a simple trick:

        jne skip
        mov eax, where_to_jump_when_condition_is_met
        jmp eax

       skip:
      }
     End;

     if (CompiledData.Position > Pos) Then
      raise Exception.CreateFmt('not enough space to place opcodes! %d :: %d (shouldn''t happen!)', [CompiledData.Position, Pos]);

     While (CompiledData.Position < Pos) Do // fill left space with nop-s
      asm_nop;

     CompiledData.Position := 0;
    End;
   End;
  End;
 Finally
  JumpTable.Free;
 End;
 Except
  if (CompiledState = csDone) Then
   CompiledState := csInvalidBytecode;
  raise;
 End;

 AddEpilogCode;
End;
End.
