(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITAsm;

 Interface
 Uses VMTypes, Stream;

 { registers }
 Type TRegister32  = (reg_eax=0, reg_ecx, reg_edx, reg_ebx, reg_esp, reg_ebp, reg_esi, reg_edi);
 Type TRegister16  = (reg_ax=0, reg_cx, reg_dx, reg_bx, reg_sp, reg_bp, reg_si, reg_di);
 Type TRegister8   = (reg_al=0, reg_cl, reg_dl, reg_bl, reg_ah, reg_ch, reg_dh, reg_bh);
 Type TRegisterFPU = (reg_st0=0, reg_st1, reg_st2, reg_st3, reg_st4, reg_st5, reg_st6, reg_st7);

 { TModRM }
 Type TModRM =
      Bitpacked Record // ModR/M structure
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

 { TResolvableCall }
 Type TResolvableCall =
      Record
       OpcodeAddress, AddressToCall: uint32;
      End;

 Type TResolvableCallArray = Array of TResolvableCall;

 { TJITAsm }
 Type TJITAsm =
      Class
       Private
        Data           : TStream;
        ResolvableCalls: TResolvableCallArray;

       Private
        Procedure emit_uint8(const Value: uint8);
        Procedure emit_uint32(const Value: uint32);
        Procedure emit_uint32(const Value: VMReference);

        Procedure emit_int8(const Value: int8);
        Procedure emit_int16(const Value: int16);
        Procedure emit_int32(const Value: int32);

        Procedure emit_modrm(const Value: TModRM);

        Procedure AddNewResolvableCall(const AddressToCall: uint32);

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure post_compilation;

       Public
    { >> CPU << }
        // nop
        Procedure nop;

        // ret
        Procedure ret;

        // push
        Procedure push_imm32(const Value: int32);
        Procedure push_mem32(const Mem: VMReference);

        // mov reg, ...
        Procedure mov_reg8_imm8(const Reg: TRegister8; const Value: int8);
        Procedure mov_reg8_reg8(const RegA, RegB: TRegister8);
        Procedure mov_reg8_mem8(const Reg: TRegister8; const Mem: VMReference);

        Procedure mov_reg16_imm16(const Reg: TRegister16; const Value: int16);

        Procedure mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
        Procedure mov_reg32_reg32(const RegA, RegB: TRegister32);
        Procedure mov_reg32_mem32(const Reg: TRegister32; const Mem: VMReference);

        // mov [mem], ...
        Procedure mov_mem8_imm8(const Mem: VMReference; const Value: int8);
        Procedure mov_mem8_reg8(const Mem: VMReference; const Reg: TRegister8);

        Procedure mov_mem16_imm16(const Mem: VMReference; const Value: int16);

        Procedure mov_mem32_imm32(const Mem: VMReference; const Value: int32);
        Procedure mov_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // xchg
        Procedure xchg_reg32_reg32(const RegA, RegB: TRegister32);

        // add
        Procedure add_mem8_imm8(const Mem: VMReference; const Value: int8);
        Procedure add_mem8_reg8(const Mem: VMReference; const Reg: TRegister8);

        Procedure add_reg32_reg32(const RegA, RegB: TRegister32);

        Procedure add_mem32_imm32(const Mem: VMReference; const Value: int32);
        Procedure add_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // adc
        Procedure adc_mem32_imm32(const Mem: VMReference; const Value: int32);
        Procedure adc_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // sub
        Procedure sub_mem8_imm8(const Mem: VMReference; const Value: int8);
        Procedure sub_mem8_reg8(const Mem: VMReference; const Reg: TRegister8);

        Procedure sub_mem32_imm32(const Mem: VMReference; const Value: int32);
        Procedure sub_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // sbb
        Procedure sbb_mem32_imm32(const Mem: VMReference; const Value: int32);
        Procedure sbb_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // mul
        Procedure mul_reg8(const Reg: TRegister8);
        Procedure mul_reg32(const Reg: TRegister32);

        Procedure mul_mem8(const Mem: VMReference);

        // div
        Procedure div_reg8(const Reg: TRegister8);

        Procedure div_mem8(const Mem: VMReference);

        // cmp
        Procedure cmp_reg32_imm32(const Reg: TRegister32; const Value: int32);
        Procedure cmp_reg32_mem32(const Reg: TRegister32; const Mem: VMReference);

        Procedure cmp_mem8_imm8(const Mem: VMReference; const Value: int8);
        Procedure cmp_mem32_imm32(const Mem: VMReference; const Value: int32);
        Procedure cmp_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // jumps
        Function jmp(const Address: int32): uint32;
        Procedure jmp(const Reg: TRegister32);

        Function jne(const Address: int32): uint32;
        Function jna(const Address: int32): uint32;
        Function jnae(const Address: int32): uint32;
        Function jnb(const Address: int32): uint32;
        Function jnbe(const Address: int32): uint32;
        Function je(const Address: int32): uint32;
        Function jg(const Address: int32): uint32;
        Function jl(const Address: int32): uint32;

        // call
        Procedure call_internalproc(const Handler: Pointer);

    { >> FPU << }

        // fld
        Procedure fld_memfloat(const Mem: VMReference);

        // fild
        Procedure fild_memint(const Mem: VMReference);

        // fstp
        Procedure fstp_memfloat(const Mem: VMReference);

        // fistp
        Procedure fistp_memint(const Mem: VMReference);

        // faddp
        Procedure faddp_st0(const Reg: TRegisterFPU);

        // fsubp
        Procedure fsubp_st0(const Reg: TRegisterFPU);

        // fmulp
        Procedure fmulp_st0(const Reg: TRegisterFPU);

        // fdivp
        Procedure fdivp_st0(const Reg: TRegisterFPU);

       Public
        Property getData: TStream read Data;
       End;

 Implementation

(* TJITAsm.emit_uint8 *)
Procedure TJITAsm.emit_uint8(const Value: uint8);
Begin
 Data.write_uint8(Value);
End;

(* TJITAsm.emit_uint32 *)
Procedure TJITAsm.emit_uint32(const Value: uint32);
Begin
 Data.write_uint32(Value);
End;

(* TJITAsm.emit_uint32 *)
Procedure TJITAsm.emit_uint32(const Value: VMReference);
Begin
 Data.write_uint32(uint32(Value));
End;

(* TJITAsm.emit_int8 *)
Procedure TJITAsm.emit_int8(const Value: int8);
Begin
 Data.write_int8(Value);
End;

(* TJITAsm.emit_int16 *)
Procedure TJITAsm.emit_int16(const Value: int16);
Begin
 Data.write_int16(Value);
End;

(* TJITAsm.emit_int32 *)
Procedure TJITAsm.emit_int32(const Value: int32);
Begin
 Data.write_int32(Value);
End;

(* TJITAsm.emut_modrm *)
Procedure TJITAsm.emit_modrm(const Value: TModRM);
Begin
 emit_uint8(puint8(@Value)^);
End;

(* TJITAsm.AddNewResolvableCall *)
Procedure TJITAsm.AddNewResolvableCall(const AddressToCall: uint32);
Begin
 {$MACRO ON}
 {$DEFINE RC := ResolvableCalls}
 SetLength(RC, Length(RC)+1);
 RC[High(RC)].OpcodeAddress := Data.Position;
 RC[High(RC)].AddressToCall := AddressToCall;
 {$UNDEF RC}
End;

(* TJITAsm.Create *)
Constructor TJITAsm.Create;
Begin
 Data := TStream.Create(False);
End;

(* TJITAsm.Destroy *)
Destructor TJITAsm.Destroy;
Begin
 Data.Free;

 inherited Destroy;
End;

(* TJITAsm.post_compilation *)
Procedure TJITAsm.post_compilation;
Var Call: TResolvableCall;
Begin
 For Call in ResolvableCalls Do
 Begin
  Data.Position := int64(Call.OpcodeAddress) + 1; // '+1' because we must skip the opcode ($E8 in this case)
  Data.write_int32(Call.AddressToCall - (uint32(Data.Memory)+Data.Position+4));
 End;

 Data.Position := 0;
End;

(* TJITAsm.nop *)
{
 nop
}
Procedure TJITAsm.nop;
Begin
 Data.write_uint8($90);
End;

(* TJITAsm.ret *)
{
 ret
}
Procedure TJITAsm.ret;
Begin
 emit_uint8($C3);
End;

(* TJITAsm.push_imm32 *)
{
 push dword value
}
Procedure TJITAsm.push_imm32(const Value: int32);
Begin
 emit_uint8($68);
 emit_int32(Value);
End;

(* TJITAsm.push_mem32 *)
{
 push dword [mem]
}
Procedure TJITAsm.push_mem32(const Mem: VMReference);
Begin
 emit_uint8($FF);
 emit_uint8($35);
 emit_uint32(Mem);
End;

(* TJITASm.mov_reg8_imm8 *)
{
 mov reg, value
}
Procedure TJITAsm.mov_reg8_imm8(const Reg: TRegister8; const Value: int8);
Begin
 emit_uint8($B0+ord(Reg));
 emit_int8(Value);
End;

(* TJITAsm.mov_reg8_reg8 *)
{
 mov regA, regB
}
Procedure TJITAsm.mov_reg8_reg8(const RegA, RegB: TRegister8);
Var ModRM: TModRM;
Begin
 if (RegA = RegB) Then
  Exit;

 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 emit_uint8($88);
 emit_modrm(ModRM);
End;

(* TJITAsm.mov_reg8_mem8 *)
{
 mov reg, byte [mem]
}
Procedure TJITAsm.mov_reg8_mem8(const Reg: TRegister8; const Mem: VMReference);
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
  emit_uint8(puint8(@ModRM)^);
  emit_uint32(Mem);
 End;
End;

(* TJITAsm.mov_reg16_imm16 *)
Procedure TJITAsm.mov_reg16_imm16(const Reg: TRegister16; const Value: int16);
Begin
 emit_uint8($66);
 emit_uint8($B8+ord(Reg));
 emit_int16(Value);
End;

(* TJITAsm.mov_reg32_imm32 *)
{
 mov reg, value
}
Procedure TJITAsm.mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
Begin
 emit_uint8($B8+ord(Reg));
 emit_int32(Value);
End;

(* TJITAsm.mov_reg32_reg32 *)
{
 mov regA, regB
}
Procedure TJITAsm.mov_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 if (RegA = RegB) Then // skip no-ops
  Exit;

 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 emit_uint8($89);
 emit_modrm(ModRM);
End;

(* TJITAsm.mov_reg32_mem32 *)
{
 mov reg, dword [mem]
}
Procedure TJITAsm.mov_reg32_mem32(const Reg: TRegister32; const Mem: VMReference);
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
  emit_uint32(Mem);
 End;
End;

(* TJITAsm.mov_mem8_imm8 *)
{
 mov byte [mem], value
}
Procedure TJITAsm.mov_mem8_imm8(const Mem: VMReference; const Value: int8);
Begin
 emit_uint8($C6);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_uint8(Value);
End;

(* TJITAsm.mov_mem8_reg8 *)
{
 mov byte [mem], reg
}
Procedure TJITAsm.mov_mem8_reg8(const Mem: VMReference; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($88);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.mov_mem16_imm16 *)
{
 mov word [mem], value
}
Procedure TJITAsm.mov_mem16_imm16(const Mem: VMReference; const Value: int16);
Begin
 emit_uint8($66);
 emit_uint8($C7);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_int16(Value);
End;

(* TJITAsm.mov_mem32_imm32 *)
{
 mov dword [mem], value
}
Procedure TJITAsm.mov_mem32_imm32(const Mem: VMReference; const Value: int32);
Begin
 emit_uint8($C7);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITAsm.mov_mem32_reg32 *)
{
 mov dword [mem], reg
}
Procedure TJITAsm.mov_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);
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
  emit_uint32(Mem);
 End;
End;

(* TJITAsm.xchg_reg32_reg32 *)
{
 xchg regA, regB
}
Procedure TJITAsm.xchg_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegA);
 ModRM.RM   := ord(RegB);

 emit_uint8($87);
 emit_modrm(ModRM);
End;

(* TJITAsm.add_mem8_imm8 *)
{
 add byte [mem], value
}
Procedure TJITAsm.add_mem8_imm8(const Mem: VMReference; const Value: int8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := 0;

 emit_uint8($80);
 emit_modrm(ModRM);
 emit_uint32(Mem);
 emit_int8(Value);
End;

(* TJITAsm.add_mem8_reg8 *)
{
 add byte [mem], reg
}
Procedure TJITAsm.add_mem8_reg8(const Mem: VMReference; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($00);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.add_reg32_reg32 *)
{
 add regA, regB
}
Procedure TJITAsm.add_reg32_reg32(const RegA, RegB: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := ord(RegB);
 ModRM.RM   := ord(RegA);

 emit_uint8($01);
 emit_modrm(ModRM);
End;

(* TJITAsm.add_mem32_imm32 *)
{
 add dword [mem], value
}
Procedure TJITAsm.add_mem32_imm32(const Mem: VMReference; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($05);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITAsm.add_mem32_reg32 *)
{
 add dword [mem], reg
}
Procedure TJITAsm.add_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($01);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.adc_mem32_imm32 *)
{
 adc dword [mem], value
}
Procedure TJITAsm.adc_mem32_imm32(const Mem: VMReference; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($15);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITAsm.adc_mem32_reg32 *)
{
 adc dword [mem], reg
}
Procedure TJITAsm.adc_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($11);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.sub_mem8_imm8 *)
{
 sub byte [mem], value
}
Procedure TJITAsm.sub_mem8_imm8(const Mem: VMReference; const Value: int8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := 5;

 emit_uint8($80);
 emit_modrm(ModRM);
 emit_uint32(Mem);
 emit_int8(Value);
End;

(* TJITAsm.sub_mem8_reg8 *)
{
 sub byte [mem], reg
}
Procedure TJITAsm.sub_mem8_reg8(const Mem: VMReference; const Reg: TRegister8);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($28);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.sub_mem32_imm32 *)
{
 sub dword [mem], value
}
Procedure TJITAsm.sub_mem32_imm32(const Mem: VMReference; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($2D);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITAsm.sub_mem32_reg32 *)
{
 sub dword [mem], reg
}
Procedure TJITAsm.sub_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($29);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.sbb_mem32_imm32 *)
{
 sbb dword [mem], value
}
Procedure TJITAsm.sbb_mem32_imm32(const Mem: VMReference; const Value: int32);
Begin
 emit_uint8($81);
 emit_uint8($1D);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITAsm.sbb_mem32_reg32 *)
Procedure TJITAsm.sbb_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.RM   := 5;
 ModRM.Reg  := ord(Reg);

 emit_uint8($19);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.mul_reg8 *)
{
 mul reg
}
Procedure TJITAsm.mul_reg8(const Reg: TRegister8);
Begin
 emit_uint8($F6);
 emit_uint8($E0+ord(Reg));
End;

(* TJITAsm.mul_reg32 *)
{
 mul reg
}
Procedure TJITAsm.mul_reg32(const Reg: TRegister32);
Begin
 emit_uint8($F7);
 emit_uint8($E0+ord(Reg));
End;

(* TJITAsm.mul_mem8 *)
Procedure TJITAsm.mul_mem8(const Mem: VMReference);
Begin
 emit_uint8($F6);
 emit_uint8($25);
 emit_uint32(Mem);
End;

(* TJITAsm.div_reg8 *)
{
 div reg
}
Procedure TJITAsm.div_reg8(const Reg: TRegister8);
Begin
 emit_uint8($F6);
 emit_uint8($F0+ord(Reg));
End;

(* TJITAsm.div_mem8 *)
{
 div byte [mem]
}
Procedure TJITAsm.div_mem8(const Mem: VMReference);
Begin
 emit_uint8($F6);
 emit_uint8($35);
 emit_uint32(Mem);
End;

(* TJITAsm.cmp_reg32_imm32 *)
{
 cmp reg, value
}
Procedure TJITAsm.cmp_reg32_imm32(const Reg: TRegister32; const Value: int32);
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

(* TJITAsm.cmp_reg32_mem32 *)
{
 cmp reg, dword [mem]
}
Procedure TJITAsm.cmp_reg32_mem32(const Reg: TRegister32; const Mem: VMReference);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($3B);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.cmp_mem8_imm8 *)
{
 cmp byte [mem], value
}
Procedure TJITAsm.cmp_mem8_imm8(const Mem: VMReference; const Value: int8);
Begin
 emit_uint8($80);
 emit_uint8($3D);
 emit_uint32(Mem);
 emit_uint8(Value);
End;

(* TJITAsm.cmp_mem32_imm32 *)
{
 cmp dword [mem], value
}
Procedure TJITAsm.cmp_mem32_imm32(const Mem: VMReference; const Value: int32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := 7;
 ModRM.RM   := 5;

 emit_uint8($81);
 emit_modrm(ModRM);
 emit_uint32(Mem);
 emit_int32(Value);
End;

(* TJITAsm.cmp_mem32_reg32 *)
{
 cmp dword [mem], reg
}
Procedure TJITAsm.cmp_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 0;
 ModRM.Reg  := ord(Reg);
 ModRM.RM   := 5;

 emit_uint8($39);
 emit_modrm(ModRM);
 emit_uint32(Mem);
End;

(* TJITAsm.jmp *)
{
 jmp address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jmp(const Address: int32): uint32;
Begin
 emit_uint8($E9);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jmp *)
{
 jmp reg
}
Procedure TJITAsm.jmp(const Reg: TRegister32);
Var ModRM: TModRM;
Begin
 ModRM.Mode := 3;
 ModRM.Reg  := 4;
 ModRM.RM   := ord(Reg);

 emit_uint8($FF);
 emit_modrm(ModRM);
End;

(* TJITAsm.jne *)
{
 jne address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jne(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($85);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jna *)
{
 jna address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jna(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($86);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jnae *)
{
 jnae address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jnae(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($82);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jnb *)
{
 jnb address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jnb(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($83);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jnbe *)
{
 jnbe address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jnbe(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($87);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.je *)
{
 je address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.je(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($84);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jg *)
{
 jg address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jg(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($8F);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.jl *)
{
 jl address

 Function returns address of the beginning of disp32 field in generated opcode.
}
Function TJITAsm.jl(const Address: int32): uint32;
Begin
 emit_uint8($0F);
 emit_uint8($8C);
 Result := getData.Position;
 emit_int32(Address);
End;

(* TJITAsm.call_internalproc *)
{
 call proc

 Note #1: this is "lazy" calling - it's resolved in late post compilation
 Note #2: passed argument must be an absolute address, it's automatically changed to relative later (see note above)
}
Procedure TJITAsm.call_internalproc(const Handler: Pointer);
Begin
 AddNewResolvableCall(uint32(Handler));

 emit_uint8($E8);
 emit_uint32(0);
End;

(* TJITAsm.fld_memfloat *)
{
 fld tword [mem]
}
Procedure TJITAsm.fld_memfloat(const Mem: VMReference);
Begin
 emit_uint8($DB);
 emit_uint8($2D);
 emit_uint32(Mem);
End;

(* TJITAsm.fild_memint *)
{
 fild qword [mem]
}
Procedure TJITAsm.fild_memint(const Mem: VMReference);
Begin
 emit_uint8($DF);
 emit_uint8($2D);
 emit_uint32(Mem);
End;

(* TJITAsm.fstp_memfloat *)
{
 fstp tword [mem]
}
Procedure TJITAsm.fstp_memfloat(const Mem: VMReference);
Begin
 emit_uint8($DB);
 emit_uint8($3D);
 emit_uint32(Mem);
End;

(* TJITAsm.fistp_memint *)
{
 fistp qword [mem]
}
Procedure TJITAsm.fistp_memint(const Mem: VMReference);
Begin
 emit_uint8($DF);
 emit_uint8($3D);
 emit_uint32(Mem);
End;

(* TJITAsm.faddp_st0 *)
{
 faddp st0, reg
}
Procedure TJITAsm.faddp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($C0 + ord(Reg));
End;

(* TJITAsm.fsubp_st0 *)
{
 fsubp st0, reg
}
Procedure TJITAsm.fsubp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($E8 + ord(Reg));
End;

(* TJITAsm.fmulp_st0 *)
{
 fmulp st0, reg
}
Procedure TJITAsm.fmulp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($C8 + ord(Reg));
End;

(* TJITAsm.fdivp_st0 *)
{
 fdivp st0, reg
}
Procedure TJITAsm.fdivp_st0(const Reg: TRegisterFPU);
Begin
 emit_uint8($DE);
 emit_uint8($F8 + ord(Reg));
End;
End.
