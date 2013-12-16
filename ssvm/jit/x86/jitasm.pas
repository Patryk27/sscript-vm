(*
 Copyright © by Patryk Wychowaniec, 2013
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

        Procedure emit_int32(const Value: int32);

        Procedure emit_modrm(const Value: TModRM);

        Procedure AddNewResolvableCall(const AddressToCall: uint32);

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure post_compilation;

       Public
        Procedure nop;
        Procedure ret;

        // mov
        Procedure mov_mem32_imm32(const Mem: VMReference; const Value: int32);

        Procedure mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
        Procedure mov_reg32_mem32(const Reg: TRegister32; const Mem: VMReference);

        // add
        Procedure add_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // adc
        Procedure adc_mem32_reg32(const Mem: VMReference; const Reg: TRegister32);

        // call
        Procedure call_internalproc(const Handler: Pointer);

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
    Addr: int32;
Begin
 For Call in ResolvableCalls Do
 Begin
  Data.Position := int64(Call.OpcodeAddress) + 1; // '+1' because we must skip the opcode ($E8 in this case)
  Addr          := Data.read_uint32;
  Data.Position := Data.Position - 4;

  Data.write_int32(Addr - (uint32(Data.Memory) + Data.Position) - 4);
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

(* TJITAsm.mov_reg32_imm32 *)
{
 mov reg, value
}
Procedure TJITAsm.mov_reg32_imm32(const Reg: TRegister32; const Value: int32);
Begin
 emit_uint8($B8+ord(Reg));
 emit_int32(Value);
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

(* TJITAsm.call_internalproc *)
{
 call proc

 Note #1: this is "lazy" calling - it's resolved in last post compilation
 Note #2: passed argument must be an absolute address
}
Procedure TJITAsm.call_internalproc(const Handler: Pointer);
Begin
 AddNewResolvableCall(uint32(Handler));

 emit_uint8($E8);
 emit_uint32(uint32(Handler));
End;
End.
