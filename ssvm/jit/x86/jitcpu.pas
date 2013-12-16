(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JITCPU;

 // @TODO: ei1 -> eax:ebx, es1 -> ecx, eb1 -> edx?
 // Ew.zrobić to dynamicznie - te najczęściej używane rejestry lądują w fizycznych rejestrach CPU, a reszta sio do pamięci; najlepiej jeszcze, aby taki "najczęściej używany" wybierany był przez kompilator lub maszynę na poziomie każdej funkcji.

 Interface
 Uses VM, Stack, Opcodes, VMTypes, JITAbstractCPU, JITOpcodes, JITOpcodeList, JITAsm;

 {$MACRO ON}
 {$DEFINE ov := override}

 { TJITCPU }
 Type TJITCPU =
      Class (TJITAbstractCPU)
       Private
        JAsm: TJITAsm;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile(const OpcodeList: TJITOpcodeList): Pointer; ov;

        Function hasNativeReg(const Kind: TBytecodeRegister; const ID: uint8): Boolean; ov;
       End;

 Implementation
Uses SysUtils, Variants;

// ------------------------ //
{$I routines.pas}
// ------------------------ //

(* TJITCPU.Create *)
Constructor TJITCPU.Create(const fVM: PVM);
Begin
 inherited Create(fVM);

 JAsm := TJITAsm.Create;
End;

(* TJITCPU.Destroy *)
Destructor TJITCPU.Destroy;
Begin
 JAsm.Free;

 inherited Destroy;
End;

(* TJITCPU.Compile *)
Function TJITCPU.Compile(const OpcodeList: TJITOpcodeList): Pointer;
Var OpcodeID  : uint32;
    Opcode    : TJITOpcode;
    Arg0, Arg1: TJITOpcodeArg;
    icall     : PCall;

    ResultMV, ParamsMV: PMixedValue;

    TmpInt: Int64;

  { InvalidOpcodeException }
  Procedure InvalidOpcodeException;
  Begin
   raise Exception.CreateFmt('TJITCPU.Compile() failed; invalid JIT opcode (opcode index=%d)', [OpcodeID]);
  End;

Begin
 if (OpcodeList.getSize = 0) Then
  raise Exception.Create('TJITCPU.Compile() -> OpcodeList.getSize() = 0 (shouldn''t happen!)');

 if (JAsm.getData.Size > 0) Then
  raise Exception.CreateFmt('TJITCPU.Compile() called more than once on one TJITCPU instance (JAsm.getData.Size() = %d)', [JAsm.getData.Size]);

 Result := nil;

 ResultMV := JITMemAlloc(sizeof(TMixedValue)); // used in icall-s
 ParamsMV := JITMemAlloc(sizeof(TMixedValue)); // ditto

 Try
  For OpcodeID := 0 To OpcodeList.getSize-1 Do
  Begin
   Opcode := OpcodeList[OpcodeID];
   Arg0   := Opcode.Args[0];
   Arg1   := Opcode.Args[1];

   Case Opcode.ID of
    { ipush }
    jo_ipush:
    Begin
     JAsm.mov_reg32_imm32(reg_eax, uint32(getVM));

     Case Arg0.Kind of
      joa_memory:
      Begin
       JAsm.mov_reg32_mem32(reg_edx, Arg0.MemoryAddr+0);
       JAsm.mov_reg32_mem32(reg_ecx, Arg0.MemoryAddr+4);
      End;

      joa_constant:
      Begin
       TmpInt := Arg0.Constant;

       JAsm.mov_reg32_imm32(reg_edx, lo(TmpInt));
       JAsm.mov_reg32_imm32(reg_ecx, hi(TmpInt));
      End;

      else
       InvalidOpcodeException;
     End;

     JAsm.call_internalproc(@r__push_int);
    End;

    { iimov }
    jo_iimov:
    Begin
     if (Arg0.Kind = joa_memory) Then
     Begin
      TmpInt := Arg1.Constant;

      JAsm.mov_mem32_imm32(Arg0.MemoryAddr+0, lo(TmpInt));
      JAsm.mov_mem32_imm32(Arg0.MemoryAddr+4, hi(TmpInt));
     End Else

      InvalidOpcodeException;
    End;

    { iiadd }
    jo_iiadd:
    Begin
     if (Arg0.Kind = joa_memory) and (Arg1.Kind = joa_memory) Then
     Begin
      JAsm.mov_reg32_mem32(reg_eax, Arg1.MemoryAddr+0);
      JAsm.mov_reg32_mem32(reg_ebx, Arg1.MemoryAddr+4);
      JAsm.add_mem32_reg32(Arg0.MemoryAddr+0, reg_eax);
      JAsm.adc_mem32_reg32(Arg0.MemoryAddr+4, reg_ebx);
     End Else

      InvalidOpcodeException;
    End;

    { icall }
    jo_icall:
    Begin
     if (Arg0.Kind <> joa_memory) Then // check parameter type
      InvalidOpcodeException;

     // fetch pointer
     icall := Arg0.MemoryAddr;

     // prepare parameter list
     JAsm.mov_reg32_imm32(reg_eax, uint32(getVM));
     JAsm.mov_reg32_imm32(reg_edx, uint32(icall));
     JAsm.mov_reg32_imm32(reg_ecx, uint32(ParamsMV));
     JAsm.call_internalproc(@r__create_icall_parameter_list);

     // clean result
     JAsm.mov_reg32_imm32(reg_eax, uint32(ResultMV));
     JAsm.call_internalproc(@r__clean_mixedvalue);

     // call icall
     JAsm.mov_reg32_imm32(reg_eax, uint32(getVM));
     JAsm.mov_reg32_imm32(reg_edx, uint32(ParamsMV));
     JAsm.mov_reg32_imm32(reg_ecx, uint32(ResultMV));
     JAsm.call_internalproc(icall^.Handler);

     // apply result; eax is preserved from the previous call, so we don't have to assign it again
     JAsm.mov_reg32_imm32(reg_edx, uint32(ResultMV));
     JAsm.call_internalproc(@r__apply_mixedvalue);
    End;

    { stop }
    jo_stop:
    Begin
     JAsm.ret;
    End;

    { invalid opcode }
    else
     raise Exception.CreateFmt('TJITCPU.Compile() -> invalid opcode (kind=#%d)', [ord(Opcode.ID)]);
   End;
  End;
 Finally
 End;

 // post compilation
 JAsm.getData.Position := 0;
 JAsm.post_compilation;

 // do other "post" things
 //JAsm.getData.SaveToFile('jit_compiled.o');

 Result := JAsm.getData.getMemoryPosition;
End;

(* TJITCPU.hasNativeReg *)
Function TJITCPU.hasNativeReg(const Kind: TBytecodeRegister; const ID: uint8): Boolean;
Begin
 {Case Kind of
  reg_ei: Result := (ID = 1); // "ei1"

  else
   Result := False;
 End;}

 Result := False;
End;
End.
