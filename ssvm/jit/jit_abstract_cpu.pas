(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MACRO ON}
Unit JIT_Abstract_CPU;

 Interface
 Uses VM, Stack, Stream, Opcodes, FGL, Variants;

 {$DEFINE va := virtual abstract}

 { Float }
 Type Float = Extended;

 { TArithmeticOperation }
 Type TArithmeticOperation = (ao_add, ao_sub, ao_mul, ao_div, ao_mod);

 { TBitwiseOperation }
 Type TBitwiseOperation = (bo_or, bo_xor, bo_and, bo_shl, bo_shr);

 { TCompareOperation }
 Type TCompareOperation = (co_equal, co_different, co_greater, co_greater_equal, co_lower, co_lower_equal);

 { TBytecodeRegister }
 Type TBytecodeRegister = (reg_eb, reg_ec, reg_ei, reg_ef, reg_es, reg_er);

 { TPointerList }
 Type TPointerList = specialize TFPGList<Pointer>;

 { TJITAbstractCPU }
 Type TJITAbstractCPU =
      Class
       Private
        VM           : PVM;
        CompiledData : TStream;
        AllocatedData: TPointerList;

       Protected
        Property getVM: PVM read VM;

       Protected
        Procedure emit_int8(const Value: int8);
        Procedure emit_uint8(const Value: uint8);
        Procedure emit_uint32(const Value: uint32);
        Procedure emit_int32(const Value: int32);

        Function AllocateInt(const Value: int64): uint64;
        Function AllocateFloat(const Value: Float): uint64;
        Function AllocateString(const Value: String): uint64;

       Public
        // move
        Procedure move_membool_immbool(const MemAddr: uint64; const Value: Boolean); va;
        Procedure move_membool_membool(const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure move_memchar_immchar(const MemAddr: uint64; const Value: Char); va;
        Procedure move_memchar_memchar(const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure move_memint_immint(const MemAddr: uint64; const Value: int64); va;
        Procedure move_memint_memint(const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure move_memfloat_immfloat(const MemAddr: uint64; const Value: Float); va;
        Procedure move_memfloat_memfloat(const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure move_memfloat_immint(const MemAddr: uint64; const Value: int64); va;
        Procedure move_memfloat_memint(const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure move_memstring_immstring(const MemAddr: uint64; const Value: String); va;
        Procedure move_memstring_memstring(const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure move_stackval_imm(const StackvalPos: int32; const Value: Variant; const ArgType: TOpcodeArgType); va;
        Procedure move_register_stackval(const MemAddr: uint64; const RegType: TBytecodeRegister; const StackvalPos: int32); va;

        // arithmetic
        Procedure arithmetic_memint_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64); va;
        Procedure arithmetic_memint_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure arithmetic_memfloat_immfloat(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: Float); va;
        Procedure arithmetic_memfloat_memfloat(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure arithmetic_memfloat_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64); va;
        Procedure arithmetic_memfloat_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure arithmetic_stackval_immint(const Operation: TArithmeticOperation; const StackvalPos: int32; const Value: int64); va;
        Procedure arithmetic_stackval_memint(const Operation: TArithmeticOperation; const StackvalPos: int32; const MemAddr: uint64); va;
        Procedure arithmetic_stackval_immfloat(const Operation: TArithmeticOperation; const StackvalPos: int32; const Value: Float); va;
        Procedure arithmetic_stackval_memfloat(const Operation: TArithmeticOperation; const StackvalPos: int32; const MemAddr: uint64); va;

        // bitwise
        Procedure bitwise_membool_immbool(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: Boolean); va;
        Procedure bitwise_membool_membool(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure bitwise_memint_immint(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: int64); va;
        Procedure bitwise_memint_memint(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64); va;

        // compare
        Procedure compare_immint_immint(const Operation: TCompareOperation; const Value0, Value1: int64); va;
        Procedure compare_immint_immfloat(const Operation: TCompareOperation; const Value0: int64; const Value1: Float); va;
        Procedure compare_immint_memint(const Operation: TCompareOperation; const Value0: int64; const NumberPnt1: uint64); va;
        Procedure compare_immint_memfloat(const Operation: TCompareOperation; const Value0: int64; const NumberPnt1: uint64); va;

        Procedure compare_memint_immint(const Operation: TCompareOperation; const NumberPnt0: uint64; const Value1: int64); va;
        Procedure compare_memint_immfloat(const Operation: TCompareOperation; const NumberPnt0: uint64; const Value1: Float); va;
        Procedure compare_memint_memint(const Operation: TCompareOperation; const NumberPnt0, NumberPnt1: uint64); va;
        Procedure compare_memint_memfloat(const Operation: TCompareOperation; const NumberPnt0, NumberPnt1: uint64); va;

        Procedure compare_immfloat_immint(const Operation: TCompareOperation; const Value0: Float; const Value1: int64); va;
        Procedure compare_immfloat_immfloat(const Operation: TCompareOperation; const Value0, Value1: Float); va;
        Procedure compare_immfloat_memfloat(const Operation: TCompareOperation; const Value0: Float; const NumberPnt1: uint64); va;
        Procedure compare_immfloat_memint(const Operation: TCompareOperation; const Value0: Float; const NumberPnt1: uint64); va;

        Procedure compare_memfloat_immint(const Operation: TCompareOperation; const NumberPnt0: uint64; const Value1: int64); va;
        Procedure compare_memfloat_memint(const Operation: TCompareOperation; const NumberPnt0, NumberPnt1: uint64); va;
        Procedure compare_memfloat_immfloat(const Operation: TCompareOperation; const NumberPnt0: uint64; const Value1: Float); va;
        Procedure compare_memfloat_memfloat(const Operation: TCompareOperation; const NumberPnt0, NumberPnt1: uint64); va;

        // strings
        Procedure strjoin_memstring_immstring(const MemAddr: uint64; const Value: String); va;
        Procedure strjoin_memstring_memstring(const MemAddrDst, MemAddrSrc: uint64); va;

        // bcpush
        Procedure bcpush_immbool(const Value: Boolean); va;
        Procedure bcpush_immchar(const Value: Char); va;
        Procedure bcpush_immint(const Value: uint64); va;
        Procedure bcpush_immfloat(const Value: Float); va;
        Procedure bcpush_immstring(const Value: String); va;

        Procedure bcpush_reg(const RegType: TBytecodeRegister; const RegAddr: uint64); va;

        Procedure bcpush_stackval(const StackvalPos: int32); va;

        // bcpop
        Procedure bcpop_reg(const RegType: TBytecodeRegister; const RegAddr: uint64); va;

        // icall
        Procedure do_icall(const icall: PCall; const ParamsMV, ResultMV: PMixedValue); va;

        // jumps and calls
        Procedure do_bcjump(const Address: uint64); va;
        Procedure do_bccondjump(const Address: uint64; const Opcode: TOpcode_E); va;
        Procedure do_bccall(const Address: uint64); va;
        Procedure do_bcret; va;

        // other
        Procedure do_nop; va;
        Procedure do_stop; va;

        Procedure pre_compilation; va;
        Procedure post_compilation; va;

        // half-properties
        Function get_bccall_size: uint8; va;
        Function get_bcconditionaljump_size: uint8; va;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Property getCompiledData: TStream read CompiledData;
       End;

 Implementation
Uses VMStrings;

(* TJITAbstractCPU.emit_int8 *)
Procedure TJITAbstractCPU.emit_int8(const Value: int8);
Begin
 CompiledData.write_int8(Value);
End;

(* TJITAbstractCPU.emit_uint8 *)
Procedure TJITAbstractCPU.emit_uint8(const Value: uint8);
Begin
 CompiledData.write_uint8(Value);
End;

(* TJITABstractCPU.emit_uint32 *)
Procedure TJITAbstractCPU.emit_uint32(const Value: uint32);
Begin
 CompiledData.write_uint32(Value);
End;

(* TJITAbstractCPU.emit_int32 *)
Procedure TJITAbstractCPU.emit_int32(const Value: int32);
Begin
 CompiledData.write_int32(Value);
End;

(* TJITAbstractCPU.AllocateInt *)
Function TJITAbstractCPU.AllocateInt(const Value: int64): uint64;
Begin
 AllocatedData.Add(AllocMem(sizeof(Value)));

 Result          := uint64(AllocatedData.Last);
 Pint64(Result)^ := Value;
End;

(* TJITAbstractCPU.AllocateFloat *)
Function TJITAbstractCPU.AllocateFloat(const Value: Float): uint64;
Begin
 AllocatedData.Add(AllocMem(sizeof(Value)));

 Result             := uint64(AllocatedData.Last);
 PExtended(Result)^ := Value;
End;

(* TJITAbstractCPU.AllocateString *)
Function TJITAbstractCPU.AllocateString(const Value: String): uint64;
Begin
 AllocatedData.Add(CopyStringToPChar(Value));

 Result := uint64(AllocatedData.Last);
End;

(* TJITAbstractCPU.Create *)
Constructor TJITAbstractCPU.Create(const fVM: PVM);
Begin
 VM := fVM;

 CompiledData  := TStream.Create(False);
 AllocatedData := TPointerList.Create;
End;

(* TJITAbstractCPU.Destroy *)
Destructor TJITAbstractCPU.Destroy;
Begin
 inherited Destroy;

 CompiledData.Free;
 AllocatedData.Free;
End;
End.
