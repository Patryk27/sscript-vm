(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MACRO ON}
Unit JIT_Abstract_CPU;

 Interface
 Uses VM, Stack, Stream, FGL;

 {$DEFINE va := virtual abstract}

 { Float }
 Type Float = Extended;

 { TArithmeticOperation }
 Type TArithmeticOperation = (aoAdd, aoSub, aoMul, aoDiv);

 { TBitwiseOperation }
 Type TBitwiseOperation = (boOr, boXor, boAnd, boShl, boShr);

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
        Procedure write_uint8(const Value: uint8);
        Procedure write_uint32(const Value: uint32);
        Procedure write_int32(const Value: int32);

        Function AllocateInt(const Value: int64): uint64;
        Function AllocateFloat(const Value: Float): uint64;

       Public
        // move
        Procedure move_membool_immbool(const MemAddr: uint64; const Value: Boolean); va;

        Procedure move_memint_immint(const MemAddr: uint64; const Value: int64); va;
        Procedure move_memint_memint(const MemAddrDst, MemAddrSrc: uint64); va;

        Procedure move_memfloat_immfloat(const MemAddr: uint64; const Value: Float); va;
        Procedure move_memfloat_memfloat(const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure move_memfloat_immint(const MemAddr: uint64; const Value: int64); va;
        Procedure move_memfloat_memint(const MemAddrDst, MemAddrSrc: uint64); va;

        // arithmetic
        Procedure arithmetic_memint_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64); va;
        Procedure arithmetic_memint_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure arithmetic_memfloat_immfloat(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: Float); va;
        Procedure arithmetic_memfloat_memfloat(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure arithmetic_memfloat_immint(const Operation: TArithmeticOperation; const MemAddrDst: uint64; const Value: int64); va;
        Procedure arithmetic_memfloat_memint(const Operation: TArithmeticOperation; const MemAddrDst, MemAddrSrc: uint64); va;

        // binary
        Procedure bitwise_membool_immbool(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: Boolean); va;
        Procedure bitwise_membool_membool(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64); va;
        Procedure bitwise_memint_immint(const Operation: TBitwiseOperation; const MemAddrDst: uint64; const Value: int64); va;
        Procedure bitwise_memint_memint(const Operation: TBitwiseOperation; const MemAddrDst, MemAddrSrc: uint64); va;

        // bcpush
        Procedure bcpush_bool_immbool(const Value: Boolean); va;
        Procedure bcpush_bool_regbool(const MemAddr: uint64); va;

        Procedure bcpush_int_immint(const Value: uint64); va;
        Procedure bcpush_int_memint(const MemAddr: uint64); va;

        Procedure bcpush_float_immfloat(const Value: Float); va;
        Procedure bcpush_float_memfloat(const MemAddr: uint64); va;

        // bcpop
        Procedure bcpop_int_reg(const RegAddr: uint64); va;

        // icall
        Procedure do_icall(const icall: PCall; const ParamsMV, ResultMV: PMixedValue); va;

        // other
        procedure do_stop; va;

        Procedure pre_compilation; va;
        Procedure post_compilation; va;

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Property getCompiledData: TStream read CompiledData;
       End;

 Implementation

(* TJITAbstractCPU.write_uint8 *)
Procedure TJITAbstractCPU.write_uint8(const Value: uint8);
Begin
 CompiledData.write_uint8(Value);
End;

(* TJITABstractCPU.write_uint32 *)
Procedure TJITAbstractCPU.write_uint32(const Value: uint32);
Begin
 CompiledData.write_uint32(Value);
End;

(* TJITAbstractCPU.write_int32 *)
Procedure TJITAbstractCPU.write_int32(const Value: int32);
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
