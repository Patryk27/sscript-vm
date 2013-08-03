(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit JIT_Base;

 Interface
 Uses VM, Opcodes, BCReader, Stream, FGL;

 Type TIntList = specialize TFPGList<uint32>;

 { TJITCompilerBase }
 Type TJITCompilerBase = Class
                          Protected
                           VM: PVM;

                           BCReader     : TBCReader;
                           CompiledData : TStream;
                           CompiledState: TJITCompiledState;

                           AllocatedDataBlocks: TIntList;

                           Function getRegMemAddr(const Arg: TOpcodeArg): uint32;
                           Function getIFRegMemAddr: uint32;
                           Function getSTPRegMemAddr: uint32;

                           Function AllocateFloat(const Value: Extended): uint32;
                           Function AllocateInt64(const Value: int64): uint32;
                           Function AllocateString(const Value: String): uint32;

                          Public
                           Constructor Create(const fVM: PVM; const BytecodeData: Pointer);
                           Destructor Destroy; override;

                           Procedure Compile; virtual;

                           Property getCompiledData: TStream read CompiledData;
                           Property getCompiledState: TJITCompiledState read CompiledState;
                          End;

 Implementation
Uses mStrings;

(* TJITCompilerBase.getRegMemAddr *)
Function TJITCompilerBase.getRegMemAddr(const Arg: TOpcodeArg): uint32;
Begin
 Result := 0;

 Case Arg.ArgType of
  ptBoolReg     : Result := uint32(@VM^.Regs.b[Arg.RegID]);
  ptCharReg     : Result := uint32(@VM^.Regs.c[Arg.RegID]);
  ptIntReg      : Result := uint32(@VM^.Regs.i[Arg.RegID]);
  ptFloatReg    : Result := uint32(@VM^.Regs.f[Arg.RegID]);
  ptStringReg   : Result := uint32(@VM^.Regs.s[Arg.RegID]);
  ptReferenceReg: Result := uint32(@VM^.Regs.r[Arg.RegID]);
 End;
End;

(* TJITCompilerBase.getIFRegMemAddr *)
Function TJITCompilerBase.getIFRegMemAddr: uint32;
Begin
 Result := uint32(@VM^.Regs.b[5]);
End;

(* TJITCompilerBase.getSTPRegMemAddr *)
Function TJITCompilerBase.getSTPRegMemAddr: uint32;
Begin
 Result := uint32(@VM^.Regs.i[5]);
End;

(* TJITCompilerBase.AllocateFloat *)
Function TJITCompilerBase.AllocateFloat(const Value: Extended): uint32;
Begin
 Result             := uint32(AllocMem(sizeof(Value)));
 PExtended(Result)^ := Value;

 AllocatedDataBlocks.Add(Result);
End;

(* TJITCompilerBase.AllocateInt64 *)
Function TJITCompilerBase.AllocateInt64(const Value: int64): uint32;
Begin
 Result          := uint32(AllocMem(sizeof(Value)));
 Pint64(Result)^ := Value;

 AllocatedDataBlocks.Add(Result);
End;

(* TJITCompilerBase.AllocateString *)
Function TJITCompilerBase.AllocateString(const Value: String): uint32;
Begin
 Result := uint32(CopyStringToPChar(Value));

 AllocatedDataBlocks.Add(Result);
End;

// -------------------------------------------------------------------------- //
(* TJITCompilerBase.Create *)
Constructor TJITCompilerBase.Create(const fVM: PVM; const BytecodeData: Pointer);
Begin
 VM := fVM;

 BCReader := TBCReader.Create(BytecodeData);

 CompiledData  := TStream.Create(False);
 CompiledState := csDone;

 AllocatedDataBlocks := TIntList.Create;
End;

(* TJITCompilerBase.Destroy *)
Destructor TJITCompilerBase.Destroy;
Var Pnt: uint32;
Begin
 For Pnt in AllocatedDataBlocks Do
  FreeMem(Pointer(Pnt));

 AllocatedDataBlocks.Free;
 CompiledData.Free;
 BCReader.Free;
End;

(* TJITCompilerBase.Compile *)
Procedure TJITCompilerBase.Compile;
Begin
 CompiledState := csJITFailed;
End;
End.
