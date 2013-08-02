(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit JIT_Base;

 Interface
 Uses VM, Opcodes, Stream;

 Type TJITOpcodeArgType = (ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg, ptBool, ptChar, ptInt, ptFloat, ptString, ptStackval);

 { TJITOpcodeArg }
 Type TJITOpcodeArg = Record
                       Case ArgType: TJITOpcodeArgType of
                        ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg: (RegID: uint8); // @TODO: 'ptBoolReg..ptReferenceReg' (but it makes Lazarus's code completion not working :/)

                        ptBool    : (ImmBool: Boolean);
                        ptChar    : (ImmChar: Char);
                        ptInt     : (ImmInt: Int64);
                        ptFloat   : (ImmFloat: Extended);
                        ptString  : (ImmString: String);
                        ptStackval: (StackvalPos: int32);
                       End;

 Type TJITOpcodeArgArray = Array of TJITOpcodeArg;

 { TJITCompilerBase }
 Type TJITCompilerBase = Class
                          Protected
                           VM: PVM;

                           BytecodeData: TStream;
                           CompiledData: TStream;

                           CompiledState: TJITCompiledState;

                           Function FetchOpcode: uint8;
                           Function FetchArgument: TJITOpcodeArg;
                           Function FetchArguments(const Opcode: uint8): TJITOpcodeArgArray;
                           Function isLocationOpcode(const Opcode: uint8): Boolean;

                           Function getRegMemAddr(const Arg: TJITOpcodeArg): uint32;
                           Function getIFRegMemAddr: uint32;
                           Function getSTPRegMemAddr: uint32;

                           Function AllocateFloat(const Value: Extended): uint32;
                           Function AllocateInt64(const Value: int64): uint32;
                           Function AllocateString(const Value: String): uint32;

                          Public
                           Constructor Create(const fVM: PVM);
                           Destructor Destroy; override;

                           Procedure LoadBytecode(const BytecodePnt: Pointer);
                           Procedure Compile; virtual;

                           Property getCompiledData: TStream read CompiledData;
                           Property getCompiledState: TJITCompiledState read CompiledState;
                          End;

 Implementation
Uses mStrings;

(* TJITCompilerBase.FetchOpcode *)
Function TJITCompilerBase.FetchOpcode: uint8;
Begin
 if (not BytecodeData.Can) Then
  Exit($FF);

 Result := BytecodeData.read_uint8;
End;

(* TJITCompilerBase.FetchArgument *)
Function TJITCompilerBase.FetchArgument: TJITOpcodeArg;
Begin
 Result.ArgType := TJITOpcodeArgType(BytecodeData.read_uint8);

 Case Result.ArgType of
  ptBoolReg..ptReferenceReg: Result.RegID       := BytecodeData.read_uint8;
  ptBool                   : Result.ImmBool     := Boolean(BytecodeData.read_uint8);
  ptChar                   : Result.ImmChar     := Char(BytecodeData.read_uint8);
  ptInt                    : Result.ImmInt      := BytecodeData.read_int64;
  ptFloat                  : Result.ImmFloat    := BytecodeData.read_float;
  ptString                 : Result.ImmString   := BytecodeData.read_string;
  ptStackval               : Result.StackvalPos := BytecodeData.read_int32;

  else
   BytecodeData.read_int32;
 End;
End;

(* TJITCompilerBase.FetchArguments *)
Function TJITCompilerBase.FetchArguments(const Opcode: uint8): TJITOpcodeArgArray;
Var I: Integer;
Begin
 SetLength(Result, OpcodeArgCount[TOpcode_E(Opcode)]);

 For I := Low(Result) To High(Result) Do
  Result[I] := FetchArgument;
End;

(* TJITCompilerBase.isLocationOpcode *)
Function TJITCompilerBase.isLocationOpcode(const Opcode: uint8): Boolean;
Begin
 Result := Opcode in [ord(o_loc_file), ord(o_loc_func), ord(o_loc_line)];
End;

(* TJITCompilerBase.getRegMemAddr *)
Function TJITCompilerBase.getRegMemAddr(const Arg: TJITOpcodeArg): uint32;
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

 // AllocateDataBlocks.Add(Result);
End;

(* TJITCompilerBase.AllocateInt64 *)
Function TJITCompilerBase.AllocateInt64(const Value: int64): uint32;
Begin
 Result          := uint32(AllocMem(sizeof(Value)));
 Pint64(Result)^ := Value;

 // AllocateDataBlocks.Add(Result);
End;

(* TJITCompilerBase.AllocateString *)
Function TJITCompilerBase.AllocateString(const Value: String): uint32;
Begin
 Result := uint32(CopyStringToPChar(Value));

 // AllocateDataBlocks.Add(Result);
End;

// -------------------------------------------------------------------------- //
(* TJITCompilerBase.Create *)
Constructor TJITCompilerBase.Create(const fVM: PVM);
Begin
 VM := fVM;

 CompiledData  := TStream.Create(False);
 CompiledState := csDone;
End;

(* TJITCompilerBase.Destroy *)
Destructor TJITCompilerBase.Destroy;
Begin
// CompiledData.Free;
End;

(* TJITCompilerBase.LoadBytecode *)
Procedure TJITCompilerBase.LoadBytecode(const BytecodePnt: Pointer);
Type puint8 = ^uint8;
Var I: uint32 = 0;
Begin
 BytecodeData          := TStream.Create(True);
 BytecodeData.Position := 0;

 // @TODO: why "BytecodeData.WriteBuffer" doesn't work?
 While (I < MemSize(BytecodePnt)) Do
 Begin
  BytecodeData.write_uint8(puint8(BytecodePnt+I)^);
  Inc(I);
 End;

 BytecodeData.Position := 0;
End;

(* TJITCompilerBase.Compile *)
Procedure TJITCompilerBase.Compile;
Begin
 CompiledState := csJITFailed;
End;
End.
