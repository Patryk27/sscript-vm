(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 Bytecode reader class.
*)
Unit BCReader;

 Interface
 Uses Stream, Opcodes, MTypes;

 Type TBCReader = Class
                   Private
                    BytecodeData: TStream;

                   Public
                    Constructor Create(const fBytecodeData: Pointer);
                    Destructor Destroy; override;

                    Function FetchOpcode: TOpcode_E;
                    Function FetchArgument: TOpcodeArg;
                    Function FetchArguments(const Opcode: TOpcode_E): TOpcodeArgArray;

                    Function AnyOpcodeLeft: Boolean;

                    Property getBytecodeData: TStream read BytecodeData;
                   End;

 Implementation
Uses SysUtils;

(* TBCReader.Create *)
Constructor TBCReader.Create(const fBytecodeData: Pointer);
Var I: uint32;
Begin
 if (MemSize(fBytecodeData) = 0) Then
  raise Exception.Create('Cannot create TBCReader instance: MemSize(fBytecodeData) = 0');

 BytecodeData := TStream.Create(True);
 For I := 0 To MemSize(fBytecodeData)-1 Do
  BytecodeData.write_uint8(puint8(fBytecodeData+I)^);
 BytecodeData.Position := 0;
End;

(* TBCReader.Destroy *)
Destructor TBCReader.Destroy;
Begin
 BytecodeData.Free;

 inherited;
End;

(* TBCReader.FetchOpcode *)
Function TBCReader.FetchOpcode: TOpcode_E;
Begin
 Result := TOpcode_E(BytecodeData.read_uint8);
End;

(* TBCReader.FetchArgument *)
Function TBCReader.FetchArgument: TOpcodeArg;
Begin
 Result.ArgType := TOpcodeArgType(BytecodeData.read_uint8);

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

(* TBCReader.FetchArguments *)
Function TBCReader.FetchArguments(const Opcode: TOpcode_E): TOpcodeArgArray;
Var I: Integer;
Begin
 SetLength(Result, OpcodeArgCount[Opcode]);

 For I := Low(Result) To High(Result) Do
  Result[I] := FetchArgument;
End;

(* TBCReader.AnyOpcodeLeft *)
Function TBCReader.AnyOpcodeLeft: Boolean;
Begin
 Result := BytecodeData.Can;
End;
End.
