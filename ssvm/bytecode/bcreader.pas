(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 -------------------
 Bytecode reader class.
*)
Unit BCReader;

 Interface
 Uses Stream, Opcodes, VMTypes;

 { TBytecodeReader }
 Type TBytecodeReader =
      Class
       Private
        BytecodeData: TStream;

       Public
        Constructor Create(const fBytecodeData: Pointer);
        Destructor Destroy; override;

        Function FetchOpcode: TOpcode_E;
        Procedure FetchOpcode(out Opcode: TOpcode_E; out Args: TOpcodeArgArray);
        Function FetchArgument: TOpcodeArg;
        Function FetchArguments(const Opcode: TOpcode_E): TOpcodeArgArray;

        Function OpcodeToString(const Opcode: TOpcode_E; const Args: TOpcodeArgArray): String;

        Function AnyOpcodeLeft: Boolean;

       Public
        Property getBytecodeData: TStream read BytecodeData;
       End;

 Implementation
Uses SysUtils, TypInfo;

(* TBytecodeReader.Create *)
Constructor TBytecodeReader.Create(const fBytecodeData: Pointer);
Var I: uint32;
Begin
 if (MemSize(fBytecodeData) = 0) Then
  raise Exception.Create('Cannot create TBytecodeReader instance: MemSize(fBytecodeData) = 0');

 BytecodeData := TStream.Create(True);
 For I := 0 To MemSize(fBytecodeData)-1 Do
  BytecodeData.write_uint8(puint8(fBytecodeData+I)^);
 BytecodeData.Position := 0;
End;

(* TBytecodeReader.Destroy *)
Destructor TBytecodeReader.Destroy;
Begin
 BytecodeData.Free;

 inherited;
End;

(* TBytecodeReader.FetchOpcode *)
Function TBytecodeReader.FetchOpcode: TOpcode_E;
Begin
 Result := TOpcode_E(BytecodeData.read_uint8);
End;

(* TBytecodeReader.FetchOpcode *)
Procedure TBytecodeReader.FetchOpcode(out Opcode: TOpcode_E; out Args: TOpcodeArgArray);
Begin
 Opcode := FetchOpcode;
 Args   := FetchArguments(Opcode);
End;

(* TBytecodeReader.FetchArgument *)
Function TBytecodeReader.FetchArgument: TOpcodeArg;
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
   Result.ImmInt := BytecodeData.read_int32;
 End;
End;

(* TBytecodeReader.FetchArguments *)
Function TBytecodeReader.FetchArguments(const Opcode: TOpcode_E): TOpcodeArgArray;
Var I: Integer;
Begin
 SetLength(Result, OpcodeArgCount[Opcode]);

 For I := Low(Result) To High(Result) Do
  Result[I] := FetchArgument;
End;

(* TBytecodeReader.OpcodeToString *)
Function TBytecodeReader.OpcodeToString(const Opcode: TOpcode_E; const Args: TOpcodeArgArray): String;
Const BoolRegsNames: Array[1..5] of String = ('eb1', 'eb2', 'eb3', 'eb4', 'if');
      IntRegsNames : Array[1..5] of String = ('ei1', 'ei2', 'ei3', 'ei4', 'stp');
      BoolTable    : Array[Boolean] of String = ('false', 'true');
Var I: int8;
Begin
 Result := Copy(GetEnumName(TypeInfo(Opcode), ord(Opcode)), 3, 10) + '(';

 For I := Low(Args) To High(Args) Do
 Begin
  Case Args[I].ArgType of
   ptBoolReg     : Result += BoolRegsNames[Args[I].RegID];
   ptCharReg     : Result += 'ec'+IntToStr(Args[I].RegID);
   ptIntReg      : Result += IntRegsNames[Args[I].RegID];
   ptFloatReg    : Result += 'ef'+IntToStr(Args[I].RegID);
   ptStringReg   : Result += 'es'+IntToStr(Args[I].RegID);
   ptReferenceReg: Result += 'er'+IntToStr(Args[I].RegID);

   ptBool  : Result += BoolTable[Args[I].ImmBool];
   ptChar  : Result += '#'+IntToStr(ord(Args[I].ImmChar));
   ptInt   : Result += IntToStr(Args[I].ImmInt);
   ptFloat : Result += FloatToStr(Args[I].ImmFloat);
   ptString: Result += '"'+Args[I].ImmString+'"';

   ptStackval: Result += '['+IntToStr(Args[I].StackvalPos)+']';

   ptConstantMemRef: Result += '&'+IntToStr(Args[I].MemoryAddress);

   else
    raise Exception.CreateFmt('TBytecodeReader.OpcodeToString -> invalid opcode argument (type = %d)', [ord(Args[I].ArgType)]);
  End;

  if (I <> High(Args)) Then
   Result += ', ';
 End;

 Result += ')';
End;

(* TBytecodeReader.AnyOpcodeLeft *)
Function TBytecodeReader.AnyOpcodeLeft: Boolean;
Begin
 Result := BytecodeData.Can;
End;
End.
