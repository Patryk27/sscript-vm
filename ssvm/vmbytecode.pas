(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
{$CODEALIGN PROC=16}
Unit VMBytecode;

 Interface
 Uses VMElement, VMStack, VMTypes, Opcodes;

 { TVMBytecode }
 Type TVMBytecode =
      Class (TVMElement)
       Private
        Data         : PByte; // pointer to the unpacked bytecode code data block
        Position     : PByte; // current pointer position in memory
        CurrentOpcode: PByte; // first byte of currently parsed opcode

       Public
        Constructor Create(const fVM: Pointer);

        Function read_uint8: uint8; inline;
        Function read_uint16: uint16; inline;
        Function read_uint32: uint32; inline;

        Function read_int8: int8; inline;
        Function read_int16: int16; inline;
        Function read_int32: int32; inline;
        Function read_int64: int64; inline;

        Function read_float: Extended; inline;
        Function read_string: String; inline;

        Function read_param(const CloneIfString: Boolean=False): TMixedValue; inline;

       Public
        Destructor Destroy; override;

        Procedure Execute;

        Procedure setPosition(const Value: PByte);
        Procedure setRelativePosition(const Value: VMIReference);

        Function getRelativePosition: VMIReference;

        Function getLineData(const Address: uint32; out FileName, FunctionName: String; out FunctionLine: uint32): Boolean;

       Public
        Property getData: PByte read Data;
        Property getPosition: PByte read Position;
        Property getCurrentOpcode: PByte read CurrentOpcode;
       End;

 Implementation
Uses VMStruct, DbgParser, Interpreter;

(* TVMBytecode.Create *)
Constructor TVMBytecode.Create(const fVM: Pointer);
Begin
 inherited Create(fVM);

 Data := getLoaderData.CodeData;
End;

(* TVMBytecode.read_uint8 *)
{
 Reads an uint8
}
Function TVMBytecode.read_uint8: uint8;
Begin
 Result := Position^;
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_uint16 *)
{
 Reads an uint16
}
Function TVMBytecode.read_uint16: uint16;
Begin
 Result := BEtoN(Puint16(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_uint32 *)
{
 Reads an uint32
}
Function TVMBytecode.read_uint32: uint32;
Begin
 Result := BEtoN(Puint32(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_int8 *)
{
 Reads an int8
}
Function TVMBytecode.read_int8: int8;
Begin
 Result := Position^;
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_int16 *)
{
 Reads an int16
}
Function TVMBytecode.read_int16: int16;
Begin
 Result := BEtoN(Pint16(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_int32 *)
{
 Reads an int32
}
Function TVMBytecode.read_int32: int32;
Begin
 Result := BEtoN(Pint32(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_int64 *)
{
 Reads an int64
}
Function TVMBytecode.read_int64: int64;
Begin
 Result := BEtoN(Pint64(Position)^);
 Inc(Position, sizeof(Result));
End;

(* TVMBytecode.read_float *)
{
 Reads a float
}
Function TVMBytecode.read_float: Extended;
Begin
 Result := PExtended(Position)^;

 Inc(Position, 10);
End;

(* TVMBytecode.read_string *)
{
 Reads a string
}
Function TVMBytecode.read_string: String;
Begin
 Result := '';

 While (Position^ <> 0) Do
 Begin
  Result += chr(Position^);
  Inc(Position);
 End;

 Inc(Position); // skip the null termintor char
End;

(* TVMBytecode.read_param *)
{
 Reads an opcode's parameters.

 If "CloneIfString" equals 'true', when 'Typ = ptStringReg', there's returned a copy of that string, not it itself.
 It's implemented only to lower the VM memory usage and also speed up execution a bit (lower number of memory allocations per instruction).
 Used eg.in "push(string reg)" opcode.
}
Function TVMBytecode.read_param(const CloneIfString: Boolean): TMixedValue;
Const TypeMap: Array[TOpcodeArgType] of TMixedValueType =
(
 mvBool, // <- ptBoolReg
 mvChar, // <- ptCharReg
 mvInt,
 mvFloat,
 mvString,
 mvReference,

 mvBool, // <- ptBool
 mvChar, // <- ptChar
 mvInt,
 mvFloat,
 mvString,

 mvInt, mvInt
);

  // ReadReg
  Procedure ReadReg; inline;
  Begin
   Result.isReg    := True;
   Result.RegIndex := read_uint8;
  End;

Var Typ: TOpcodeArgType;
Begin
 Result.Reset;

 // read argument type
 Typ := TOpcodeArgType(read_uint8);

 // fetch type
 Result.Typ := TypeMap[Typ];

 // fill appropriate value field
 Case Typ of
  { bool reg }
  ptBoolReg:
  Begin
   ReadReg;

   Result.MemAddr    := @PVM(VMPnt)^.Regs.b[Result.RegIndex];
   Result.Value.Bool := PVMBool(Result.MemAddr)^;
  End;

  { char reg }
  ptCharReg:
  Begin
   ReadReg;

   Result.MemAddr     := @PVM(VMPnt)^.Regs.c[Result.RegIndex];
   Result.Value.Char  := PVMChar(Result.MemAddr)^;
  End;

  { int reg }
  ptIntReg:
  Begin
   ReadReg;

   Result.MemAddr   := @PVM(VMPnt)^.Regs.i[Result.RegIndex];
   Result.Value.Int := PVMInt(Result.MemAddr)^;
  End;

  { float reg }
  ptFloatReg:
  Begin
   ReadReg;

   Result.MemAddr     := @PVM(VMPnt)^.Regs.f[Result.RegIndex];
   Result.Value.Float := PVMFloat(Result.MemAddr)^;
  End;

  { string reg }
  ptStringReg:
  Begin
   ReadReg;

   Result.MemAddr   := @PVM(VMPnt)^.Regs.s[Result.RegIndex];
   Result.Value.Str := PPVMString(Result.MemAddr)^;

   if (CloneIfString) Then
    Result.Value.Str := PVM(VMPnt)^.VMStringList.CloneVMString(Result.Value.Str) Else
  End;

  { reference reg }
  ptReferenceReg:
  Begin
   ReadReg;

   Result.MemAddr   := @PVM(VMPnt)^.Regs.r[Result.RegIndex];
   Result.Value.Int := VMInt(PVMReference(Result.MemAddr)^);
  End;

  { constant value }
  ptBool  : Result.Value.Bool  := Boolean(read_uint8);
  ptChar  : Result.Value.Char  := chr(read_uint8);
  ptInt   : Result.Value.Int   := read_int64;
  ptFloat : Result.Value.Float := read_float;
  ptString: Result.Value.Str   := PVM(VMPnt)^.VMStringList.StringToVMString(read_string);

  { constant memory reference }
  ptConstantMemRef:
  Begin
   Result.isMemRef := True;
   Result.MemAddr  := BytecodeRelativeToAbsolute(Pointer(read_int64));
  End;

  { stackval }
  ptStackval:
  Begin
   Result.isStackval := True;

   Result.Stackval := PVM(VMPnt)^.Stack.getPointer(getStackPos+read_int32-1);
   Result.Typ      := Result.Stackval^.Typ;
   Result.Value    := Result.Stackval^.Value;
  End;
 End;
End;

(* TVMBytecode.Destroy *)
Destructor TVMBytecode.Destroy;
Begin
 Dispose(Data);

 inherited Destroy;
End;

(* TVMBytecode.Execute *)
{
 Executes the bytecode.
}
Procedure TVMBytecode.Execute;
Var Stop: PBoolean;
Label loop;
Begin
 Stop := @PVM(VMPnt)^.Stop;

 While (true) Do
 Begin
  if (Stop^) Then
   Exit;

  CurrentOpcode := Position;
  OpcodeTable[TOpcodeKind(read_uint8)](VMPnt);

  if (Stop^) Then
   Exit;

  CurrentOpcode := Position;
  OpcodeTable[TOpcodeKind(read_uint8)](VMPnt);
 End;
End;

(* TVMBytecode.setPosition *)
Procedure TVMBytecode.setPosition(const Value: PByte);
Begin
 Position := Value;
End;

(* TVMBytecode.setRelativePosition *)
{
 Sets instruction pointer relative to the first byte of bytecode data.
}
Procedure TVMBytecode.setRelativePosition(const Value: VMIReference);
Begin
 Position := VMReference(VMIReference(Data) + Value);
End;

(* TVMBytecode.getRelativePosition *)
{
 Returns instruction pointer relative to the first byte of bytecode data.
}
Function TVMBytecode.getRelativePosition: VMIReference;
Begin
 Result := VMIReference(Position) - VMIReference(Data);
End;

(* TVMBytecode.getLineData *)
Function TVMBytecode.getLineData(const Address: uint32; out FileName, FunctionName: String; out FunctionLine: uint32): Boolean;
Var DbgData: TDebugData;
    Current: uint32;
    I      : uint32;
Begin
 FileName     := '';
 FunctionName := '';
 FunctionLine := 0;

 Current := uint32(Address) - uint32(Data);

 // get debug data
 DbgData := PVM(VMPnt)^.LoaderData.Debug;

 // if no debug data available, give up
 if (DbgData.LineDataCount = 0) Then
  Exit(False);

 // find the nearest opcode
 With DbgData do
 Begin
  For I := 0 To LineDataCount-1 Do
  Begin
   if (LineDataList[I].Opcode > Current) Then
    Exit(Length(FileName) > 0);

   FileName     := FileList[LineDataList[I].FileID].FileName;
   FunctionName := FunctionList[LineDataList[I].FunctionID].FunctionName;
   FunctionLine := LineDataList[I].Line;

   if (LineDataList[I].Opcode = Current) Then
    Exit(True);
  End;
 End;

 Result := (Length(FileName) > 0);
End;
End.
