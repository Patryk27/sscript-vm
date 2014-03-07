(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
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

        Property getData: PByte read Data;
        Property getPosition: PByte read Position;
        Property getCurrentOpcode: PByte read CurrentOpcode;
       End;

 Implementation
Uses VMStruct, Interpreter;

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
Var Typ: TOpcodeArgType;
Begin
 Result.Reset;

 Typ := TOpcodeArgType(read_uint8);

 Result.isReg := (Typ in [ptBoolReg..ptReferenceReg]);

 if (Result.isReg) Then // if it's a register, read this register's ID
  Result.RegIndex := read_uint8;

 Case Typ of
  ptBoolReg, ptBool    : Result.Typ := mvBool;
  ptCharReg, ptChar    : Result.Typ := mvChar;
  ptIntReg, ptInt      : Result.Typ := mvInt;
  ptFloatReg, ptFloat  : Result.Typ := mvFloat;
  ptStringReg, ptString: Result.Typ := mvString;
  ptReferenceReg       : Result.Typ := mvReference;
  ptConstantMemRef     : Result.Typ := mvReference;

  else
   Result.Typ := mvInt;
 End;

 Case Typ of
  { register value }
  ptBoolReg     : Result.Value.Bool  := getBoolReg(Result.RegIndex);
  ptCharReg     : Result.Value.Char  := getCharReg(Result.RegIndex);
  ptIntReg      : Result.Value.Int   := getIntReg(Result.RegIndex);
  ptFloatReg    : Result.Value.Float := getFloatReg(Result.RegIndex);
  ptReferenceReg: Result.Value.Int   := VMInt(getReferenceReg(Result.RegIndex));

  ptStringReg:
  Begin
   if (CloneIfString) Then
    Result.Value.Str := PVM(VMPnt)^.VMStringList.CloneVMString(getStringReg(Result.RegIndex)) Else
    Result.Value.Str := getStringReg(Result.RegIndex);
  End;

  { constant value }
  ptBool          : Result.Value.Bool  := Boolean(read_uint8);
  ptChar          : Result.Value.Char  := chr(read_uint8);
  ptInt           : Result.Value.Int   := read_int64;
  ptFloat         : Result.Value.Float := read_float;
  ptString        : Result.Value.Str   := PVM(VMPnt)^.VMStringList.StringToVMString(read_string);
  ptConstantMemRef: Result.MemAddr     := Pointer(read_int64);

  else
   Result.Value.Int := read_int32;
 End;

 Result.isStackval := (Typ = ptStackval);
 Result.isMemRef   := (Typ = ptConstantMemRef);

 if (Result.isStackval) Then // if stackval
 Begin
  Result.Stackval := PVM(VMPnt)^.Stack.getPointer(getStackPos+Result.Value.Int-1);
  Result.Typ      := Result.Stackval^.Typ;
  Result.Value    := Result.Stackval^.Value;
 End;

 if (Result.isMemRef) Then // if memory reference
 Begin
  Result.MemAddr := BytecodeRelativeToAbsolute(Result.MemAddr); // make an absolute address from relative
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
Begin
 While (not PVM(VMPnt)^.Stop) Do
 Begin
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
End.
