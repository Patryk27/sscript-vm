(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITOpcodeList;

 Interface
 Uses VMTypes, JITOpcodes;

 { TJITOpcodeArray }
 Type TJITOpcodeArray = Array of TJITOpcode;

 { TJITOpcodeList }
 Type TJITOpcodeList =
      Class
       Private
        Data    : TJITOpcodeArray;
        Position: uint32;

       Public
        Constructor Create;

        Function Append(const Opcode: TJITOpcode): uint32;

        Procedure Dump;

       Public
        Function getOpcode(const ID: uint32): TJITOpcode;
        Procedure setOpcode(const ID: uint32; const Value: TJITOpcode);

        Function getSize: uint32;

       Public
        Property Opcode[const ID: uint32]: TJITOpcode read getOpcode write setOpcode; default;
        Property getData: TJITOpcodeArray read Data;
       End;

 Implementation
Uses SysUtils, TypInfo;

(* TJITOpcodeList.Create *)
{
 Creates a new class instance.
}
Constructor TJITOpcodeList.Create;
Begin
 SetLength(Data, 0);
 Position := 0;
End;

(* TJITOpcodeList.Append *)
{
 Adds next opcode to the end of the list.
}
Function TJITOpcodeList.Append(const Opcode: TJITOpcode): uint32;
Begin
 if (Position >= uint32(Length(Data))) Then
 Begin
  Try
   if (Length(Data) = 0) Then
    SetLength(Data, 50) Else
    SetLength(Data, Length(Data) + 50);
  Except
   raise Exception.Create('TJITOpcodeList.Append() -> list re-allocation failed!');
  End;
 End;

 Data[Position] := Opcode;
 Inc(Position);
End;

(* TJITOpcodeList.Dump *)
{
 Prints opcode list to the console.
}
Procedure TJITOpcodeList.Dump;
Var I, Q: uint32;
    Cnt : int8;
    Op  : TJITOpcode;
Begin
 Writeln('TJITOpcodeList.Dump()');
 Writeln;
 Writeln('Length(Data)   = ', Length(Data));
 Writeln('self.getSize() = ', self.getSize());
 Writeln;

 if (getSize = 0) Then
 Begin
  Writeln('Error: no opcodes to dump! Aborting...');
  Exit;
 End;

 Writeln('Opcode list:');
 Writeln;

 For I := 0 To getSize-1 Do
 Begin
  Op := Data[I];

  Write('[', I, '] ', Copy(GetEnumName(TypeInfo(Op.Kind), ord(Op.Kind)), 4, 50));

  Cnt := JITOpcodeParamCount[Op.KInd];

  if (Cnt > 0) Then
  Begin
   Write(' -> ');

   For Q := 0 To Cnt-1 Do
   Begin
    Case Op.Args[Q].Kind of
     joa_register: Write('reg: #', Op.Args[Q].RegisterID);
     joa_memory  : Write('mem: 0x', IntToHex(uint64(Op.Args[Q].MemoryAddr), sizeof(VMReference)*2));
     joa_constant: Write('const: `', Op.Args[Q].Constant, '`');
     joa_stackval: Write('stackval: ', Op.Args[Q].StackvalPos);
    End;

    if (Q < Cnt-1) Then
     Write(', ');
   End;
  End;

  Writeln;
 End;

 Writeln;
 Writeln('End of opcode list.');
 Writeln('End of dump.');
 Writeln;
End;

(* TJITOpcodeList.getOpcode *)
{
 Returns opcode with specified index.
}
Function TJITOpcodeList.getOpcode(const ID: uint32): TJITOpcode;
Begin
 if (ID >= Position) Then
  raise Exception.CreateFmt('TJITOpcodeList.getOpcode() -> array out of bounds! (ID=%d, size=%d)', [ID, getSize]);

 Result := Data[ID];
End;

(* TJITOpcodeList.setOpcode *)
{
 Changes opcode with specified index.
}
Procedure TJITOpcodeList.setOpcode(const ID: uint32; const Value: TJITOpcode);
Begin
 if (ID >= Position) Then
  raise Exception.CreateFmt('TJITOpcodeList.setOpcode() -> array out of bounds! (ID=%d, size=%d)', [ID, getSize]);

 Data[ID] := Value;
End;

(* TJITOpcodeList.getSize *)
{
 Returns size (index of the last element + 1) of the opcode list.
}
Function TJITOpcodeList.getSize: uint32;
Begin
 Result := Position;
End;
End.
