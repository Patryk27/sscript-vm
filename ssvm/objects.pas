(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Objects;

 Interface
 Uses SysUtils, Stack, VM;

 Type uint32Array = Array of uint32;

 { TMObject }
 Type TMObject = Class
                  Private
                   SomeField: Integer;

                  Public
                   Constructor Create;

                   Procedure Test;
                   Function getAddress: Pointer;
                  End;

 { TMArray }
 Type TMArray = Class (TMObject)
                 Private
                  VM       : PVM;
                  Data     : Pointer; // array data
                  Typ      : Byte; // array internal type ID
                  CTypeSize: Byte; // internal type size
                  MemSize  : uint32; // total memory size used by `array data`
                  Sizes    : uint32Array; // array dimensions

                  Function getElementMemory(Position: uint32Array): Pointer;

                 Public
                  Constructor Create(const fVM: Pointer; const fType: Byte; fSizes: uint32Array);
                  Destructor Destroy; override;

                  Procedure setValue(const Position: uint32Array; NewValue: TMixedValue); // set element's value
                  Function getValue(const Position: uint32Array): TMixedValue; // get element's value
                  Function getSize(const Dimension: Byte): uint32; // get dimension's size
                 End;

 Implementation

{ FreeString }
Procedure FreeString(const MemPos: Pointer); inline;
Var Mem: Pointer;
Begin
 Mem := Pointer(Puint32(MemPos)^);

 if (Mem <> nil) Then
  FreeMem(Mem);

 Puint32(MemPos)^ := 0;
End;

{ SaveString }
Procedure SaveString(const MemPos: Pointer; Str: String); inline;
Var Mem: Pointer;
    I  : uint32;
Begin
 FreeString(MemPos); // free string (if possible)

 Mem := Pointer(AllocMem(sizeof(uint32)+Length(Str))); // allocate memory

 Puint32(MemPos)^ := uint32(Mem); // at array element, save pointer to allocated memory (there our final string will be hold)
 Puint32(Mem)^    := Length(Str); // save string length

 Inc(Mem, sizeof(uint32));
 For I := 1 To Length(Str) Do
  PChar(Mem+I-1)^ := Str[I];
End;

{ LoadString }
Function LoadString(const MemPos: Pointer): PChar; inline;
Var Mem, Len: uint32;
Begin
 Result := '';

 Mem := Puint32(MemPos)^; // get string's content pointer
 if (Mem = 0) Then // no string associated
  Exit;

 Len := Puint32(Mem)^; // get string's length
 if (Len = 0) Then // no data to be read
  Exit;

 Inc(Mem, sizeof(uint32));
 Result := PChar(Mem);
// For I := 0 To Len-1 Do
// Result += PChar(Mem+I)^;
End;

(* ========== TMObject ========== *)

{ TMObject.Create }
Constructor TMObject.Create;
Begin
End;

{ TMObject.Test }
Procedure TMObject.Test;
Begin
 SomeField := $CAFEBABE;
End;

{ TMObject.getAddress }
Function TMObject.getAddress: Pointer;
Begin
 Result := self;
End;

(* ========== TMArray =========== *)

{ TMArray.getElementMemory }
Function TMArray.getElementMemory(Position: uint32Array): Pointer;
Var I: Integer;
Begin
 if (Length(Position) <> Length(Sizes)) Then
 Begin
  if (Typ = TYPE_STRING) and (Length(Position)-1 = Length(Sizes)) Then
   SetLength(Position, High(Position)) Else
   VM^.ThrowExceptionByMessage('Invalid array access.');
 End;

 For I := Low(Position) To High(Position) Do
  if (Position[I] >= Sizes[I]) Then
   VM^.ThrowExceptionByMessage(PChar('Array out of bounds. Tried to access element #'+IntToStr(Position[I])+', while #'+IntToStr(Sizes[I]-1)+' is the last one.'));

 Result := Pointer(Position[Low(Position)]*CTypeSize);

 For I := Low(Position)+1 To High(Position) Do
   Result += Position[I]*Sizes[I]*CTypeSize;

 Result += uint32(Data);
End;

{ TMArray.Create }
Constructor TMArray.Create(const fVM: Pointer; const fType: Byte; fSizes: uint32Array);
Var I: uint32;
Begin
 { set class fields }
 VM        := fVM;
 Typ       := fType;
 CTypeSize := TypeSize[fType];
 Sizes     := fSizes;

 { allocate memory }
 MemSize := 0;
 For I := Low(fSizes) To High(fSizes) Do
  MemSize += CTypeSize*fSizes[I];

 if (MemSize = 0) Then
  MemSize := 1;

 Data := GetMem(MemSize);
 For I := 0 To MemSize-1 Do
  PByte(Data+I)^ := 0;
End;

{ TMArray.Destroy }
Destructor TMArray.Destroy;
Var Mem, MemEnd: Pointer;
Begin
 if (Typ = TYPE_STRING) Then // strings need a special destroying
 Begin
  Mem    := Data;
  MemEnd := Mem+MemSize;

  While (Mem < MemEnd) Do
  Begin
   FreeString(Mem);
   Inc(Mem, sizeof(uint32));
  End;
 End;

 FreeMem(Data);
End;

{ TMArray.setValue }
Procedure TMArray.setValue(const Position: uint32Array; NewValue: TMixedValue);
Var DataPos: Pointer;
    Str    : String;
Begin
 DataPos := getElementMemory(Position);

 With NewValue do
 Begin
  if (Length(Position)-1 = Length(Sizes)) and (self.Typ = TYPE_STRING) Then // special 'feature': immediate string's char reading
  Begin
   Str                           := LoadString(DataPos);
   Str[Position[High(Position)]] := getChar(NewValue);
   Value.Str                     := PChar(Str);
  End;

  Case self.Typ of
   TYPE_BOOL, TYPE_CHAR, TYPE_INT: Puint32(DataPos)^ := Value.Int;
   TYPE_STRING                   : SaveString(DataPos, getString(NewValue));
   TYPE_FLOAT                    : PExtended(DataPos)^ := getFloat(NewValue);

   else
    raise Exception.CreateFmt('Invalid internal array type: %d', [ord(Typ)]);
  End;
 End;
End;

{ TMArray.getValue }
Function TMArray.getValue(const Position: uint32Array): TMixedValue;
Var DataPos: Pointer;
Begin
 DataPos := getElementMemory(Position);

 With Result do
 Begin
  Case self.Typ of
   TYPE_BOOL  : Typ := mvBool;
   TYPE_CHAR  : Typ := mvChar;
   TYPE_INT   : Typ := mvInt;
   TYPE_FLOAT : Typ := mvFloat;
   TYPE_STRING: Typ := mvString;

   else
    raise Exception.CreateFmt('Invalid internal array type: %d', [ord(self.Typ)]); // shouldn't happen
  End;

  Case Typ of
   mvBool, mvChar, mvInt: Value.Int   := Puint32(DataPos)^;
   mvString             : Value.Str   := LoadString(DataPos);
   mvFloat              : Value.Float := PExtended(DataPos)^;
  End;

  if (Length(Position)-1 = Length(Sizes)) and (self.Typ = TYPE_STRING) Then
  Begin
   Value.Str := PChar(AnsiString(Value.Str[Position[High(Position)]]));
   Value.Int := ord(Value.Str[1]);
  End;
 End;
End;

{ TMArray.getSize }
Function TMArray.getSize(const Dimension: Byte): uint32;
Begin
 if (Dimension = 0) or (Dimension > Length(Sizes)) Then
  VM^.ThrowExceptionByMessage(PChar('Array of of bounds. Tried to access dimension #'+IntToStr(Dimension)+', while #'+IntToStr(Length(Sizes))+' is the last one.'));

 Result := Sizes[Dimension-1]; // dimensions from user-side are numbered starting by `1`, but internally from `0`.
End;
End.