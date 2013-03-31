(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Objects;

 Interface
 Uses SysUtils;

 Type TLongWordArray = Array of LongWord;

 { TMObject }
 Type TMObject = Class
                  Private
                   SomeField: Integer;

                  Public
                   Constructor Create;

                   Procedure Test;
                   Function getAddress: LongWord;
                  End;

 { TMArray }
 Type TMArray = Class (TMObject)
                 Private
                  Data    : Pointer; // array data
                  Typ     : Byte; // array internal type ID
                  TypeSize: Byte; // internal type size
                  MemSize : LongWord; // total memory size used by `array data`
                  Sizes   : TLongWordArray; // array dimensions

                  Function getElementMemory(Position: TLongWordArray): LongWord;

                 Public
                  Constructor Create(const fType: Byte; fSizes: TLongWordArray);
                  Destructor Destroy; override;

                  Procedure setValue(const Position: TLongWordArray; OpParam: Pointer); // set element's value
                  Function getValue(const Position: TLongWordArray): Pointer; // get element's value
                  Function getSize(const Dimension: Byte): LongWord; // get dimension's size
                 End;

 Implementation
Uses Machine, Exceptions, Opcodes;

{ FreeString }
Procedure FreeString(const MemPos: LongWord);
Var Mem: LongWord;
Begin
 Mem := PLongword(MemPos)^;

 if (Mem <> 0) Then
  FreeMem(Pointer(Mem));

 PLongword(MemPos)^ := 0;
End;

{ SaveString }
Procedure SaveString(const MemPos: LongWord; Str: String);
Var Mem: LongWord;
    Ch : Char;
Begin
 FreeString(MemPos); // free previous string (if possible)

 Mem := LongWord(AllocMem(Length(Str)+sizeof(LongWord))); // allocate memory

 PLongword(MemPos)^ := Mem; // at `MemPos` save pointer to allocated memory (there our finally string will be hold)

 PLongword(Mem)^ := Length(Str); // save string length
 Inc(Mem, sizeof(LongWord)); // move pointer (so we don't overwrite length)
 For Ch in Str Do // save each char
 Begin
  PChar(Mem)^ := Ch;
  Inc(Mem, sizeof(Byte));
 End;
End;

{ LoadString }
Function LoadString(const MemPos: LongWord): String;
Var Mem   : LongWord;
    I, Len: LongWord;
Begin
 Result := '';
 Mem    := PLongword(MemPos)^; // get string pointer

 if (Mem = 0) Then // no string associated
  Exit;

 Len := PLongword(Mem)^; // get string length
 Inc(Mem, sizeof(LongWord)); // skip length (thus we just read it)

 For I := 1 To Len Do // read chars
 Begin
  Result += PChar(Mem)^;
  Inc(Mem, sizeof(Byte));
 End;
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
Function TMObject.getAddress: LongWord;
Begin
 Result := LongWord(self);
End;

(* ========== TMArray =========== *)

{ TMArray.getElementMemory }
Function TMArray.getElementMemory(Position: TLongWordArray): LongWord;
Var I: Integer;
Begin
 if (Length(Position) <> Length(Sizes)) Then
 Begin
  if (Typ = TYPE_STRING) and (Length(Position)-1 = Length(Sizes)) Then
   SetLength(Position, High(Position)) Else
   raise eInvalidAccess.Create('Invalid array access.');
 End;

 For I := Low(Position) To High(Position) Do
  if (Position[I] >= Sizes[I]) Then
   raise eOutOfBounds.Create('Array out of bounds. Tried to access element #'+IntToStr(Position[I])+', while #'+IntToStr(Sizes[I]-1)+' is the last one.');

 Result := Position[Low(Position)]*TypeSize;

 For I := Low(Position)+1 To High(Position) Do
   Result += Position[I]*Sizes[I]*TypeSize;

 Result += LongWord(Data);
End;

{ TMArray.Create }
Constructor TMArray.Create(const fType: Byte; fSizes: TLongWordArray);
Var I: LongWord;
Begin
 { set variables }
 Typ      := fType;
 TypeSize := Machine.TypeSize[fType];
 Sizes    := fSizes;

 { allocate memory }
 MemSize := 0;
 For I := Low(fSizes) To High(fSizes) Do
  MemSize += TypeSize*fSizes[I];

 if (MemSize = 0) Then
  MemSize := 1;

 Data := GetMem(MemSize);
 For I := 0 To MemSize-1 Do
  PByte(Data+I)^ := 0;
End;

{ TMArray.Destroy }
Destructor TMArray.Destroy;
Var Mem, MemEnd: LongWord;
Begin
 if (Typ = TYPE_STRING) Then // strings need a special destroying
 Begin
  Mem    := LongWord(Data);
  MemEnd := Mem+MemSize;

  While (Mem < MemEnd) Do
  Begin
   FreeString(Mem);
   Inc(Mem, sizeof(LongWord));
  End;
 End;

 FreeMem(Data);
End;

{ TMArray.setValue }
Procedure TMArray.setValue(const Position: TLongWordArray; OpParam: Pointer);
Var DataPos: LongWord;
    Str    : String;
Begin
 DataPos := getElementMemory(Position);

 With POpParam(OpParam)^ do
 Begin
  if (Length(Position)-1 = Length(Sizes)) and (self.Typ = TYPE_STRING) Then // special 'feature': immediate string's char reading
  Begin
   Str                           := LoadString(DataPos);
   Str[Position[High(Position)]] := getChar;
   sVal := Str;
  End;

  Case self.Typ of
   TYPE_BOOL, TYPE_CHAR, TYPE_INT: PLongWord(DataPos)^ := Val;
   TYPE_STRING                   : SaveString(DataPos, sVal);
   TYPE_FLOAT                    : PExtended(DataPos)^ := fVal;
   else
    raise eInvalidAccess.Create('Invalid type: '+IntToStr(ord(Typ)));
  End;
 End;
End;

{ TMArray.getValue }
Function TMArray.getValue(const Position: TLongWordArray): Pointer;
Var OpParam: POpParam;
    DataPos: LongWord;
Begin
 DataPos := getElementMemory(Position);

 New(OpParam);

 With POpParam(OpParam)^ do
 Begin
  Case self.Typ of
   TYPE_BOOL  : Typ := ptBool;
   TYPE_CHAR  : Typ := ptChar;
   TYPE_INT   : Typ := ptInt;
   TYPE_FLOAT : Typ := ptFloat;
   TYPE_STRING: Typ := ptString;
   else
    raise eInvalidAccess.Create('Invalid self-type: '+IntToStr(ord(self.Typ))); // shouldn't happen
  End;

  Case Typ of
   ptBool, ptChar, ptInt: Val := PLongWord(DataPos)^;
   ptString             : sVal := LoadString(DataPos);
   ptFloat              : fVal := PExtended(DataPos)^;
  End;

  if (Length(Position)-1 = Length(Sizes)) and (self.Typ = TYPE_STRING) Then
  Begin
   sVal := sVal[Position[High(Position)]];
   Val  := ord(sVal[1]);
  End;
 End;

 Result := OpParam;
End;

{ TMArray.getSize }
Function TMArray.getSize(const Dimension: Byte): LongWord;
Begin
 if (Dimension = 0) or (Dimension > Length(Sizes)) Then
  raise eOutOfBounds.Create('Array of of bounds. Tried to access dimension #'+IntToStr(Dimension)+', while #'+IntToStr(Length(Sizes))+' is the last one.');

 Result := Sizes[Dimension-1]; // dimensions are from user-side numbered from `1`, but internally from `0`.
End;
End.
