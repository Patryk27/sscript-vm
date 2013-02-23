(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Objects;

 Interface
 Uses SysUtils;

 Const MagicNumber = $CAFEBABE;

 Type TObjectType = (otArray);

 Type TLongWordArray = Array of LongWord;

 { TMObject }
 Type TMObject = Class
                  Private
                   Magic: LongWord;

                  Public
                   Property getMagic: LongWord read Magic;

                  Public
                   ObjType: TObjectType;

                   Constructor Create(const fObjType: TObjectType);

                   Function getAddress: LongWord;
                  End;

 { TMArray }
 Type TMArray = Class (TMObject)
                 Private
                  Data    : Pointer;
                  Typ     : Byte;
                  TypeSize: Byte;
                  Sizes   : TLongWordArray;

                  Function getElementMemory(Position: TLongWordArray): LongWord;

                 Public
                  Constructor Create(const fType: Byte; fSizes: TLongWordArray);
                  Destructor Destroy; override;

                  Procedure setValue(const Position: TLongWordArray; OpParam: Pointer);
                  Function getValue(const Position: TLongWordArray): Pointer;
                  Function getSize(const Position: TLongWordArray): LongWord;
                 End;

 Implementation
Uses Machine, Opcodes;

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
 FreeString(MemPos);

 Mem := LongWord(AllocMem(Length(Str)+sizeof(Integer)));

 PLongword(MemPos)^ := Mem;

 PLongword(Mem)^ := Length(Str);
 Inc(Mem, sizeof(LongWord));
 For Ch in Str Do
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
 Mem    := PLongword(MemPos)^;

 if (Mem = 0) Then // no string assigned
  Exit;

 Len := PLongword(Mem)^;

 Inc(Mem, sizeof(LongWord));

 For I := 1 To Len Do
 Begin
  Result += PChar(Mem)^;
  Inc(Mem, sizeof(Byte));
 End;
End;

(* ========== TMObject ========== *)

{ TMObject.Create }
Constructor TMObject.Create(const fObjType: TObjectType);
Begin
 Magic := MagicNumber;

 ObjType := fObjType;
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
   raise Exception.Create('Invalid array access.');
 End;

 For I := Low(Position) To High(Position) Do
  if (Position[I] >= Sizes[I]) Then
   raise Exception.Create('Array out of bounds; tried to access element #'+IntToStr(Position[I])+', while #'+IntToStr(Sizes[I]-1)+' is the last one.');

 Result := Position[Low(Position)]*TypeSize;

 For I := Low(Position)+1 To High(Position) Do
   Result += Position[I]*Sizes[I]*TypeSize;

 Result += LongWord(Data);
End;

{ TMArray.Create }
Constructor TMArray.Create(const fType: Byte; fSizes: TLongWordArray);
Var I, Size: LongWord;
Begin
 inherited Create(otArray);

 { set variables }
 Typ      := fType;
 TypeSize := Machine.TypeSize[fType];
 Sizes    := fSizes;

 { allocate memory }
 Size := 0;
 For I := Low(fSizes) To High(fSizes) Do
  Size += TypeSize*fSizes[I];

 Data := GetMem(Size);
 For I := 0 To Size-1 Do
  PByte(Data+I)^ := 0;
End;

{ TMArray.Destroy }
Destructor TMArray.Destroy;
Var Mem, MemEnd: LongWord;
Begin
 if (Typ = TYPE_STRING) Then
 Begin
  Mem    := LongWord(Data);
  MemEnd := Mem+MemSize(Data)-sizeof(LongWord);

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
  if (Length(Position)-1 = Length(Sizes)) and (self.Typ = TYPE_STRING) Then
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
    raise Exception.Create('Invalid type: '+IntToStr(ord(Typ)));
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
    raise Exception.Create('Invalid self-type: '+IntToStr(ord(self.Typ))); // shouldn't happen
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
Function TMArray.getSize(const Position: TLongWordArray): LongWord;
Var I: Integer;
Begin
 if (Length(Position) >{=} Length(Sizes)) Then
  raise Exception.Create('Invalid array access.');

 For I := Low(Position) To High(Position) Do
  if (Position[I] >= Sizes[I]) Then
   raise Exception.Create('Array out of bounds; tried to access element #'+IntToStr(Position[I])+', while #'+IntToStr(Sizes[I]-1)+' is the last one.');

 Result := Sizes[Length(Position)];
End;
End.
