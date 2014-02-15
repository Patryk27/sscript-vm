(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
// @TODO: rewrite this whole unit...
{$H+}
Unit VMObjects;

 Interface
 Uses SysUtils, VMStack, VMStruct;

 Type PIndexArray = ^TIndexArray;
      TIndexArray = Array of uint32;

 { TMObject }
 Type TMObject =
      Class
       Private
        VM: PVM;

       Public
        isMarked: Boolean; // used in garbage collector

       Public
        Constructor Create(const fVM: PVM);

        Procedure GCMark; virtual;
       End;

 { TMArray }
 Type TMArray =
      Class (TMObject)
       Private
        Data     : Pointer; // array data
        Typ      : Byte; // array internal type ID
        CTypeSize: Byte; // internal type size
        MemSize  : uint32; // total memory size used by `array data`
        Sizes    : TIndexArray; // array dimension sizes

       Private
        Function getElement(Position: TIndexArray): Pointer;

       Public
        Constructor Create(const fVM: Pointer; const fType: Byte; const fSizes: TIndexArray);
        Destructor Destroy; override;

        Procedure setValue(const Position: TIndexArray; NewValue: TMixedValue); // set element's value
        Function getValue(const Position: TIndexArray): TMixedValue; // get element's value
        Function getSize(const Dimension: Byte): uint32; // get dimension's size

        Procedure GCMark; override;
       End;

 Implementation
Uses GarbageCollector, VMTypes;

(* ========== TMObject ========== *)

(* TMObject.Create *)
Constructor TMObject.Create(const fVM: PVM);
Begin
 VM := fVM;
 TGarbageCollector(VM^.GarbageCollector).PutObject(self);
End;

(* TMObject.GCMark *)
Procedure TMObject.GCMark;
Begin
 isMarked := True;
End;

(* ========== TMArray =========== *)

(* TMArray.getElementMemory *)
Function TMArray.getElement(Position: TIndexArray): Pointer;
Var I: Integer;
Begin
 if (Length(Position) <> Length(Sizes)) Then
 Begin
  if (Typ = TYPE_STRING_id) and (Length(Position)-1 = Length(Sizes)) Then
   SetLength(Position, High(Position)) Else
   VM^.ThrowException('Invalid array access.');
 End;

 For I := Low(Position) To High(Position) Do
  if (Position[I] >= Sizes[I]) Then
   VM^.ThrowException('Array out of bounds. Tried to access element #%d, while #%d is the last one.', [Position[I], Sizes[I]-1]);

 Result := Pointer(Position[Low(Position)]*CTypeSize);

 For I := Low(Position)+1 To High(Position) Do
   Result += Position[I]*Sizes[I]*CTypeSize;

 Result += uint32(Data);
End;

(* TMArray.Create *)
Constructor TMArray.Create(const fVM: Pointer; const fType: Byte; const fSizes: TIndexArray);
Var I: uint32;
Begin
 inherited Create(fVM);

 { set class fields }
 Typ       := fType;
 CTypeSize := TypeSizes[fType];
 Sizes     := fSizes;

 { allocate memory }
 MemSize := 0;
 For I := Low(fSizes) To High(fSizes) Do
  MemSize += CTypeSize*fSizes[I];

 if (MemSize = 0) Then
  MemSize := 1;

 Data := AllocMem(MemSize);
End;

(* TMArray.Destroy *)
Destructor TMArray.Destroy;
Var Mem, MemEnd: PPointer;
Begin
 if (Typ = TYPE_STRING_id) Then // strings need special memory freeing
 Begin
  Mem    := Data;
  MemEnd := PPointer(VMIReference(Mem) + VMIReference(MemSize));

  While (Mem < MemEnd) Do
  Begin
   if (Mem^ <> nil) Then // if anything is assigned there...
    VM^.VMStringList.Dispose(Mem^); // these strings are not automatically freed, so we must dispose them by ourselves

   Inc(Mem);
  End;
 End;

 FreeMem(Data);
End;

(* TMArray.setValue *)
Procedure TMArray.setValue(const Position: TIndexArray; NewValue: TMixedValue);
Var DataPos: Pointer;
    FreeStr: Boolean = True;
Begin
 DataPos := getElement(Position);

 if (self.Typ = TYPE_STRING_id) and (Length(Position) = Length(Sizes)+1) Then // special feature: immediate string's char reference; like: ArrayOfString[ArrayIndex][CharIndex] := 'f';
 Begin
  With NewValue do
  Begin
   Value.Str                                 := DataPos;
   Value.Str^.Data[Position[High(Position)]] := VM^.getChar(NewValue);

   Typ := mvString;

   FreeStr := False;
  End;
 End;

 Case self.Typ of
  TYPE_BOOL_id  : PVMBool(DataPos)^  := VM^.getBool(NewValue);
  TYPE_CHAR_id  : PVMChar(DataPos)^  := VM^.getChar(NewValue);
  TYPE_INT_id   : PVMInt(DataPos)^   := VM^.getInt(NewValue);
  TYPE_FLOAT_id : PVMFloat(DataPos)^ := VM^.getFloat(NewValue);
  TYPE_STRING_id:
  Begin
   if (FreeStr) and (PPointer(DataPos)^ <> nil) Then // if some other string was previously assigned there, free it and then assign the new one
    VM^.VMStringList.Dispose(PPointer(DataPos)^);

   PPointer(DataPos)^ := VM^.VMStringList.CloneVMString(VM^.getString(NewValue));
   VM^.VMStringList.Unbind(PPointer(DataPos)^); // we don't want our string to be "accidentally" freed
  End;

  else
   VM^.ThrowException('Invalid array internal type (#%d)', [ord(Typ)]);
 End;
End;

(* TMArray.getValue *)
Function TMArray.getValue(const Position: TIndexArray): TMixedValue;
Var DataPos: Pointer;
Begin
 Result.Reset;

 DataPos := getElement(Position);

 With Result do
 Begin
  Case self.Typ of
   TYPE_BOOL_id  : Typ := mvBool;
   TYPE_CHAR_id  : Typ := mvChar;
   TYPE_INT_id   : Typ := mvInt;
   TYPE_FLOAT_id : Typ := mvFloat;
   TYPE_STRING_id: Typ := mvString;

   else
    VM^.ThrowException('Invalid internal array type (#%d)', [ord(self.Typ)]);
  End;

  Case Typ of
   mvBool  : Value.Bool  := PVMBool(DataPos)^;
   mvChar  : Value.Char  := PVMChar(DataPos)^;
   mvInt   : Value.Int   := PVMInt(DataPos)^;
   mvFloat : Value.Float := PVMFloat(DataPos)^;
   mvString: Value.Str   := VM^.VMStringList.CloneVMString(PPointer(DataPos)^);
  End;

  if (Length(Position) = Length(Sizes)+1) and (self.Typ = TYPE_STRING_id) Then
  Begin
   Typ := mvChar;

   Value.Char := Value.Str^.Data[Position[High(Position)]-1];
  End;
 End;
End;

(* TMArray.getSize *)
Function TMArray.getSize(const Dimension: Byte): uint32;
Begin
 if (Dimension = 0) or (Dimension > Length(Sizes)) Then
  VM^.ThrowException('Array of of bounds. Tried to access dimension #%d, while #%d is the last one.', [Dimension, Length(Sizes)]);

 Result := Sizes[Dimension-1]; // dimensions from the user-side are counted from `1`, but internally from `0`. This is the place where we do magic to "fix" it.
End;

(* TMArray.GCMark *)
Procedure TMArray.GCMark;
Var Mem, MemEnd: Pointer;
    Obj        : TMObject;
Begin
 inherited;

 if (Typ = TYPE_INT_id) Then
 Begin
  Mem    := Data;
  MemEnd := Mem+MemSize;

  While (Mem < MemEnd) Do
  Begin
   Obj := TMObject(Pointer(uint32(Pint64(Mem)^)));
   if (VM^.isValidObject(Obj)) Then
    Obj.GCMark;

   Inc(Mem, sizeof(int64));
  End;
 End;
End;
End.
