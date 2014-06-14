(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMObjects;

 Interface
 Uses SysUtils, VMStack, VMStruct;

 { TIndex }
 Type TIndex = uint32;

 { TIndexArray }
 Type PIndexArray = ^TIndexArray;
      TIndexArray = Array of TIndex;

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
        Data: Pointer;

        TypeID, TypeSize: uint8; // array internal type ID
        DimensionSize   : TIndex;
        MemSize         : uint32; // total memory size occupied by the array data

       Private
        Procedure __set(const DataPos: Pointer; const NewValue: TMixedValue; const aType: uint8);
        Function __get(const DataPos: Pointer; const aType: uint8): TMixedValue;

        Function getElement(const Index: TIndex): Pointer;
        Function getElement(Indexes: TIndexArray; out Typ: uint8): Pointer;

       Public
        Constructor Create(const fVM: Pointer; const fType: uint8; const fDimensionSize: uint32);
        Constructor Create(const fVM: Pointer; const fType: uint8; fDimensions: TIndexArray);
        Destructor Destroy; override;

        Procedure setValue(const Index: TIndex; const NewValue: TMixedValue);
        Procedure setValue(const Indexes: TIndexArray; const NewValue: TMixedValue);

        Function getValue(const Index: TIndex): TMixedValue;
        Function getValue(const Indexes: TIndexArray): TMixedValue;

        Function getSize: TIndex;

        Procedure Resize(const NewSize: TIndex);

        Procedure GCMark; override;
       End;

 Implementation
Uses GarbageCollector, VMTypes;

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

// -------------------------------------------------------------------------- //
(* TMArray.__set *)
Procedure TMArray.__set(const DataPos: Pointer; const NewValue: TMixedValue; const aType: uint8);
Begin
 Case aType of
  TYPE_BOOL_id  : PVMBool(DataPos)^  := VM^.getBool(NewValue);
  TYPE_CHAR_id  : PVMChar(DataPos)^  := VM^.getChar(NewValue);
  TYPE_INT_id   : PVMInt(DataPos)^   := VM^.getInt(NewValue);
  TYPE_FLOAT_id : PVMFloat(DataPos)^ := VM^.getFloat(NewValue);
  TYPE_STRING_id:
  Begin
   // if some other string was previously assigned there, free it and then assign the new one
   VM^.VMStringList.Dispose(PPointer(DataPos)^);

   // clone string
   PPointer(DataPos)^ := VM^.VMStringList.CloneVMString(VM^.getString(NewValue));

   // and unbind it - we don't want our string to be accidentally freed by the GC
   VM^.VMStringList.Unbind(PPointer(DataPos)^);
  End;

  $FF:
  Begin
   PPointer(DataPos)^ := VM^.getReference(NewValue);
  End;

  else
   VM^.ThrowException('Invalid array internal type: %d', [aType]);
 End;
End;

(* TMArray.__get *)
Function TMArray.__get(const DataPos: Pointer; const aType: uint8): TMixedValue;
Begin
 // make sure result is empty
 Result.Reset;

 With Result do
 Begin
  // set mixedvalue type
  Case aType of
   TYPE_BOOL_id  : Typ := mvBool;
   TYPE_CHAR_id  : Typ := mvChar;
   TYPE_INT_id   : Typ := mvInt;
   TYPE_FLOAT_id : Typ := mvFloat;
   TYPE_STRING_id: Typ := mvString;

   $FF: Typ := mvReference;

   else
    VM^.ThrowException('Invalid internal array type: %d', [aType]);
  End;

  // save value
  Case Typ of
   mvBool     : Value.Bool  := PVMBool(DataPos)^;
   mvChar     : Value.Char  := PVMChar(DataPos)^;
   mvInt      : Value.Int   := PVMInt(DataPos)^;
   mvFloat    : Value.Float := PVMFloat(DataPos)^;
   mvString   : Value.Str   := VM^.VMStringList.CloneVMString(PPointer(DataPos)^);
   mvReference: Value.Int   := VMInt(PPointer(DataPos)^);
  End;
 End;
End;

(* TMArray.getElement *)
Function TMArray.getElement(const Index: TIndex): Pointer;
Begin
 if (Index >= DimensionSize) Then
  VM^.ThrowException('Array index out of bounds: index %d is outside array range 0..%d', [Index, DimensionSize-1]);

 Result := Pointer(uint32(Data) + Index * TypeSize);
End;

(* TMArray.getElement *)
Function TMArray.getElement(Indexes: TIndexArray; out Typ: uint8): Pointer;
Var VMStr: PVMString;
    Arr  : TMArray;
    I    : uint8;
Begin
 Typ := TypeID;

 if (Length(Indexes) = 1) Then
  Exit(getElement(Indexes[0]));

 // multidimensional array
 if (TypeID = $FF) Then
 Begin
  // fetch pointer
  Arr := TMArray(PPointer(getElement(Indexes[0]))^);

  // check if pointer is valid
  if (not VM^.isValidObject(Arr)) Then
   VM^.ThrowException('Multidimensional array contains an invalid entry! (entry address = 0x%x)', [VMIReference(Arr)]);

  // truncate array by first element (i.e. create a new array without that element)
  For I := 0 To High(Indexes)-1 Do
   Indexes[I] := Indexes[I+1];

  SetLength(Indexes, High(Indexes));

  // use recursion, luke
  Result := Arr.getElement(Indexes, Typ);
 End Else

 // 1D string array
 if (TypeID = TYPE_STRING_id) Then
 Begin
  // stringarray[indexID][charID]
  Typ := TYPE_CHAR_id;

  // fetch string pointer
  VMStr := PPointer(getElement(Indexes[0]))^;

  if (VMStr = nil) Then
   Exit(nil);

  // get char
  if (Indexes[1] < 1) or (Indexes[1] > VMStr^.Length) Then
   VM^.ThrowException('String index out of bounds: index %d is outside string range 1..%d', [Indexes[1], VMStr^.Length]);

  Result := VMStr^.Data + Indexes[1] - 1;
 End Else

 // invalid argument
 Begin
  VM^.ThrowException('TMArray.getElement() invalid argument');
 End;
End;

(* TMArray.Create *)
Constructor TMArray.Create(const fVM: Pointer; const fType: uint8; const fDimensionSize: uint32);
Begin
 inherited Create(fVM);

 // set fields
 TypeID := fType;

 if (TypeID = $FF) Then
  TypeSize := sizeof(Pointer) Else
  TypeSize := TypeSizes[fType];

 DimensionSize := fDimensionSize;

 // allocate memory
 MemSize := TypeSize * DimensionSize;
 Data    := AllocMem(MemSize);
End;

(* TMArray.Create *)
Constructor TMArray.Create(const fVM: Pointer; const fType: uint8; fDimensions: TIndexArray);
Var I: uint32;
Begin
 if (Length(fDimensions) = 1) Then
 Begin
  Create(fVM, fType, fDimensions[0]);
  Exit;
 End;

 // create multidimensional array
 Create(fVM, $FF, fDimensions[High(fDimensions)]);

 SetLength(fDimensions, High(fDimensions));

 if (getSize = 0) Then
  Exit;

 For I := 0 To getSize-1 Do
 Begin
  PPointer(getElement(I))^ := TMArray.Create(fVM, fType, fDimensions);
 End;
End;

(* TMArray.Destroy *)
Destructor TMArray.Destroy;
Var Mem, MemEnd: PPointer;
Begin
 // strings needs special memory freeing (in fact - it doesn't have to be done here, but why not? Let's not waste memory.)
 if (TypeID = TYPE_STRING_id) Then
 Begin
  Mem    := Data;
  MemEnd := PPointer(VMIReference(Mem) + VMIReference(MemSize));

  While (Mem < MemEnd) Do
  Begin
   if (Mem^ <> nil) Then // if anything is assigned there...
    VM^.VMStringList.Dispose(Mem^); // these strings are not automatically freed so we must dispose them by ourselves

   Inc(Mem);
  End;
 End;

 // release pointer
 FreeMem(Data);

 inherited;
End;

(* TMArray.setValue *)
Procedure TMArray.setValue(const Index: TIndex; const NewValue: TMixedValue);
Begin
 __set(getElement(Index), NewValue, TypeID);
End;

(* TMArray.setValue *)
Procedure TMArray.setValue(const Indexes: TIndexArray; const NewValue: TMixedValue);
Var Typ: uint8;
    Pos: Pointer;
Begin
 Pos := getElement(Indexes, Typ);
 __set(Pos, NewValue, Typ);
End;

(* TMArray.getValue *)
Function TMArray.getValue(const Index: TIndex): TMixedValue;
Begin
 Result := __get(getElement(Index), TypeID);
End;

(* TMArray.getValue *)
Function TMArray.getValue(const Indexes: TIndexArray): TMixedValue;
Var Typ: uint8;
    Pos: Pointer;
Begin
 Pos    := getElement(Indexes, Typ);
 Result := __get(Pos, Typ);
End;

(* TMArray.getSize *)
Function TMArray.getSize: TIndex;
Begin
 Result := DimensionSize;
End;

(* TMArray.Resize *)
Procedure TMArray.Resize(const NewSize: TIndex);
Var Mem, MemEnd: Pointer;
    Pnt        : PPointer;

    OldSize: TIndex;
Begin
 // if new size is equal to current, nothing have to be done
 if (NewSize = DimensionSize) Then
  Exit;

 // if new size if lower than current and we have a string array, we need to clean string memory (as the array will shrink which could and would lead to memory leaks)
 if (NewSize < DimensionSize) and (TypeID = TYPE_STRING_id) Then
 Begin
  Mem    := Data + NewSize*TypeSize;
  MemEnd := Data + MemSize;

  While (Mem < MemEnd) Do
  Begin
   Pnt := PPointer(Mem);

   if (Pnt^ <> nil) Then
    VM^.VMStringList.Dispose(Pnt^);

   Inc(Mem);
  End;
 End;

 // save old size
 OldSize := DimensionSize;

 // compute new size
 MemSize := NewSize * TypeSize;

 // reallocate memory block
 ReallocMem(Data, MemSize);

 // zero 'new' clean memory block (leaving the old data untouched, ofc.)
 if (NewSize > DimensionSize) Then
 Begin
  Mem    := Data + OldSize*TypeSize;
  MemEnd := Data + MemSize;

  While (Mem < MemEnd) Do
  Begin
   Puint32(Mem)^ := 0;
   Inc(Mem);
  End;
 End;

 // update other data
 DimensionSize := NewSize;
End;

(* TMArray.GCMark *)
Procedure TMArray.GCMark;
Var Mem, MemEnd: Pointer;
    Obj        : TMObject;
Begin
 inherited;

 if (TypeID = $FF) Then
 Begin
  Mem    := Data;
  MemEnd := Mem+MemSize;

  While (Mem < MemEnd) Do
  Begin
   Obj := TMObject(PPointer(Mem)^);

   if (VM^.isValidObject(Obj)) Then
    Obj.GCMark;

   Inc(Mem, sizeof(Pointer));
  End;
 End;
End;
End.
