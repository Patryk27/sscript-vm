(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMStack;

 Interface
 Uses VMElement, VMTypes;

 Const ExceptionStackSize = 2*1024; // 2 KB

 { TMixedValueType }
 Type TMixedValueType = (mvNone=-1, mvBool, mvChar, mvInt, mvFloat, mvString, mvReference, mvCallstackRef);

 Const MixedValueTypeNames: Array[TMixedValueType] of String = ('none', 'bool', 'char', 'int', 'float', 'string', 'reference', 'callstack reference');

 { TMixedValue }
 Type PMixedValue = ^TMixedValue;
      TMixedValue =
      Packed Record
       // public fields
       Typ: TMixedValueType;

       Value:
       Record
        Bool : VMBool;
        Char : VMChar;
        Int  : VMInt;
        Float: VMFloat;
        Str  : PVMString;
       End;

       isStackval: Boolean;
       Stackval  : PMixedValue;

       // VM-only fields
       isReg   : Boolean;
       isMemRef: Boolean;
       RegIndex: uint8;
       MemAddr : VMReference;

       // VM-only functions
       Class Function Create(const NewValue: Int64): TMixedValue; static;
       Class Function Create(const NewValue: Extended): TMixedValue; static;

       Function isLValue: Boolean;
       Procedure Reset;
      End;

 { TStackElement }
 Type PStackElement = ^TStackElement;
      TStackElement = TMixedValue; // a synonym-type

 { TStackArray }
 Type TStackArray = Array of TStackElement;

 { TVMStack }
 Type TVMStack =
      Class (TVMElement)
       Private
        Data    : TStackArray;
        Position: PVMInt;
        Size    : uint32;

       Public
        Constructor Create(const fVM: Pointer);
        Destructor Destroy; override;

        Procedure Clear;

        Procedure Push(const Element: TStackElement);
        Function Pop: TStackElement;
        Function Get(const Index: uint32): TStackElement;
        Function getPointer(const Index: uint32): PStackElement;

        Property getElement[const Index: uint32]: TStackElement read Get; default;
       End;

 // ---------- //

 Operator = (const P1, P2: TMixedValue): Boolean;
 Operator <> (const P1, P2: TMixedValue): Boolean;
 Operator > (const P1, P2: TMixedValue): Boolean;
 Operator >= (const P1, P2: TMixedValue): Boolean;
 Operator < (const P1, P2: TMixedValue): Boolean;
 Operator <= (const P1, P2: TMixedValue): Boolean;

 // ---------- //

 Function getTypeName(const MV: TMixedValue): String;

 Implementation
Uses VMStruct;

{$I mixedvalue_operators.inc}

(* getTypeName *)
Function getTypeName(const MV: TMixedValue): String;
Begin
 Result := MixedValueTypeNames[MV.Typ];

 if (MV.isReg) Then
  Result += ' reg';
End;

// -------------------------------------------------------------------------- //
(* TVMStack.Create *)
Constructor TVMStack.Create(const fVM: Pointer);
Begin
 inherited Create(fVM);

 SetLength(Data, 0);

 Size     := 0;
 Position := @PVM(fVM)^.Regs.i[5];
End;

(* TVMStack.Destroy *)
Destructor TVMStack.Destroy;
Begin
 SetLength(Data, 0);

 inherited Destroy;
End;

(* TVMStack.Clear *)
Procedure TVMStack.Clear;
Begin
 SetLength(Data, 0);

 Size      := 0;
 Position^ := 0;
End;

(* TVMStack.Push *)
{
 Pushes a new element onto the stack.
}
Procedure TVMStack.Push(const Element: TStackElement);
Begin
 if (Size >= Position^) Then
 Begin
  Inc(Size, 100);
  SetLength(Data, Size);
 End;

 Data[Position^] := Element;
 Inc(Position^);
End;

(* TVMStack.Pop *)
{
 Pops an element from the stack.
}
Function TVMStack.Pop: TStackElement;
Begin
 if (Position^ = 0) Then
  ThrowException('Cannot pop any element from the stack - it''s empty');

 Dec(Position^);
 Result := Data[Position^];

 if (Size - Position^ >= 100) Then
 Begin
  Dec(Size, 100);
  SetLength(Data, Size);
 End;
End;

(* TVMStack.Get *)
{
 Returns stack element with given ID.
}
Function TVMStack.Get(const Index: uint32): TStackElement;
Begin
 if (Index >= Position^) Then
  ThrowException('Cannot do stack fetch - index out of bounds (%d/%d)', [Index, Position^]);

 Result := Data[Index];
End;

(* TVMStack.getPointer *)
{
 Returns pointer to stack element with given ID.
}
Function TVMStack.getPointer(const Index: uint32): PStackElement;
Begin
 if (Index >= Position^) Then
  ThrowException('Cannot do stack fetch - index out of bounds (%d/%d)', [Index, Position^]);

 Result := @Data[Index];
End;

// -------------------------------------------------------------------------- //
(* TMixedValue.Create *)
Class Function TMixedValue.Create(const NewValue: Int64): TMixedValue;
Begin
 Result.Reset;
 Result.Typ       := mvInt;
 Result.Value.Int := NewValue;
End;

(* TMixedValue.Create *)
Class Function TMixedValue.Create(const NewValue: Extended): TMixedValue;
Begin
 Result.Reset;
 Result.Typ         := mvFloat;
 Result.Value.Float := NewValue;
End;

(* TMixedValue.isLValue *)
{
 Returns 'true' if this MixedValue is one of those: register, stackval, memory reference
}
Function TMixedValue.isLValue: Boolean;
Begin
 Result := (isReg or isStackval or isMemRef);
End;

(* TMixedValue.Reset *)
{
 Clears all structure fields.
}
Procedure TMixedValue.Reset;
Begin
 Typ        := mvNone;
 isStackval := False;
 isReg      := False;
 isMemRef   := False;
 RegIndex   := 0;
 MemAddr    := nil;
End;
End.
