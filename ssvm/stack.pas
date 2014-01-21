(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit Stack;

 Interface

 { TMixedValueType }
 Type TMixedValueType = (mvNone=-1, mvBool, mvChar, mvInt, mvFloat, mvString, mvReference, mvCallstackRef);

 { TMixedValue }
 Type PStackElement = ^TStackElement;

      PMixedValue = ^TMixedValue;
      TMixedValue =
      Packed Record
       // public fields
       Typ  : TMixedValueType;
       Value: Record
               Bool : Boolean;
               Char : Char;
               Int  : Int64;
               Float: Extended;
               Str  : PChar; // @TODO: TVMString?
              End;

       isStackval: Boolean;
       Stackval  : PStackElement;

       // VM-only fields
       isReg   : Boolean;
       isMemRef: Boolean;
       RegIndex: uint8;
       MemAddr : uint32;

       // VM-only functions
       Class Function Create(const NewValue: Int64): TMixedValue; static;
       Class Function Create(const NewValue: Extended): TMixedValue; static;

       Function isLValue: Boolean;
       Procedure Reset;
       Procedure ReleaseData;
      End;

      TStackElement = TMixedValue;

 Const MixedValueTypeNames: Array[TMixedValueType] of String = ('none', 'bool', 'char', 'int', 'float', 'string', 'reference', 'callstack reference');

 Type PStack = ^TStack;
      TStack = Array of TStackElement;

 // ---------- //

 Operator = (P1, P2: TMixedValue): Boolean;
 Operator <> (P1, P2: TMixedValue): Boolean;
 Operator > (P1, P2: TMixedValue): Boolean;
 Operator >= (P1, P2: TMixedValue): Boolean;
 Operator < (P1, P2: TMixedValue): Boolean;
 Operator <= (P1, P2: TMixedValue): Boolean;

 // ---------- //

 Function getBool(MV: TMixedValue): Boolean; inline;
 Function getChar(MV: TMixedValue): Char; inline;
 Function getInt(MV: TMixedValue): Int64; inline;
 Function getFloat(MV: TMixedValue): Extended; inline;
 Function getString(MV: TMixedValue): PChar; inline;
 Function getReference(MV: TMixedValue): Pointer; inline;

 Function getTypeName(MV: TMixedValue): String;

 Implementation
Uses VMStrings, SysUtils;

(* = *)
Operator = (P1, P2: TMixedValue): Boolean;
Begin
 Result := False;

 if (P1.Typ = mvFloat) and (P2.Typ = mvInt) { float = int } Then
  Exit(P1.Value.Float = P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvFloat) { int = float } Then
  Exit(P1.Value.Int = P2.Value.Float);

 if (P1.Typ = mvChar) and (P2.Typ = mvInt) { char = int } Then
  Exit(ord(P1.Value.Char) = P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvChar) { int = char } Then
  Exit(P1.Value.Int = ord(P2.Value.Char));

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  mvBool  : Exit(P1.Value.Bool = P2.Value.Bool);
  mvChar  : Exit(P1.Value.Char = P2.Value.Char);
  mvInt   : Exit(P1.Value.Int = P2.Value.Int);
  mvFloat : Exit(P1.Value.Float = P2.Value.Float);
  mvString: Exit(P1.Value.Str = P2.Value.Str);
 End;
End;

(* <> *)
Operator <> (P1, P2: TMixedValue): Boolean;
Begin
 Result := not (P1 = P2);
End;

(* > *)
Operator > (P1, P2: TMixedValue): Boolean;
Begin
 Result := False;

 if (P1.Typ = mvFloat) and (P2.Typ = mvInt) { float > int } Then
  Exit(P1.Value.Float > P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvFloat) { int > float } Then
  Exit(P1.Value.Int > P2.Value.Float);

 if (P1.Typ = mvChar) and (P2.Typ = mvInt) { char > int } Then
  Exit(ord(P1.Value.Char) > P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvChar) { int > char } Then
  Exit(P1.Value.Int > ord(P2.Value.Char));

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  mvBool  : Exit(P1.Value.Bool > P2.Value.Bool);
  mvChar  : Exit(P1.Value.Char > P2.Value.Char);
  mvInt   : Exit(P1.Value.Int > P2.Value.Int);
  mvFloat : Exit(P1.Value.Float > P2.Value.Float);
  mvString: Exit(P1.Value.Str > P2.Value.Str);
 End;
End;

(* >= *)
Operator >= (P1, P2: TMixedValue): Boolean;
Begin
 Result := False;

 if (P1.Typ = mvFloat) and (P2.Typ = mvInt) { float >= int } Then
  Exit(P1.Value.Float >= P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvFloat) { int >= float } Then
  Exit(P1.Value.Int >= P2.Value.Float);

 if (P1.Typ = mvChar) and (P2.Typ = mvInt) { char >= int } Then
  Exit(ord(P1.Value.Char) >= P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvChar) { int >= char } Then
  Exit(P1.Value.Int >= ord(P2.Value.Char));

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  mvBool  : Exit(P1.Value.Bool >= P2.Value.Bool);
  mvChar  : Exit(P1.Value.Char >= P2.Value.Char);
  mvInt   : Exit(P1.Value.Int >= P2.Value.Int);
  mvFloat : Exit(P1.Value.Float >= P2.Value.Float);
  mvString: Exit(P1.Value.Str >= P2.Value.Str);
 End;
End;

(* < *)
Operator < (P1, P2: TMixedValue): Boolean;
Begin
 Result := False;

 if (P1.Typ = mvFloat) and (P2.Typ = mvInt) { float < int } Then
  Exit(P1.Value.Float < P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvFloat) { int < float } Then
  Exit(P1.Value.Int < P2.Value.Float);

 if (P1.Typ = mvChar) and (P2.Typ = mvInt) { char < int } Then
  Exit(ord(P1.Value.Char) < P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvChar) { int < char } Then
  Exit(P1.Value.Int < ord(P2.Value.Char));

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  mvBool  : Exit(P1.Value.Bool < P2.Value.Bool);
  mvChar  : Exit(P1.Value.Char < P2.Value.Char);
  mvInt   : Exit(P1.Value.Int < P2.Value.Int);
  mvFloat : Exit(P1.Value.Float < P2.Value.Float);
  mvString: Exit(P1.Value.Str < P2.Value.Str);
 End;
End;

(* <= *)
Operator <= (P1, P2: TMixedValue): Boolean;
Begin
 Result := False;

 if (P1.Typ = mvFloat) and (P2.Typ = mvInt) { float <= int } Then
  Exit(P1.Value.Float <= P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvFloat) { int <= float } Then
  Exit(P1.Value.Int <= P2.Value.Float);

 if (P1.Typ = mvChar) and (P2.Typ = mvInt) { char <= int } Then
  Exit(ord(P1.Value.Char) <= P2.Value.Int);

 if (P1.Typ = mvInt) and (P2.Typ = mvChar) { int <= char } Then
  Exit(P1.Value.Int <= ord(P2.Value.Char));

 if (P1.Typ <> P2.Typ) Then
  Exit(False);

 Case P1.Typ of
  mvBool  : Exit(P1.Value.Bool <= P2.Value.Bool);
  mvChar  : Exit(P1.Value.Char <= P2.Value.Char);
  mvInt   : Exit(P1.Value.Int <= P2.Value.Int);
  mvFloat : Exit(P1.Value.Float <= P2.Value.Float);
  mvString: Exit(P1.Value.Str <= P2.Value.Str);
 End;
End;

(* getBool *)
Function getBool(MV: TMixedValue): Boolean;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PBoolean(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(Value.Bool);

   { int }
   mvInt: Exit(Boolean(Value.Int));
  End;
 End;

 raise Exception.Create('Invalid casting: '+MixedValueTypeNames[MV.Typ]+' -> bool');
End;

(* getChar *)
Function getChar(MV: TMixedValue): Char;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PChar(MemAddr)^);

  Case Typ of
   { char }
   mvChar: Exit(Value.Char);

   { int }
   mvInt: Exit(Char(Value.Int));

   { string }
   mvString: Exit(Value.Str[1]);
  End;
 End;

 raise Exception.Create('Invalid casting: '+MixedValueTypeNames[MV.Typ]+' -> char');
End;

(* getInt *)
Function getInt(MV: TMixedValue): Int64;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PInt64(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(Int64(Value.Bool));

   { char }
   mvChar: Exit(ord(Value.Char));

   { int, reference, callstack ref }
   mvInt, mvReference, mvCallstackRef: Exit(Value.Int);

   { float }
   mvFloat: Exit(Round(Value.Float)); // @TODO: inf, NaN?
  End;
 End;

 raise Exception.Create('Invalid casting: '+MixedValueTypeNames[MV.Typ]+' -> int');
End;

(* getFloat *)
Function getFloat(MV: TMixedValue): Extended;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PExtended(MemAddr)^);

  Case Typ of
   { bool }
   mvBool: Exit(uint8(Value.Bool));

   { int, reference, callstack ref }
   mvInt, mvReference, mvCallstackRef: Exit(Value.Int);

   { float }
   mvFloat: Exit(Value.Float);
  End;
 End;

 raise Exception.Create('Invalid casting: '+MixedValueTypeNames[MV.Typ]+' -> float');
End;

(* getString *)
Function getString(MV: TMixedValue): PChar;
Begin
 With MV do
 Begin
  if (isMemRef) Then
   Exit(PPChar(MemAddr)^);

  Case Typ of
   { char }
   mvChar: Exit(CopyStringToPChar(Value.Char));

   { string }
   mvString: Exit(Value.Str);
  End;
 End;

 raise Exception.Create('Invalid casting: '+MixedValueTypeNames[MV.Typ]+' -> string');
End;

(* getReference *)
Function getReference(MV: TMixedValue): Pointer;
Begin
 Result := Pointer(uint32(getInt(MV)));
End;

(* getTypeName *)
Function getTypeName(MV: TMixedValue): String;
Begin
 Result := MixedValueTypeNames[MV.Typ];

 if (MV.isReg) Then
  Result += ' reg';
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
 MemAddr    := 0;
End;

(* TMixedValue.RelaseData *)
{
 Releases string data, if 'Typ' = 'mvString'
}
Procedure TMixedValue.ReleaseData;
Begin
 if (Typ = mvString) Then
  FreeMem(Value.Str);
End;
End.
