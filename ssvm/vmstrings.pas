(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMStrings;

 Interface
 Uses VMTypes;

 Function StringToPChar(const S: String; const AddNullChar: Boolean=True): PChar;

 Operator := (const Value: String): VMString;

 Operator = (const A, B: VMString): Boolean;
 Operator + (const A, B: VMString): VMString;
 Operator + (const A: VMString; const B: Char): VMString;

 Implementation

(* StringToPChar *)
Function StringToPChar(const S: String; const AddNullChar: Boolean=True): PChar;
Var I: uint32;
Begin
 Result := AllocMem(Length(S)+ord(AddNullChar));

 For I := 1 To Length(S) Do
  Result[I-1] := S[I];
End;

(* VMString := String *)
Operator := (const Value: String): VMString;
Begin
 Result.Length := Length(Value);
 Result.Data   := StringToPChar(Value, False);
End;

(* VMString = VMString *)
Operator = (const A, B: VMString): Boolean;
Var I: uint32;
Begin
 Result := (A.Length = B.Length);

 if (Result) and (A.Length > 0) Then
 Begin
  For I := 0 To A.Length-1 Do
   if (A.Data[I] <> B.Data[I]) Then
    Exit(False);
 End;
End;

(* VMString + VMString *)
Operator + (const A, B: VMString): VMString;
Var I, D: uint32;
Begin
 Result.Length := A.Length + B.Length;
 Result.Data   := GetMem(Result.Length);

 D := 0;

 if (A.Length > 0) Then
  For I := 0 To A.Length-1 Do
  Begin
   Result.Data[D] := A.Data[I];
   Inc(D);
  End;

 if (B.Length > 0) Then
  For I := 0 To B.Length-1 Do
  Begin
   Result.Data[D] := B.Data[I];
   Inc(D);
  End;
End;

(* VMString + Char *)
Operator + (const A: VMString; const B: Char): VMString;
Begin
 Result.Length := A.Length+1;
 Result.Data   := GetMem(Result.Length);

 Move(A.Data[0], Result.Data[0], A.Length);

 Result.Data[A.Length] := B;
End;
End.
