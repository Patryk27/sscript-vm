(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

// ... VMStack.pas

(* = *)
Operator = (const P1, P2: TMixedValue): Boolean;
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
  mvString: Exit(P1.Value.Str^ = P2.Value.Str^);
 End;
End;

(* <> *)
Operator <> (const P1, P2: TMixedValue): Boolean;
Begin
 Result := (not (P1 = P2));
End;

(* > *)
Operator > (const P1, P2: TMixedValue): Boolean;
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
  mvBool : Exit(P1.Value.Bool > P2.Value.Bool);
  mvChar : Exit(P1.Value.Char > P2.Value.Char);
  mvInt  : Exit(P1.Value.Int > P2.Value.Int);
  mvFloat: Exit(P1.Value.Float > P2.Value.Float);
 End;
End;

(* >= *)
Operator >= (const P1, P2: TMixedValue): Boolean;
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
  mvBool : Exit(P1.Value.Bool >= P2.Value.Bool);
  mvChar : Exit(P1.Value.Char >= P2.Value.Char);
  mvInt  : Exit(P1.Value.Int >= P2.Value.Int);
  mvFloat: Exit(P1.Value.Float >= P2.Value.Float);
 End;
End;

(* < *)
Operator < (const P1, P2: TMixedValue): Boolean;
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
  mvBool : Exit(P1.Value.Bool < P2.Value.Bool);
  mvChar : Exit(P1.Value.Char < P2.Value.Char);
  mvInt  : Exit(P1.Value.Int < P2.Value.Int);
  mvFloat: Exit(P1.Value.Float < P2.Value.Float);
 End;
End;

(* <= *)
Operator <= (const P1, P2: TMixedValue): Boolean;
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
  mvBool : Exit(P1.Value.Bool <= P2.Value.Bool);
  mvChar : Exit(P1.Value.Char <= P2.Value.Char);
  mvInt  : Exit(P1.Value.Int <= P2.Value.Int);
  mvFloat: Exit(P1.Value.Float <= P2.Value.Float);
 End;
End;
