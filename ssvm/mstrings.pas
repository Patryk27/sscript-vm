(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mStrings;

 Interface

 Type PStringData = ^TStringData;
      TStringData = Packed Record
                     Length: uint32;
                     Data  : PChar;
                    End;

 Type PStringHeader = ^TStringHeader;
      TStringHeader = Packed Record
                       DataPointer: PStringData;
                      End;

 Function CopyStringToPChar(const S: String): PChar;

 Procedure SaveString(const Pointer: Pointer; const Data: String); inline;
 Function LoadString(const Pointer: Pointer): String; inline;
 Procedure ReleaseStringData(const Pointer: Pointer); inline;

 Implementation

(* CopyStringToPChar *)
Function CopyStringToPChar(const S: String): PChar;
Var I: uint32;
Begin
 Result := AllocMem(Length(S)+1);

 For I := 1 To Length(S) Do
  Result[I-1] := S[I];
End;

(* SaveString *)
Procedure SaveString(const Pointer: Pointer; const Data: String);
Begin
 ReleaseStringData(Pointer);

 With PStringHeader(Pointer)^ do
 Begin
  New(DataPointer);
  DataPointer^.Length := Length(Data);
  DataPointer^.Data   := CopyStringToPChar(Data);
 End;
End;

(* LoadString *)
Function LoadString(const Pointer: Pointer): String;
Begin
 With PStringHeader(Pointer)^ do
 Begin
  if (DataPointer = nil) Then
   Result := '' Else
   Result := DataPointer^.Data;
 End;
End;

(* ReleaseStringData *)
Procedure ReleaseStringData(const Pointer: Pointer);
Begin
 With PStringHeader(Pointer)^ do
 Begin
  if (DataPointer <> nil) Then
  Begin
   FreeMem(DataPointer^.Data);
   FreeMem(DataPointer);
  End;

  DataPointer := nil;
 End;
End;

End.
