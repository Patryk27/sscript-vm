(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMStrings;

 Interface

 { TVMString }
 Type PVMString = ^TVMString;
      TVMString =
      Record
       Length: uint32; // string length
       Data  : PChar; // string data
      End;

 Function CopyStringToPChar(const S: String): PChar;

 Procedure SaveString(const VMString: PVMString; const Data: String);
 Function FetchString(const VMString: PVMString): String;
 Procedure ReleaseString(const VMString: PVMString);

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
Procedure SaveString(const VMString: PVMString; const Data: String);
Begin
 VMString^.Length := Length(Data);
 VMString^.Data   := CopyStringToPChar(Data);
End;

(* FetchString *)
Function FetchString(const VMString: PVMString): String;
Begin
 if (VMString^.Data = nil) Then
  Result := '' Else
  Result := VMString^.Data;
End;

(* ReleaseString *)
Procedure ReleaseString(const VMString: PVMString);
Begin
 FreeMem(VMString^.Data);
 FreeMem(VMString);
End;
End.
