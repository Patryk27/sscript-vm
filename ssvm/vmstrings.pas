(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
// @Note: VMString itself is declared in VMTypes.pas (!)
{$H+}
Unit VMStrings;

 Interface
 Uses VMTypes, VMElement, FGL;

 { VMStringList }
 Type VMStringList = specialize TFPGList<PVMString>;

 { TVMStringList }
 Type TVMStringList =
      Class(TVMElement)
       Private
        List: VMStringList;

       Public
        Constructor Create(const fVM: Pointer);
        Destructor Destroy; override;

        Function StringToVMString(const Value: String): PVMString;
        Function CharToVMString(const Value: Char): PVMString;
        Function CloneVMString(const Value: PVMString): PVMString;

        Procedure Purge;
        Procedure PurgeUnused;

        Procedure Remove(const Str: PVMString);
        Procedure Unbind(const Str: PVMString);
        Procedure Dispose(const Str: PVMString);
       End;

 Function StringToPChar(const S: String; const AddNullChar: Boolean=True): PChar;

 Implementation
Uses VMStruct, VMStack;

(* StringToPChar *)
Function StringToPChar(const S: String; const AddNullChar: Boolean=True): PChar;
Var I: uint32;
Begin
 Result := AllocMem(Length(S)+ord(AddNullChar));

 For I := 1 To Length(S) Do
  Result[I-1] := S[I];
End;

// -------------------------------------------------------------------------- //
(* TVMStringList.Create *)
Constructor TVMStringList.Create(const fVM: Pointer);
Begin
 inherited Create(fVM);

 List := VMStringList.Create;
End;

(* TVMStringList.Destroy *)
Destructor TVMStringList.Destroy;
Begin
 WriteLog('Destroying VMStringList instance...');

 Purge;
 List.Free;

 inherited Destroy;
End;

(* TVMStringList.StringToVMString *)
{
 Converts String to VMString and puts it onto the list.
}
Function TVMStringList.StringToVMString(const Value: String): PVMString;
Begin
 CheckMemory;

 New(Result);
 List.Add(Result);

 Result^.Length := Length(Value);
 Result^.Data   := StringToPChar(Value, False);
End;

(* TVMStringList.CharToVMString *)
{
 Converts Char to VMString and puts it onto the list.
}
Function TVMStringList.CharToVMString(const Value: Char): PVMString;
Begin
 Result := StringToVMString(Value); // affirmative - I'm lazy as hell
End;

(* TVMString.CloneVMString *)
{
 Clones VMString an puts it onto the list.
}
Function TVMStringList.CloneVMString(const Value: PVMString): PVMString;
Begin
 if (Value = nil) Then
 Begin
  Result := StringToVMString('');
  Exit;
 End;

 CheckMemory;

 New(Result);
 List.Add(Result);

 Result^.Length := Value^.Length;
 Result^.Data   := GetMem(Result^.Length);

 Move(Value^.Data[0], Result^.Data[0], Result^.Length);
End;

(* TVMStringList.Purge *)
{
 Relases memory data of all registered VMStrings.
}
Procedure TVMStringList.Purge;
Var Pnt: PVMString;
    Mem: uint32;
Begin
 WriteLog('Purging all VMStrings data (%d VMString instances to free)', [List.Count]);

 Mem := GetFPCHeapStatus.CurrHeapFree;
 For Pnt in List Do
 Begin
  FreeMem(Pnt^.Data);
  FreeMem(Pnt);
 End;
 Mem := GetFPCHeapStatus.CurrHeapFree - Mem;

 WriteLog('Released %d kB of memory (%d bytes).', [Mem div 1024, Mem]);

 List.Clear;
End;

(* TVMStringList.PurgeUnused *)
{
 Removes unused strings from the list and frees their memory.
}
Procedure TVMStringList.PurgeUnused;
Var VM       : PVM;
    Removable: VMStringList;
    NewList  : VMStringList;
    Str      : PVMString;
    I        : uint32;
    Memory   : uint32;
Begin
 WriteLog('Purging unused VMStrings memory...');

 VM := VMPnt;

 Removable := VMStringList.Create;
 NewList   := VMStringList.Create;

 // prepare list
 For Str in List Do
  Removable.Add(Str);

 For I := Low(VM^.Regs.s) To High(VM^.Regs.s) Do
 Begin
  Removable.Remove(VM^.Regs.s[I]);
  NewList.Add(VM^.Regs.s[I]);
 End;

 if (VM^.StackPos^ > 0) Then
 Begin
  For I := 0 To VM^.StackPos^-1 Do
  Begin
   if (VM^.Stack[I].Typ = mvString) Then
   Begin
    Removable.Remove(VM^.Stack[I].Value.Str);
    NewList.Add(VM^.Stack[I].Value.Str);
   End;
  End;
 End;

 // remove VMStrings
 Memory := GetFPCHeapStatus.CurrHeapUsed;

 For Str in Removable Do
  Dispose(Str);

 Memory := Memory - GetFPCHeapStatus.CurrHeapUsed;

 // set new pointer
 List.Free;
 List := NewList;

 // write log
 WriteLog('Removed %d VMStrings (%d kB memory freed), %d VMStrings are currently in use.', [Removable.Count, Memory div 1024, NewList.Count]);

 // dispose class instances
 Removable.Free;
End;

(* TVMStringList.Remove *)
{
 Removes string from the list and frees its memory.
}
Procedure TVMStringList.Remove(const Str: PVMString);
Begin
 List.Remove(Str);
 Dispose(Str);
End;

(* TStringList.Unbind *)
{
 Removes string from the list and **doesn't** free its memory.
}
Procedure TVMStringList.Unbind(const Str: PVMString);
Begin
 List.Remove(Str);
End;

(* TVMStringList.Dispose *)
{
 Frees string memory.
}
Procedure TVMStringList.Dispose(const Str: PVMString);
Begin
 if (Str <> nil) Then
 Begin
  FreeMem(Str^.Data);
  FreeMem(Str);
 End;
End;
End.
