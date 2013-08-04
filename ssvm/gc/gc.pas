(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 Simple mark-and-sweep garbage collector for SScript Virtual Machine.
*)
Unit GC;

 Interface
 Uses Classes, SysUtils, FGL, VM, Objects;

 Type TObjectList = specialize TFPGList<TMObject>;

 Type TGarbageCollector = Class
                           Private
                            ObjectList: TObjectList;

                            VM         : PVM;
                            MemoryLimit: uint32;

                            Procedure Mark;
                            Procedure Sweep;

                           Public
                            Constructor Create(const fVM: PVM; const fMemoryLimit: uint32);
                            Destructor Destroy; override;

                            Procedure PutObject(const Obj: TMObject);
                            Function findObject(const Obj: TMObject): Boolean;

                            Procedure DoGarbageCollection;
                           End;

 Implementation
Uses Stack;

(* TGarbageCollector.Mark *)
Procedure TGarbageCollector.Mark;

  // TryToMark
  Procedure TryToMark(const Address: Pointer);
  Var Obj: TMObject;
  Begin
   Obj := TMObject(Address);
   if (ObjectList.IndexOf(Obj) > -1) and (not Obj.isMarked) Then
    Obj.GC_Mark;
  End;

Var Obj: TMObject;
    I  : Integer;
Begin
 For Obj in ObjectList Do
  Obj.isMarked := False;

 For I := VM^.Regs.i[5] Downto 0 Do // traverse the stack
  if (VM^.Stack[I].Typ = mvReference) Then
   TryToMark(Pointer(uint32(VM^.Stack[I].Value.Int)));

 For I := Low(VM^.Regs.r) To High(VM^.Regs.r) Do
  TryToMark(VM^.Regs.r[i]);
End;

(* TGarbageCollector.Sweep *)
Procedure TGarbageCollector.Sweep;
Var I, Removed: Int32;
Begin
 I       := 0;
 Removed := 0;

 While (I < ObjectList.Count) Do
 Begin
  if (not ObjectList[I].isMarked) Then
  Begin
   ObjectList[I].Free;
   ObjectList.Delete(I);
   Inc(Removed);
  End Else
   Inc(I);
 End;
End;

(* TGarbageCollector.Create *)
Constructor TGarbageCollector.Create(const fVM: PVM; const fMemoryLimit: uint32);
Begin
 VM          := fVM;
 MemoryLimit := fMemoryLimit;
 ObjectList  := TObjectList.Create;
End;

(* TGarbageCollector.Destroy *)
Destructor TGarbageCollector.Destroy;
Var Obj: TMObject;
Begin
 For Obj in ObjectList Do
  Obj.Free;
 ObjectList.Free;
End;

(* TGarbageCollector.PutObject *)
Procedure TGarbageCollector.PutObject(const Obj: TMObject);
Begin
 if (GetFPCHeapStatus.CurrHeapUsed > MemoryLimit) Then
  DoGarbageCollection;

 ObjectList.Add(Obj);
End;

(* TGarbageCollector.findObject *)
Function TGarbageCollector.findObject(const Obj: TMObject): Boolean;
Begin
 Result := (ObjectList.IndexOf(Obj) > -1);
End;

(* TGarbageCollector.DoGarbageCollection *)
Procedure TGarbageCollector.DoGarbageCollection;
Begin
 Mark();
 Sweep();
End;
End.
