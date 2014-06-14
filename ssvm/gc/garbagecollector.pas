(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 A simple mark-and-sweep garbage collector for the SScript Virtual Machine.
*)
Unit GarbageCollector;

 Interface
 Uses Classes, SysUtils, FGL, VMStruct, VMObjects;

 Const ObjectListCount = 1; // Number of object allocation lists; keep in mind that the higher number you put here, the higher amount of threads will be executed during the GC's mark stage (1 object list = 1 thread) and that doesn't mean it will be any faster. In fact, more than 2 sweeping-threads will most likely just slow down everything.

 Type TObjectList = specialize TFPGList<TSSVMObject>;

 { TGarbageCollector }
 Type TGarbageCollector =
      Class
       Private
        ObjectLists: Array[1..ObjectListCount] of TObjectList;

        VM         : PVM;
        MemoryLimit: uint32;

       Private
        Procedure Mark(const ObjectList: TObjectList);
        Procedure Sweep;

       Public
        Constructor Create(const fVM: PVM; const fMemoryLimit: uint32);
        Destructor Destroy; override;

        Procedure PutObject(const Obj: TSSVMObject);
        Function FindObject(const Obj: TSSVMObject): Boolean;

        Procedure DoGarbageCollection;

        Function getObjectsCount: uint32;
        Property getMemoryLimit: uint32 read MemoryLimit;
       End;

 Implementation
Uses VMStack;

{ TGCSweepWorker }
Type TGCSweepWorker =
     Class(TThread)
      Private
      ObjectList: TObjectList;
      isWorking : Boolean;

      Public
       Constructor Create(const fObjectList: TObjectList);

      Protected
       Procedure Execute; override;
      End;

(* TGCSweepWorker.Create *)
Constructor TGCSweepWorker.Create(const fObjectList: TObjectList);
Begin
 ObjectList := fObjectList;
 isWorking  := True;

 inherited Create(False);
End;

(* TGCSweepWorker.Execute *)
Procedure TGCSweepWorker.Execute;
Var I, ObjectCount: int32;
Begin
 isWorking := True;

 ObjectCount := ObjectList.Count;
 I           := 0;

 While (I < ObjectCount) Do
 Begin
  if (not ObjectList[I].isMarked) Then
  Begin
   ObjectList[I].Free;
   ObjectList[I] := nil;
  End;

  Inc(I);
 End;

 isWorking := False;
End;

// -------------------------------------------------------------------------- //
(* TGarbageCollector.Mark *)
{
 Does the "mark" stage of GC.
}
Procedure TGarbageCollector.Mark(const ObjectList: TObjectList);

  { TryToMark }
  Procedure TryToMark(const Address: Pointer);
  Var Obj: TSSVMObject;
  Begin
   Obj := TSSVMObject(Address);
   if (ObjectList.IndexOf(Obj) > -1) and (not Obj.isMarked) Then
    Obj.GCMark;
  End;

Var Obj: TSSVMObject;
    I  : Integer;
Begin
 For Obj in ObjectList Do
  Obj.isMarked := False;

 if (VM^.StackPos^ > 0) Then // traverse the stack, if it's not empty
 Begin
  For I := 0 To VM^.StackPos^-1 Do
   if (VM^.Stack[I].Typ = mvReference) Then
    TryToMark(Pointer(VM^.Stack[I].Value.Int));
 End;

 For I := Low(VM^.Regs.r) To High(VM^.Regs.r) Do // check the reference registers
  TryToMark(VM^.Regs.r[i]);
End;

(* TGarbageCollector.Sweep *)
{
 Does the "sweep" stage of GC.
}
Procedure TGarbageCollector.Sweep;
Var Workers: Array[1..ObjectListCount] of TGCSweepWorker;
    I      : uint8;

  { isWorking }
  Function isWorking: Boolean;
  Var I: uint8;
  Begin
   Result := False;

   For I := Low(Workers) To High(Workers) Do
    if (Workers[I].isWorking) Then
     Exit(True);
  End;

  { CreateNewList }
  Procedure CreateNewList(var ObjectList: TObjectList);
  Var NewList: TObjectList;
      I      : Integer;
  Begin
   NewList := TObjectList.Create;

   For I := 0 To ObjectList.Count-1 Do // usually creating an entire new list is faster than removing elements from the previous one (especially when a lot of objects were removed during the sweep stage).
    if (ObjectList[I] <> nil) Then
     NewList.Add(ObjectList[I]);

   ObjectList.Free;
   ObjectList := NewList;
  End;

Begin
 For I := Low(Workers) To High(Workers) Do
  Workers[I] := TGCSweepWorker.Create(ObjectLists[I]);

 While (isWorking) Do; // wait while workers are sweeping memory

 For I := Low(Workers) To High(Workers) Do
  Workers[I].Free;

 // create new lists
 For I := Low(ObjectLists) To High(ObjectLists) Do
  CreateNewList(ObjectLists[I]);
End;

(* TGarbageCollector.Create *)
Constructor TGarbageCollector.Create(const fVM: PVM; const fMemoryLimit: uint32);
Var I: uint8;
Begin
 VM          := fVM;
 MemoryLimit := fMemoryLimit;

 For I := Low(ObjectLists) To High(ObjectLists) Do
  ObjectLists[I] := TObjectList.Create;

 VM^.WriteLog('GC instance created - %d objects list(s) allocated.', [Length(ObjectLists)]);
End;

(* TGarbageCollector.Destroy *)
{
 Frees all the objects registered by the GC and then frees itself.
}
Destructor TGarbageCollector.Destroy;
Var List: TObjectList;
    Obj : TSSVMObject;
    Mem : uint32;
Begin
 VM^.WriteLog('GC is about to be destroyed - %d objects left to free...', [getObjectsCount]);

 Mem := GetFPCHeapStatus.CurrHeapFree;

 For List in ObjectLists Do
 Begin
  For Obj in List Do
   Obj.Free;
  List.Free;
 End;

 Mem := GetFPCHeapStatus.CurrHeapFree - Mem;

 VM^.WriteLog('Freed %d kB of memory (%d bytes), destroying GC instance...', [Mem div 1024, Mem]);
End;

(* TGarbageCollector.PutObject *)
{
 Puts an object instance on the list.
}
Procedure TGarbageCollector.PutObject(const Obj: TSSVMObject);
Var List: TObjectList;
    I   : uint8;
Begin
 VM^.CheckMemory;

 List := ObjectLists[Low(ObjectLists)];

 For I := Low(ObjectLists)+1 To High(ObjectLists) Do
  if (ObjectLists[I].Count < List.Count) Then // choose the list with the least number of elements
   List := ObjectLists[I];

 List.Add(Obj);
End;

(* TGarbageCollector.FindObject *)
{
 Returns "true" if given object is on the object list, i.e. - returns "true" if it's a valid object address.
}
Function TGarbageCollector.FindObject(const Obj: TSSVMObject): Boolean;
Var List: TObjectList;
Begin
 Result := False;

 For List in ObjectLists Do
  if (List.IndexOf(Obj) > -1) Then
   Exit(True);
End;

(* TGarbageCollector.DoGarbageCollection *)
{
 Does mark-and-sweep.
}
Procedure TGarbageCollector.DoGarbageCollection;
Var List: TObjectList;
    Mem : uint32;
Begin
 VM^.WriteLog('Doing garbage collection...');

 For List in ObjectLists Do
  Mark(List);

 Mem := GetFPCHeapStatus.CurrHeapFree;
 Sweep();
 Mem := GetFPCHeapStatus.CurrHeapFree - Mem;

 VM^.WriteLog('Freed %d kB of memory', [Mem div 1024]);
End;

(* TGarbageCollector.getObjectsCount *)
{
 Returns number of objects on the lists.
}
Function TGarbageCollector.getObjectsCount: uint32;
Var List: TObjectList;
Begin
 Result := 0;

 For List in ObjectLists Do
  Result += uint32(List.Count);
End;
End.
