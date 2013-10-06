(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 A simple mark-and-sweep garbage collector for the SScript Virtual Machine.
*)
Unit GC;

 Interface
 Uses Classes, SysUtils, FGL, VM, Objects;

 Const ObjectListCount = 1; // Number of object allocation lists; keep in mind that the higher number you put here, the higher amount of threads will be executed during the GC's mark stage (1 object list = 1 thread) and that doesn't mean it will be any faster. In fact, more than 2 sweeping-threads will most likely just slow down everything.

 Type TObjectList = specialize TFPGList<TMObject>;

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

        Procedure PutObject(const Obj: TMObject);
        Function FindObject(const Obj: TMObject): Boolean;

        Procedure DoGarbageCollection;
       End;

 Implementation
Uses Stack;

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
Procedure TGarbageCollector.Mark(const ObjectList: TObjectList);

  { TryToMark }
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

 For I := Low(VM^.Regs.r) To High(VM^.Regs.r) Do // check the reference registers
  TryToMark(VM^.Regs.r[i]);
End;

(* TGarbageCollector.Sweep *)
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
End;

(* TGarbageCollector.Destroy *)
Destructor TGarbageCollector.Destroy;
Var List: TObjectList;
    Obj : TMObject;
Begin
 For List in ObjectLists Do
 Begin
  For Obj in List Do
   Obj.Free;
  List.Free;
 End;
End;

(* TGarbageCollector.PutObject *)
Procedure TGarbageCollector.PutObject(const Obj: TMObject);
Var List: TObjectList;
    I   : uint8;
Begin
 if (GetFPCHeapStatus.CurrHeapUsed > MemoryLimit) Then
  DoGarbageCollection;

 List := ObjectLists[Low(ObjectLists)];

 For I := Low(ObjectLists)+1 To High(ObjectLists) Do
  if (ObjectLists[I].Count < List.Count) Then // choose the list with the least number of elements
   List := ObjectLists[I];

 List.Add(Obj);
End;

(* TGarbageCollector.FindObject *)
Function TGarbageCollector.FindObject(const Obj: TMObject): Boolean;
Var List: TObjectList;
Begin
 Result := False;

 For List in ObjectLists Do
  if (List.IndexOf(Obj) > -1) Then
   Exit(True);
End;

(* TGarbageCollector.DoGarbageCollection *)
Procedure TGarbageCollector.DoGarbageCollection;
Var List: TObjectList;
Begin
 For List in ObjectLists Do
  Mark(List);

 Sweep();
End;
End.
