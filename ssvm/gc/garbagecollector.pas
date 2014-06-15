(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 A simple mark-and-sweep garbage collector for the SScript Virtual Machine.
*)
Unit GarbageCollector;

 Interface
 Uses Classes, SysUtils, FGL, VMStruct, VMObjects;

 Type TObjectList = specialize TFPGList<TSSVMObject>;

 { TGarbageCollector }
 Type TGarbageCollector =
      Class
       Private
        VM         : PVM;
        MemoryLimit: uint32;

        ObjectList, NewObjectList: TObjectList;

       Private
        Procedure Mark;
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

(* TGarbageCollector.Mark *)
{
 Does the "mark" stage of GC.
}
Procedure TGarbageCollector.Mark;

  { TryToMark }
  Procedure TryToMark(const Address: Pointer);
  Var Obj: TSSVMObject;
  Begin
   Obj := TSSVMObject(Address);
   if (ObjectList.IndexOf(Obj) > -1) and (not Obj.isMarked) Then
    Obj.GCMark;
  End;

Var Obj: TSSVMObject;
    I  : int32;
Begin
 // unmark all objects
 For I := 0 To ObjectList.Count-1 Do
  ObjectList[I].isMarked := False;

 // traverse the stack
 if (VM^.StackPos^ > 0) Then
 Begin
  For I := 0 To VM^.StackPos^-1 Do
   if (VM^.Stack[I].Typ = mvReference) Then
    TryToMark(Pointer(VM^.Stack[I].Value.Int));
 End;

 // check the reference registers
 For I := Low(VM^.Regs.r) To High(VM^.Regs.r) Do
  TryToMark(VM^.Regs.r[i]);
End;

(* TGarbageCollector.Sweep *)
{
 Does the "sweep" stage of GC.
}
Procedure TGarbageCollector.Sweep;
Var Tmp: TObjectList;
    I  : int32;
Begin
 // remove unwanted objects
 For I := 0 To ObjectList.Count-1 Do
 Begin
  if (ObjectList[I].isMarked) Then
   NewObjectList.Add(ObjectList[I]) Else
   ObjectList[I].Free;
 End;

 // swap lists
 ObjectList.Clear;

 Tmp           := ObjectList;
 ObjectList    := NewObjectList;
 NewObjectList := Tmp;
End;

(* TGarbageCollector.Create *)
Constructor TGarbageCollector.Create(const fVM: PVM; const fMemoryLimit: uint32);
Var I: uint8;
Begin
 VM          := fVM;
 MemoryLimit := fMemoryLimit;

 ObjectList    := TObjectList.Create;
 NewObjectList := TObjectList.Create;

 VM^.WriteLog('GC instance created.');
End;

(* TGarbageCollector.Destroy *)
{
 Frees all the objects registered by the GC and then frees itself.
}
Destructor TGarbageCollector.Destroy;
Var Mem: uint32;
    I  : int32;
Begin
 // log
 VM^.WriteLog('GC is about to be destroyed - %d objects left to free...', [getObjectsCount]);

 // get memory status
 Mem := GetFPCHeapStatus.CurrHeapFree;

 // destroy objects and the object list
 For I := 0 To ObjectList.Count-1 Do
  ObjectList[I].Free;
 ObjectList.Free;
 NewObjectList.Free;

 // update memory status
 Mem := GetFPCHeapStatus.CurrHeapFree - Mem;

 // log
 VM^.WriteLog('Freed %d kB of memory (%d bytes), destroying GC instance...', [Mem div 1024, Mem]);
End;

(* TGarbageCollector.PutObject *)
{
 Puts an object instance on the list.
}
Procedure TGarbageCollector.PutObject(const Obj: TSSVMObject);
Begin
 // check memory status
 VM^.CheckMemory;

 // put object on the list
 ObjectList.Add(Obj);
End;

(* TGarbageCollector.FindObject *)
{
 Returns "true" if given object is on the object list, i.e. - returns "true" if it's a valid object address.
}
Function TGarbageCollector.FindObject(const Obj: TSSVMObject): Boolean;
Var List: TObjectList;
Begin
 Result := (ObjectList.indexOf(Obj) > 0);
End;

(* TGarbageCollector.DoGarbageCollection *)
{
 Does mark-and-sweep.
}
Procedure TGarbageCollector.DoGarbageCollection;
Var List: TObjectList;
    Mem : uint32;
Begin
 // log
 VM^.WriteLog('Doing garbage collection...');

 // get memory status
 Mem := GetFPCHeapStatus.CurrHeapFree;

 // stage 1: mark
 Mark();

 // stage 2: sweep
 Sweep();

 // update memory status
 Mem := GetFPCHeapStatus.CurrHeapFree - Mem;

 // log
 VM^.WriteLog('Freed %d kB of memory', [Mem div 1024]);
End;

(* TGarbageCollector.getObjectsCount *)
{
 Returns number of objects on the list.
}
Function TGarbageCollector.getObjectsCount: uint32;
Begin
 Result := ObjectList.Count;
End;
End.
