(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITJumpTable;

 Interface
 Uses FGL;

 { TJITJumpRecord }
 Type PJITJumpRecord = ^TJITJumpRecord;
      TJITJumpRecord =
      Packed Record
       BytecodeAddress, CodeAddress: uint32;
      End;

 { TJITJumpRecordList }
 Type TJITJumpRecordList = specialize TFPGList<PJITJumpRecord>;

 { TJITJumpTable }
 Type TJITJumpTable =
      Class
       Private
        List: TJITJumpRecordList;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure AddJump(const JumpRecord: TJITJumpRecord);
        Procedure AddJump(const BytecodeAddress, CodeAddress: uint32);

        Function FindJumpByBytecodeAddress(const Address: uint32; out Rec: TJITJumpRecord): Boolean;

        Function getLast: TJITJumpRecord;
       End;

 Implementation

(* TJITJumpTable.Create *)
Constructor TJITJumpTable.Create;
Begin
 List := TJITJumpRecordList.Create;
End;

(* TJITJumpTable.Destroy *)
Destructor TJITJumpTable.Destroy;
Var Rec: PJITJumpRecord;
Begin
 inherited Destroy;

 For Rec in List Do
  Dispose(Rec);
 List.Free;
End;

(* TJITJumpTable.AddJump *)
Procedure TJITJumpTable.AddJump(const JumpRecord: TJITJumpRecord);
Var Rec: PJITJumpRecord;
Begin
 New(Rec);
 Rec^ := JumpRecord;

 List.Add(Rec);
End;

(* TJITJumpTable.AddJump *)
Procedure TJITJumpTable.AddJump(const BytecodeAddress, CodeAddress: uint32);
Var Jump: TJITJumpRecord;
Begin
 Jump.BytecodeAddress := BytecodeAddress;
 Jump.CodeAddress     := CodeAddress;

 AddJump(Jump);
End;

(* TJITJumpTable.FindJumpByBytecodeAddress *)
Function TJITJumpTable.FindJumpByBytecodeAddress(const Address: uint32; out Rec: TJITJumpRecord): Boolean;
Var Pnt: PJITJumpRecord;
Begin
 For Pnt in List Do
  if (Pnt^.BytecodeAddress = Address) Then
  Begin
   Rec := Pnt^;
   Exit(True);
  End;

 Exit(False);
End;

(* TJITJumpTable.getLast *)
Function TJITJumpTable.getLast: TJITJumpRecord;
Begin
 Result := List.Last^;
End;
End.
