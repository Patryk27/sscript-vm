(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JIT_JumpTable;

 Interface
 Uses FGL;

 { TJumpRecord }
 Type PJumpRecord = ^TJumpRecord;
      TJumpRecord =
      Packed Record
       BytecodeAddress, JumpAddress: uint32;
      End;

 { TJumpRecordList }
 Type TJumpRecordList = specialize TFPGList<PJumpRecord>;

 { TJumpTable }
 Type TJumpTable =
      Class
       Private
        List: TJumpRecordList;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure AddJump(const JumpRecord: TJumpRecord);
        Procedure AddJump(const BytecodeAddress, JumpAddress: uint32);

        Function FindJumpByBytecodeAddress(const Address: uint32): PJumpRecord;
       End;

 Implementation

(* TJumpTable.Create *)
Constructor TJumpTable.Create;
Begin
 List := TJumpRecordList.Create;
End;

(* TJumpTable.Destroy *)
Destructor TJumpTable.Destroy;
Var Rec: PJumpRecord;
Begin
 inherited Destroy;

 For Rec in List Do
  Dispose(Rec);
 List.Free;
End;

(* TJumpTable.AddJump *)
Procedure TJumpTable.AddJump(const JumpRecord: TJumpRecord);
Var Rec: PJumpRecord;
Begin
 New(Rec);
 Rec^ := JumpRecord;

 List.Add(Rec);
End;

(* TJumpTable.AddJump *)
Procedure TJumpTable.AddJump(const BytecodeAddress, JumpAddress: uint32);
Var Jump: TJumpRecord;
Begin
 Jump.BytecodeAddress := BytecodeAddress;
 Jump.JumpAddress     := JumpAddress;

 AddJump(Jump);
End;

(* TJumpTable.FindJumpByBytecodeAddress *)
Function TJumpTable.FindJumpByBytecodeAddress(const Address: uint32): PJumpRecord;
Begin
 For Result in List Do
  if (Result^.BytecodeAddress = Address) Then
   Exit;

 Exit(nil);
End;
End.
