(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMTypes;

 Interface

 Type Puint8  = ^uint8; // for some reason FPC doesn't have them declared
      Pint8   = ^int8;
      Puint16 = ^uint16;
      Pint16  = ^int16;
      Puint32 = ^uint32;
      Pint32  = ^int32;
      Puint64 = ^uint64;
      Pint64  = ^int64;

 Type VMBool       = Boolean;
      VMChar       = Char;
      VMInt        = Int64;
      VMFloat      = Extended;
      VMReference  = Pointer;
      VMIReference = {$IFDEF CPU64} uint64 {$ELSE} uint32 {$ENDIF};

 Type VMString =
      Record
       Length: uint32;
       Data  : PChar;

       Procedure setNull;

       Function asString: String;
      End;

 Type PVMBool      = ^VMBool;
      PVMChar      = ^VMChar;
      PVMInt       = ^VMInt;
      PVMFloat     = ^VMFloat;
      PVMString    = ^VMString;
      PVMReference = ^VMReference;

 Type PPVMString = ^PVMString;

 Const TYPE_BOOL_id   = 0; // do not modify
       TYPE_CHAR_id   = 1;
       TYPE_INT_id    = 2;
       TYPE_FLOAT_id  = 3;
       TYPE_STRING_id = 4;

       TypeSizes: Array[TYPE_BOOL_id..TYPE_STRING_id] of Byte = (sizeof(VMBool), sizeof(VMChar), sizeof(VMInt), sizeof(VMFloat), sizeof(PVMString));

 Implementation

(* VMString.setNull *)
{
 Resets VMString.

 Note: doesn't free any memory!
}
Procedure VMString.setNull;
Begin
 Length := 0;
 Data   := nil;
End;

(* VMString.asString *)
{
 Returns string representation of VMString.
}
Function VMString.asString: String;
Var I: uint32;
    P: PChar;
Begin
 Result := '';
 P      := @Data[0];

 For I := 1 To Length Do
 Begin
  Result += P^;
  Inc(P);
 End;
End;
End.
