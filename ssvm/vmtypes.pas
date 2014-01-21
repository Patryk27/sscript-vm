(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
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

 Type VMBool      = Boolean;
      VMChar      = Char;
      VMInt       = Int64;
      VMFloat     = Extended;
      VMString    = String;
      VMReference = Pointer;

 Type PVMBool      = ^VMBool;
      PVMChar      = ^VMChar;
      PVMInt       = ^VMInt;
      PVMFloat     = ^VMFloat;
      PVMString    = ^VMString;
      PVMReference = ^VMReference;

 Implementation

End.
