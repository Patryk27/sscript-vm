(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit mTime;

 Interface
 Uses vm_header;

 Procedure Init(VM: Pointer);

 Implementation
Uses os_functions;

{ time.get_milliseconds }
Procedure _get_milliseconds(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 Result^ := GetMilliseconds;
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 SSAddInternalCall(VM, 'time', 'get_milliseconds', 0, @_get_milliseconds);
End;
End.
