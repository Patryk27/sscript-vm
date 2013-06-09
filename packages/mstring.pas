(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mString;

 Interface

 Procedure Init(VM: Pointer);

 Implementation
Uses vm_header;

{ string.length }
Procedure _length(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 Result^ := Length(getString(Params[0]));
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 AddInternalCall(VM, 'string', 'length', 1, @_length);
End;
End.
