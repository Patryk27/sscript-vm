(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mTime;

 Interface
 Uses Machine;

 Procedure Init(M: TMachine);

 Implementation
Uses os_functions;

{ time.get_milliseconds }
Procedure _get_milliseconds(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpInt;
 Result.Value := GetMilliseconds;
End;

// -------------------------------------------------------------------------- //
Procedure Init(M: TMachine);
Begin
 M.AddInternalCall('time', 'get_milliseconds', 0, @_get_milliseconds);
End;
End.
