(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mTime;

 Interface

 Implementation
Uses Machine, os_functions;

{ time.get_tick_count }
Procedure _get_tick_count(M: TMachine);
Begin
 M.StackPush(getTickCount);
End;

initialization
 NewFunction('time', 'get_tick_count', @_get_tick_count);
End.