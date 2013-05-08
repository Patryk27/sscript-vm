(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mTime;

 Interface

 Implementation
Uses Machine, os_functions;

{ time.get_milliseconds }
Procedure _get_milliseconds(M: TMachine);
Begin
 M.StackPush(GetMilliseconds);
End;

initialization
 Add_icall('time', 'get_milliseconds', @_get_milliseconds);
End.
