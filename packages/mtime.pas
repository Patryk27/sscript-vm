{$H+}
Unit mTime;

 Interface

 Implementation
Uses Machine, Windows;

{ time.get_tick_count }
Procedure _get_tick_count(M: TMachine);
Begin
 M.StackPush(GetTickCount);
End;

initialization
 NewFunction('time', 'get_tick_count', @_get_tick_count);
End.
