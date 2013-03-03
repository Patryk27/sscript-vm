(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mVM;

 Interface

 Implementation
Uses Machine, SysUtils;

{ vm.exit }
Procedure _exit(M: TMachine);
Begin
With M do
Begin
 M.exitcode := StackPop.getInt;

 Log('');
 Log('-- STOP (reason: `vm.exit`) --');
 raise Exception.Create('');
End;
End;

initialization
 NewFunction('vm', 'exit', @_exit);
End.
