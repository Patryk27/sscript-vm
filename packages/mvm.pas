(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mVM;

 Interface

 Implementation
Uses Machine, SysUtils;

Type eThrow = Class(Exception);

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

{ vm.save_exception_handler }
Procedure _save_exception_handler(M: TMachine);
Begin
With M do
Begin
 Inc(ExceptionStackPos);
 ExceptionStack[ExceptionStackPos] := exception_handler;
End;
End;

{ vm.restore_exception_handler }
Procedure _restore_exception_handler(M: TMachine);
Begin
With M do
Begin
 exception_handler := ExceptionStack[ExceptionStackPos];
 Dec(ExceptionStackPos);
End;
End;

{ vm.set_exception_handler }
Procedure _set_exception_handler(M: TMachine);
Begin
With M do
 exception_handler := StackPop.getReference;
End;

{ vm.get_exception_handler }
Procedure _get_exception_handler(M: TMachine);
Begin
With M do
 StackPush(exception_handler);
End;

{ vm.throw }
Procedure _throw(M: TMachine);
Var msg: String;
Begin
With M do
Begin
 msg := StackPop.getString;

 if (exception_handler = 0) Then
  raise eThrow.Create(msg) Else
  Begin
   last_exception := msg;

   setPosition(exception_handler);
  End;
End;
End;

{ vm.get_last_exception }
Procedure _get_last_exception(M: TMachine);
Begin
 With M do
  StackPush(last_exception);
End;

initialization
 NewFunction('vm', 'exit', @_exit);
 NewFunction('vm', 'save_exception_handler', @_save_exception_handler);
 NewFunction('vm', 'restore_exception_handler', @_restore_exception_handler);
 NewFunction('vm', 'set_exception_handler', @_set_exception_handler);
 NewFunction('vm', 'get_exception_handler', @_get_exception_handler);
 NewFunction('vm', 'throw', @_throw);
 NewFunction('vm', 'get_last_exception', @_get_last_exception);
End.
