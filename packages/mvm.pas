(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mVM;

 Interface

 Implementation
Uses Machine, Exceptions, SysUtils;

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

{ vm.save_exception_state }
Procedure _save_exception_state(M: TMachine);
Begin
With M do
Begin
 ExceptionStack^ := StackPos^; // save stack position
 Inc(ExceptionStack);

 ExceptionStack^ := exception_handler; // save previous handler
 Inc(ExceptionStack);
End;
End;

{ vm.restore_exception_state }
Procedure _restore_exception_state(M: TMachine);
Begin
With M do
Begin
 Dec(ExceptionStack);
 exception_handler := ExceptionStack^; // restore previous handler

 Dec(ExceptionStack);
 StackPos^ := ExceptionStack^; // restore stack position
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
 NewFunction('vm', 'save_exception_state', @_save_exception_state);
 NewFunction('vm', 'restore_exception_state', @_restore_exception_state);
 NewFunction('vm', 'set_exception_handler', @_set_exception_handler);
 NewFunction('vm', 'get_exception_handler', @_get_exception_handler);
 NewFunction('vm', 'throw', @_throw);
 NewFunction('vm', 'get_last_exception', @_get_last_exception);
End.
