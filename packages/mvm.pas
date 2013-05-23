(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mVM;

 Interface
 Uses Machine;

 Procedure Init(M: TMachine);

 Implementation
Uses SysUtils;

{ vm.exit }
Procedure _exit(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 M.ExitCode := Params[0].Value;

 raise Exception.Create('');
End;

{ vm.save_exception_state }
Procedure _save_exception_state(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 With M do
 Begin
  ExceptionStack^ := StackPos^; // save stack position
  Inc(ExceptionStack);

  ExceptionStack^ := ExceptionHandler; // save previous handler
  Inc(ExceptionStack);
 End;
End;

{ vm.restore_exception_state }
Procedure _restore_exception_state(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 With M do
 Begin
  Dec(ExceptionStack);
  ExceptionHandler := ExceptionStack^; // restore previous handler

  Dec(ExceptionStack);
  StackPos^ := ExceptionStack^; // restore stack position
 End;
End;

{ vm.set_exception_handler }
Procedure _set_exception_handler(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 M.ExceptionHandler := Params[0].Value;
End;

{ vm.get_exception_handler }
Procedure _get_exception_handler(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpInt;
 Result.Value := M.ExceptionHandler;
End;

{ vm.throw }
Procedure _throw(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 M.ThrowException(Params[0].Value);
End;

{ vm.get_last_exception }
Procedure _get_last_exception(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 Result.Typ   := cpString;
 Result.Value := M.LastException;
End;

// -------------------------------------------------------------------------- //
Procedure Init(M: TMachine);
Begin
 M.AddInternalCall('vm', 'exit', 1, @_exit);
 M.AddInternalCall('vm', 'save_exception_state', 0, @_save_exception_state);
 M.AddInternalCall('vm', 'restore_exception_state', 0, @_restore_exception_state);
 M.AddInternalCall('vm', 'set_exception_handler', 1, @_set_exception_handler);
 M.AddInternalCall('vm', 'get_exception_handler', 0, @_get_exception_handler);
 M.AddInternalCall('vm', 'throw', 1, @_throw);
 M.AddInternalCall('vm', 'get_last_exception', 0, @_get_last_exception);
End;
End.
