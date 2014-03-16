(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

// ... interpreter.pas

 { vm.exit }
 if (AnsiCompareStr(Name, 'vm.exit') = 0) Then
 Begin
  // ExitCode := getInt(StackPop);
  getInt(Stack.Pop);

  Stop := True;
 End Else

 { vm.save_exception_state }
 if (AnsiCompareStr(Name, 'vm.save_exception_state') = 0) Then
 Begin
  ExceptionStack[ExceptionStackID] := StackPos^; // save stack position
  Inc(ExceptionStackID);

  ExceptionStack[ExceptionStackID] := ExceptionHandler; // save previous handler
  Inc(ExceptionStackID);
 End Else

 { vm.restore_exception_state }
 if (AnsiCompareStr(Name, 'vm.restore_exception_state') = 0) Then
 Begin
  if (ExceptionStackID < 2) Then
   ThrowException('Cannot restore exception state: exception stack is corrupted.');

  Dec(ExceptionStackID);
  ExceptionHandler := ExceptionStack[ExceptionStackID]; // restore previous handler

  Dec(ExceptionStackID);
  StackPos^ := ExceptionStack[ExceptionStackID]; // restore stack position
 End Else

 { vm.set_exception_handler }
 if (AnsiCompareStr(Name, 'vm.set_exception_handler') = 0) Then
 Begin
  ExceptionHandler := getInt(Stack.Pop);
 End Else

 { vm.get_exception_handler }
 if (AnsiCompareStr(Name, 'vm.get_exception_handler') = 0) Then
 Begin
  Param.Typ       := mvInt;
  Param.Value.Int := ExceptionHandler;
  Stack.Push(Param);
 End Else

 { vm.throw }
 if (AnsiCompareStr(Name, 'vm.throw') = 0) Then
 Begin
  ThrowException(getPChar(Stack.Pop, True));
 End Else

 { vm.get_last_exception }
 if (AnsiCompareStr(Name, 'vm.get_last_exception') = 0) Then
 Begin
  if (LatestException.Typ <> etByMessage) Then
   ThrowException('VM internal error: interpreter_icalls.pas -> LatestException.Typ <> etByMessage');

  Param.Typ       := mvString;
  Param.Value.Str := VMStringList.StringToVMString(PChar(LatestException.Data));
  Stack.Push(Param);
 End Else

 { vm.version }
 if (AnsiCompareStr(Name, 'vm.version') = 0) Then
 Begin
  Param.Typ       := mvString;
  Param.Value.Str := VMStringList.StringToVMString(VMVersion);
  Stack.Push(Param);
 End Else

 { invalid VM call }
  VM^.ThrowException(PVMChar('Invalid VM-icall: '+Name));
