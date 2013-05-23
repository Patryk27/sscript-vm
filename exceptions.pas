Unit Exceptions;

 Interface
 Uses SysUtils;

 { VM exceptions }
 Type eInternalError  = Class(Exception);
      eInvalidCasting = Class(Exception);
      eInvalidFile    = Class(Exception);
      eInvalidOpcode  = Class(Exception);
      eInvalidCall    = Class(Exception);

      eThrow = Class(Exception);

 Procedure DumpExceptionInfo(const MachinePnt: Pointer);

 Implementation
Uses Machine, Opcodes;

Procedure DumpExceptionInfo(const MachinePnt: Pointer);
Var Machine: TMachine absolute MachinePnt;

  { -- stacktrace -- }
  Procedure Stacktrace;
  Var eLine       : Integer;
      eFile, eFunc: String;
  Begin
   With Machine do
   Begin
    FetchLocation(LongWord(CurrentOpcode)-LongWord(CodeData), eLine, eFile, eFunc);

    if (eLine = -1) Then
     Writeln('   at ', eFile, ' (unknown source)') Else
     Writeln('   in "', eFile, '", line ', eLine, ', inside "', eFunc, '"');

    // @TODO: 10 references max
    if (StackPos <> nil) Then
     While (StackPos^ > 0) Do
     Begin
      if (Stack[StackPos^].Typ = ptCallstackRef) Then
      Begin
       FetchLocation(Stack[StackPos^].getReference, eLine, eFile, eFunc);

       if (eLine = -1) Then
        Writeln('   ... from ', eFile, ' (unknown source)') Else
        Writeln('   ... from "', eFile, '", line ', eLine, ', inside "', eFunc, '"');
      End;

      Dec(StackPos^);
     End;
   End;
  End;

Begin
 With Machine do
 Begin
  if (CodeData = nil) Then // no code has been loaded
  Begin
   Writeln('No data available.');
   Exit;
  End;

  { -- stacktrace -- }
  Writeln;
  Writeln('Stacktrace:');
  Stacktrace;
 End;
End;

End.
