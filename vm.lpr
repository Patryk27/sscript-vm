(*
 SScript Virtual Machine
 Copyright © by Patryk Wychowaniec, 2013

 -------------------------------------------------------------------------------
 SScript Compiler is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation; either version 2.1 of the License, or
 (at your option) any later version.

 SScript Compiler is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with SScript Compiler; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA   
*)

// @TODO: better command-line parsing
// @TODO: opcode `dllcall`

{$R *.res}
Program ssvm;
Uses CRT, SysUtils,
     os_functions, Exceptions, Machine, Opcodes,
     mInput, mOutput, mString, mMath, mTime, mVM;

Const Version = '0.3.1 nightly';

{ getBoolOption }
Function getBoolOption(O: String; Default: Boolean): Boolean;
Var I: Integer;
Begin
 O := '-'+O;

 Result := Default;
 For I := 0 To ParamCount Do
  if (ParamStr(I) = O+'+') or (ParamStr(I) = O) Then
   Exit(True) Else
  if (ParamStr(I) = O+'-') Then
   Exit(False);
End;

{ DumpExceptionCallstack }
Procedure DumpExceptionCallstack;
Var I     : Integer;
    Frames: PPointer;
Begin
 Writeln(BackTraceStrFunc(ExceptAddr));
 Frames := ExceptFrames;
 For I := 0 To ExceptFrameCount-1 Do
  Writeln(BackTraceStrFunc(Frames[I]));
End;

// -------------------------------------------------------------------------- //
Var VM  : TMachine;
    Time: Cardinal;
Label Finish;
Begin
 DefaultFormatSettings.DecimalSeparator := '.';
 Time := GetMilliseconds;

 SetConsoleSize(CRT.WindMaxX, CRT.WindMaxY, CRT.WindMaxX, CRT.WindMaxY+1000);

 if (ParamCount < 1) or (ParamStr(1) = '-logo') Then
 Begin
  Writeln('SScript Virtual Machine, version '+Version+' [compiled '+{$I %DATE%}+']');
  Writeln('by Patryk Wychowaniec');

  if (ParamStr(1) = '-logo') Then
   goto Finish;
 End;

 if (ParamCount < 1) Then
 Begin
  Writeln('Usage:');
  Writeln('vm.exe [input file]');
  Writeln;
  Writeln('Available options:');
  Writeln('To enable switch use `name+` (or just `name`), to disable `name-`');
  Writeln;

  Writeln('name = description');
  Writeln;

  Writeln('-v     enable verbose mode');
  Writeln('-err   detailed error log');
  Writeln('-wait  wait for `enter` when finished');
  Writeln('-time  display program''s execution time');
 End Else
 Begin
  Try
  Try
   Machine.VerboseMode := getBoolOption('v', False);

   VM := TMachine.Create(ParamStr(1));

   mInput.Init(VM);
   mOutput.Init(VM);
   mString.Init(VM);
   mMath.Init(VM);
   mTime.Init(VM);
   mVM.Init(VM);

   Time := GetMilliseconds;
   VM.Run;
  Except
   On E: Exception Do
    if (E.Message <> '') Then
    Begin
     Write(E.ClassName, ': ', E.Message);

     if (Assigned(VM)) Then
     Begin
      Writeln;
      DumpExceptionInfo(VM);
     End;
    End;
  End;
  Except
   On E: Exception Do
   Begin
    Writeln;
    Writeln('Exception raised in exception handler:');
    Writeln(E.Message);
    Writeln;
    Writeln('VM stacktrace:');
    DumpExceptionCallstack;
   End;
  End;
 End;

 Time := GetMilliseconds-Time;

 if (getBoolOption('v', False) or getBoolOption('time', False)) Then
 Begin
  Writeln('-- END --');
  Writeln('Time   : '+IntToStr(Time)+' ms');

  if (Assigned(VM)) Then
  Begin
   Writeln('Opcodes: ', VM.ParsedOpcodes);

   if (Time = 0) Then
    Writeln('Opc/ms : ~', VM.ParsedOpcodes) Else
    Writeln('Opc/ms : ', IntToStr(Round(VM.ParsedOpcodes/Time)));
  End Else
  Begin
   Writeln('Opcodes: <unknown>');
   Writeln('Opc/ms : <unknown>');
  End;
 End;

Finish:
 if (getBoolOption('wait', False)) Then
  Readln;
End.
