{$MODE OBJFPC}
{$H+}
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

//{$R *.res}

Program vm;
Uses os_functions, CRT, SysUtils, Machine, Opcodes, mOutput;
Const Version = '0.3 nightly';

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

// @TODO: better command-line parsing

Var M   : TMachine;
    Time: Cardinal;

    eLine: Integer;
    eFile: String;

Label Finish;
Begin
 DefaultFormatSettings.DecimalSeparator := '.';
 Time := GetTickCount;

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
  Try
   Machine.VerboseMode := getBoolOption('v', False);

   M := TMachine.Create(ParamStr(1));
   M.Prepare;
   Time := GetTickCount;
   M.Run;
  Except
   On E: Exception Do
    if (E.Message <> '') Then
    Begin
     Write(E.ClassName, ': ', E.Message);

     if (Assigned(M)) Then
     Begin
      Writeln;

      M.FetchLineAndFile(M.LastOpcodePos, eLine, eFile);

      if (eLine = -1) Then
       Write('   at unknown source (', eFile, ')') Else
       Write('   at ', eFile, ' (line: ', eLine, ')');

      Writeln(' :: ', M.disasm(M.LastOpcodePos));

      With M do  // @TODO: 10 references max
       While (StackPos^ > 0) Do
       Begin
        if (Stack[StackPos^].Typ = ptCallstackRef) Then
        Begin
         M.FetchLineAndFile(Stack[StackPos^].getReference, eLine, eFile);

         if (eLine = -1) Then
          Writeln('   ... from unknown source (', eFile, ')') Else
          Writeln('   ... from ', eFile, ' (', eLine, ')');
        End;
        Dec(StackPos^);
       End;

     End;
    End;
  End;
  Finally
   //M.Free;
  End;
  Except
   On E: Exception Do
   Begin
    Writeln;
    Writeln('Critical fail:');
    Writeln(E.Message);
   End;
  End;
 End;

 Time := GetTickCount-Time;

 if (getBoolOption('v', False) or getBoolOption('time', False)) Then
 Begin
  Writeln('-- END --');
  Writeln('Time   : '+IntToStr(Time)+' ms');

  if (Assigned(M)) Then
  Begin
   Writeln('Opcodes: ', M.OpcodeNo);

   if (Time = 0) Then
    Writeln('Opc/ms : ~', M.OpcodeNo) Else
    Writeln('Opc/ms : ', IntToStr(Round(M.OpcodeNo/Time)));
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
