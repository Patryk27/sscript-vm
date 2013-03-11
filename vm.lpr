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
{$R *.res}
Program vm;
Uses CRT, Windows, SysUtils, Registry, Machine, mOutput;
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

Const Ext = 'ssc';
Var M   : TMachine;
    Time: Cardinal;
    Reg : TRegistry;
Label Finish;
Begin
 DefaultFormatSettings.DecimalSeparator := '.';
 Time := GetTickCount;

 mOutput.SetScreenSize(CRT.WindMaxX, CRT.WindMaxY, CRT.WindMaxX, CRT.WindMaxY+1000);

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
  Writeln('To enable a boolean switch use `name+` (or just `name`), to disable `name-`');
  Writeln;

  Writeln('name = description');
  Writeln;

 // Writeln('-force  load file even when eg.header is not correct (use carefully)');
  Writeln('-v      enable verbose mode');
  Writeln('-err    detailed error log');
 // Writeln('-debug  run with debugger');
  Writeln('-wait   wait for `enter` when finished (-)');
  Writeln('-time   display program''s execution time');

  Writeln('-setdblclick     enable running SScript compiled (*.ssc) programs by double-click on them');
  Writeln('-removedblclick  remove option above');
 End Else
 Begin
  Try
  Try
  Try
   Reg         := TRegistry.Create;
   Reg.RootKey := HKEY_CLASSES_ROOT;

   Case ParamStr(1) of
    '-setdblclick':
    Begin
     Reg.OpenKey('.'+Ext, True);
     Reg.WriteString('', Ext+'file');
     Reg.CloseKey;
     Reg.OpenKey(Ext+'file', True);
     Reg.WriteString('', 'Program MLang');
     Reg.CloseKey;
     Reg.OpenKey(Ext+'file\DefaultIcon', True);
     Reg.WriteString('', ParamStr(0)+',0');
     Reg.CloseKey;
     Reg.OpenKey(Ext+'file\shell\open', True);
     Reg.WriteString('', '&Otwórz w MLang Editor');
     Reg.CloseKey;
     Reg.OpenKey(Ext+'file\shell\open\command', True);
     Reg.WriteString('', ParamStr(0)+' "%1"');

     Writeln('Done!');
     Exit;
    End;

    '-removedblclick':
    Begin
     Reg.DeleteKey('.'+Ext);
     Reg.DeleteKey(Ext+'file');
     Reg.DeleteKey(Ext+'file\DefaultIcon');
     Reg.DeleteKey(Ext+'file\shell\open');
     Reg.DeleteKey(Ext+'file\shell\open\command');

     Writeln('Done!');
     Exit;
    End;
   End;
   Reg.Free;

   Machine.VerboseMode := getBoolOption('v', False);

   M := TMachine.Create(ParamStr(1));
   M.Prepare;
   Time := GetTickCount;
   M.Run;
  Except
   On E: Exception Do
    if (E.Message <> '') Then
    Begin
     Writeln(E.ClassName, ': ', E.Message);

     if (Assigned(M)) Then
     Begin
      {
       at FileName.ss (line: 97)
       ... from AnotherFile.ss (line: 32)
       ... from main.ss (line: 13)
      }
      Writeln('-> at 0x', IntToHex(M.LastOpcodePos, 8), ' :: ', M.disasm(M.LastOpcodePos)); //, ' (', M.FetchLabelName(M.getPosition));

      {
      I := M.CallstackPos;

      For I := M.CallstackPos Downto Integer(M.CallstackPos)-10 Do
       if (I >= 0) Then
        Writeln('--> from 0x', IntToHex(M.Callstack[I], 8));

      if (M.CallstackPos > 10) Then
       Writeln('--> ... ', M.CallstackPos-10, ' more');}
      Writeln('< callstack unavailable >');

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
