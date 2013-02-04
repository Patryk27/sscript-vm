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

Program vm;
Uses CRT, Windows, SysUtils, Machine, Registry;
Const Version = '0.2';

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

{ getStringOption }
Function getStringOption(O: String; Default: String): String;
Var I: Integer;
Begin
 O := '-'+O;

 Result := Default;
 For I := 0 To ParamCount-1 Do
  if (ParamStr(I) = O) Then
  Begin
   Result := ParamStr(I+1);
   Exit;
  End;
End;

{ getIntOption }
Function getIntOption(O: String; Default: Integer): Integer;
Begin
 Try
  Result := StrToInt(getStringOption(O, IntToStr(Default)));
 Except
  Result := Default;
 End;
End;

Const Ext = 'ssc';
Var M   : TMachine;
    I   : Integer;
    Time: Cardinal;
    Reg : TRegistry;

{$R *.res}

Begin
 DefaultFormatSettings.DecimalSeparator := '.';
 Time := GetTickCount;

 CRT.WindMaxY := -1; // range check error is a deliberate effect

 if (ParamCount < 1) Then
 Begin
  Writeln('SScript Virtual Machine v.', Version);

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

  Writeln('-setdblclick     run SScript compiled (*.ssc) programs by double-click on them');
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
   M.Run;
  Except
   On E: Exception Do
    if (E.Message <> '') Then
    Begin
     Writeln('VM Exception');
     Writeln(E.Message);

     if (getBoolOption('err', False)) and (Assigned(M)) Then // detailed error log
     Begin
      M.setPosition(M.LastOpcodePos);

      Writeln;
      Writeln('Position:');
      Writeln('-> CODE:0x', IntToHex(M.getPosition, 8));
      Writeln('-> FILE:0x', IntToHex(M.getPosition+M.SectionList[M.CodeSection].DataPnt, 8));
      Writeln;

      Writeln('Error opcode:');
      Writeln('> ', M.disasm(M.getPosition));
      Writeln;

      Writeln('Callstack (first 15):');
      For I := M.CallstackPos-15 To M.CallstackPos-1 Do
       if (I >= 0) Then
       Begin
        Write('-> 0x', IntToHex(M.Callstack[I], 8));

        // @TODO
       // M.Position := M.Callstack[I]+sizeof(Byte) - sizeof(LongWord)-2*sizeof(Byte); // go back 1 param and 1 opcode (i.e. - go back to the beginning of 'call (int value)')
       // if (M.FetchLabelName(M.read_param.Val, Str)) Then
       //  Write(' = call(:', Str, ')');

        Writeln;
       End;

      Writeln;
      Writeln('Registers:');
      With M do
      Begin
       For I := Low(breg) To High(breg) Do
        if (I = 5) Then
         Writeln('if  = ', breg[I]) Else
         Writeln('eb',I,' = ', breg[I]);

       For I := Low(creg) To High(creg) Do
        Writeln('ec',I,' = #', ord(creg[I]));

       For I := Low(ireg) To High(ireg) Do
        if (I = 5) Then
         Writeln('stp = ', ireg[5]) Else
         Writeln('ei',I,' = ', ireg[I]);

       For I := Low(freg) To High(freg) Do
        Writeln('ef',I,' = ', FloatToStr(freg[I]));

       For I := Low(sreg) To High(sreg) Do
        Writeln('es',I,' = ', sreg[I]);

       For I := Low(rreg) To High(rreg) Do
        Writeln('er',I,' = ', rreg[I]);
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
  Writeln('Opcodes: ', M.OpcodeNo);

  if (Time = 0) Then
   Writeln('Opc/ms : > ', M.OpcodeNo) Else
   Writeln('Opc/ms : ', IntToStr(Round(M.OpcodeNo/Time)));
 End;

 if (getBoolOption('wait', False)) Then
  Readln;
End.
