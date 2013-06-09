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
{$H+}
Program VM_;
Uses vm_header, SysUtils, mInput, mOutput, mMath, mString, mTime, os_functions;
Var _err, _wait, _time: Boolean;

// ParseCommandLine
Procedure ParseCommandLine;
Var Pos  : uint16 = 2;
    Param: String;
Begin
 While (Pos <= ParamCount) Do
 Begin
  Param := ParamStr(Pos);

  Case Param of
   '-err' : _err  := True;
   '-wait': _wait := True;
   '-time': _time := True;
   else
    Writeln('Unknown command-line switch: ', Param);
  End;

  Inc(Pos);
 End;
End;

// main program block
Var VM  : Pointer;
    Time: int64;
Begin
 DecimalSeparator := '.';
 if (ParamCount < 1) Then
 Begin
  Writeln('SScript Virtual Machine ', getVersion);
  Writeln;

  Writeln('Usage:');
  Writeln('vm.exe [input file] <options>');
  Writeln;

  Writeln('Available options:');
  Writeln;

 // Writeln('-v     enable verbose mode');
 // Writeln('-err   detailed error log');
  Writeln('-wait  wait for `enter` when finished');
  Writeln('-time  display program''s execution time');
  Exit;
 End;

 ParseCommandLine; // parse command line

 VM := LoadProgram(PChar(ParamStr(1))); // load program

 Try
  if (VM = nil) Then
  Begin
   Writeln('LoadProgam() falied!');
   Writeln('#', GetErrorID, ': ', GetErrorMsg);
   Exit;
  End;

  mInput.Init(VM);
  mOutput.Init(VM);
  mMath.Init(VM);
  mString.Init(VM);
  mTime.Init(VM);

  Try
   Time := GetMilliseconds;
   Run(VM);
  Except
   On E: Exception Do
    Writeln('Exception! ', E.Message);
  End;
 Finally
  FreeVM(VM);
  Time := GetMilliseconds-Time;

  Writeln;
  Writeln('-- END --');

  { -time }
  if (_time) Then
   Writeln('Execution time: ', Time, 'ms');

  { -wait }
  if (_wait) Then
   Readln;
 End;
End.
