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
Var opt_wait, opt_time: Boolean;
    GCMemoryLimit     : uint32;

(* ParseCommandLine *)
Procedure ParseCommandLine;
Var Pos  : uint16 = 2;
    Param: String;
    Mult : Integer;
Begin
 While (Pos <= ParamCount) Do
 Begin
  Param := ParamStr(Pos);

  Case Param of
   '-wait': opt_wait := True;
   '-time': opt_time := True;

   '-gc':
   Begin
    Inc(Pos);
    Param := ParamStr(Pos);

    if (Length(Param) = 0) Then
     Param := '256M';

    Case Param[Length(Param)] of
     'm', 'M': Mult := 1024*1024;
     'g', 'G': Mult := 1024*1024*1024;
     else
      Mult := 1;
    End;

    if (Param[Length(Param)] in ['m', 'M', 'g', 'G']) Then
     Delete(Param, Length(Param), 1);

    GCMemoryLimit := StrToInt(Param) * Mult;
   End;

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

  Writeln('-wait        wait for `enter` when finished');
  Writeln('-time        display program''s execution time');
  Writeln('-gc <value>  change garbage collector''s memory limit, eg.`-gc 512M` will set it to 512 megabytes. Switches: G (GB), M (MB), otherwise bytes are used. Default: 256 MB. Don''t set it below 64 MB.');

  Exit;
 End;

 ParseCommandLine; // parse command line

 if (GCMemoryLimit = 0) Then
  GCMemoryLimit := 256*1024*1024;

 if (ParamStr(1) = '-logo') Then
 Begin
  WRiteln('SScript Virtual Machine ', getVersion);
 End Else
 Begin
  VM := LoadProgram(PChar(ParamStr(1)), GCMemoryLimit); // load program

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
     Writeln('VM Exception! ', E.Message);
   End;
  Finally
   if (VM <> nil) and (getStopReason(VM) = srException) Then
   Begin
    Writeln('Exception has been thrown:');
    Writeln;
    Writeln(PChar(getException(VM).Data));
   End;

   if (VM <> nil) Then
    Free(VM);

   Time := GetMilliseconds-Time;
  End;
 End;

 { -time }
 if (opt_time) Then
 Begin
  Writeln;
  Writeln('------------------------');
  Writeln('Execution time: ', Time, 'ms');
 End;

 { -wait }
 if (opt_wait) Then
 Begin
  Writeln;
  Writeln('-- END --');
  Readln;
 End;
End.
