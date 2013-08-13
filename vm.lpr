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
Uses vm_header, SysUtils, Classes, mInput, mOutput, mMath, mTime, os_functions;
Var opt_wait, opt_time, opt_jit, opt_verbose: Boolean;
    GCMemoryLimit                           : uint32;
    JITSaveTo                               : String;

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
   '-v'   : opt_verbose := True;
   '-wait': opt_wait := True;
   '-time': opt_time := True;
   '-jit' : opt_jit  := True;

   '-jit_save':
   Begin
    Inc(Pos);
    JITSaveTo := ParamStr(Pos);
   End;

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
Label VMEnd;
Var VM       : Pointer;
    Time     : int64;
    JITState : TJITCompiledState;
    JITStream: TMemoryStream;
    JITCode  : Pointer;
    I        : Integer;
Begin
 Time := GetMilliseconds;

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

  Writeln('-v                enable verbose mode');
  Writeln('-wait             wait for `enter` when finished');
  Writeln('-time             display program''s execution time');
  Writeln('-jit              enable just-in-time compilation (**very** experimental!)');
  Writeln('-jit_save <file>  save compiled JIT code to the specified file');
  Writeln('-gc <value>       change garbage collector''s memory limit, eg.`-gc 512M` will set it to 512 megabytes. Switches: G (GB), M (MB), otherwise bytes are used. Default: 256 MB. Don''t set it below 64 MB.');

  Exit;
 End;

 ParseCommandLine; // parse command line

 if (GCMemoryLimit = 0) Then
  GCMemoryLimit := 256*1024*1024;

 if (ParamStr(1) = '-logo') Then
 Begin
  Writeln('SScript Virtual Machine ', getVersion);
 End Else
 Begin
  if (opt_verbose) Then
   Writeln('-> Loading bytecode from ''', ParamStr(1), '''');

  VM := LoadProgram(PChar(ParamStr(1)), GCMemoryLimit); // load program

  if (VM = nil) Then
  Begin
   Writeln('LoadProgam() falied!');
   Writeln('#', GetErrorID, ': ', GetErrorMsg);
   goto VMEnd;
  End;

  if (opt_verbose) Then
   Writeln('-> Setting internal calls');

  mInput.Init(VM);
  mOutput.Init(VM);
  mMath.Init(VM);
  mTime.Init(VM);

  // run JIT compiler?
  if (opt_jit) Then
  Begin
   if (opt_verbose) Then
    Writeln('-> Running JIT compiler...');

   JITState := JITCompile(VM);

   if (JITState <> csDone) Then
   Begin
    Writeln('JITCompile() failed!');
    Writeln(JITState, ' -> ', GetLastJITError(VM));
    goto VMEnd;
   End;

   if (opt_verbose) Then
    Writeln('-> Bytecode has been compiled! JIT compiled size: ', GetJITCodeSize(VM), ' bytes');
  End;

  // save JIT output?
  if (opt_jit) and (Length(JITSaveTo) > 0) Then
  Begin
   if (opt_verbose) Then
    Writeln('-> Saving JIT code to ''', JITSaveTo, '''');

   JITStream := TMemoryStream.Create;
   Try
    JITCode := GetJITCode(VM);
    For I := 0 To GetJITCodeSize(VM)-1 Do
     JITStream.WriteByte(PByte(JITCode+I)^);
    JITStream.SaveToFile(JITSaveTo);
   Finally
    JITStream.Free;
   End;
  End;

  if (opt_verbose) Then
   Writeln('-> Running program...');

  Run(VM);

  if (opt_verbose) Then
   Writeln('-> Program stopped/finished executing!');

  if (VM <> nil) and (getStopReason(VM) = srException) Then
  Begin
   Writeln('Virtual machine exception has been thrown:');
   Writeln;
   Writeln(PChar(getException(VM).Data));
  End;

  if (VM <> nil) Then
   Free(VM);
 End;

VMEnd:
 Time := GetMilliseconds-Time;

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
