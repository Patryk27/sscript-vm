(*
 SScript Virtual Machine
 Copyright © by Patryk Wychowaniec, 2013-2014

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
Program SScriptVM;
Uses vm_header, SysUtils, Classes, mInput, mOutput, mMath, mTime, os_functions;
Type EBogusException = Class(Exception);
Var opt_logo, opt_wait, opt_time, opt_jit: Boolean;

    LogMode: TLogMode = lmDisabled;
    LogFile: String = 'vm.log';

    JITSaveTo: String;

    GCMemorySize: uint32;

    VM  : Pointer;
    Time: uint32;

(* RaiseError *)
Procedure RaiseError(const Format: String; const Args: Array of Const);
Begin
 raise EBogusException.CreateFmt(Format, Args);
End;

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
   // -w, -wait
   '-w', '-wait':
   Begin
    opt_wait := True;
   End;

   // -t, -time
   '-t', '-time':
   Begin
    opt_time := True;
   End;

   // -log
   '-log':
   Begin
    Inc(Pos);

    Case ParamStr(Pos) of
     'disabled': LogMode := lmDisabled;
     'console' : LogMode := lmConsole;
     'file'    : LogMode := lmFile;

     else
      RaiseError('Unknown ''-log'' switch value: %s', [ParamStr(Pos)]);
    End;
   End;

   // -log_file
   '-log_file':
   Begin
    Inc(Pos);
    LogFile := ParamStr(Pos);
    LogMode := lmFile;
   End;

   // -jit
   '-jit':
   Begin
    opt_jit := True;
   End;

   // -jit_save
   '-jit_save':
   Begin
    Inc(Pos);
    JITSaveTo := ParamStr(Pos);
   End;

   // -gc
   '-gc':
   Begin
    Inc(Pos);
    Param := ParamStr(Pos);

    if (Length(Param) = 0) Then
     RaiseError('Empty command-line switch value (usage: -gc <size>)', []);

    Case Param[Length(Param)] of
     'm', 'M': Mult := 1024*1024;
     'g', 'G': Mult := 1024*1024*1024;

     else
      Mult := 1;
    End;

    if (Param[Length(Param)] in ['m', 'M', 'g', 'G']) Then
     Delete(Param, Length(Param), 1);

    Try
     GCMemorySize := StrToInt(Param) * Mult;
    Except
     RaiseError('Invalid command-line argument: -gc %s (usage: -gc <size>)', [Param]);
    End;
   End;

   // unknown switch
   else
    RaiseError('Unknown command-line switch: %s', [Param]);
  End;

  Inc(Pos);
 End;
End;

(* DisplayHeader *)
Procedure DisplayHeader;
Begin
 Writeln('SScript Virtual Machine (ssvm.dll version: ', SSGetVMVersion, ')');
End;

(* DisplayHelp *)
Procedure DisplayHelp;
Begin
 Writeln('Usage:');
 Writeln('vm.exe [input file] <options>');
 Writeln;

 Writeln('Available options:');
 Writeln;

 Writeln('Primary:');
 Writeln('  -w(ait)  waits for `enter` when finished');
 Writeln('  -t(ime)  displays program''s execution time');
 Writeln;

 Writeln('Logging:');
 Writeln('  -log <mode>       changes VM logging mode (disabled/console/file)');
 Writeln('  -log_file <file>  changes log file name (default: vm.log) and enables file logging mode');
 Writeln;

 Writeln('JIT compiler:');
 Writeln('  -jit              enables just-in-time compilation');
 Writeln('  -jit_save <file>  saves compiled JIT code to the specified file');
 Writeln;

 Writeln('Garbage collector:');
 Writeln('  -gc <size>  changes garbage collector''s memory limit; eg.`-gc 512M` will set it to 512 megabytes. Available switches: G (GB), M (MB) - otherwise bytes are used. Default: 256 MB. Better not to set it below 64 MB.');
End;

(* Prepare *)
Function Prepare: Boolean;
Var Struct: TVMDataStructure;
Begin
 Struct.FileName  := PChar(ParamStr(1));
 Struct.GCMemSize := GCMemorySize;

 Struct.LogMode := LogMode;
 Struct.LogFile := PChar(LogFile);

 VM := SSCreateAndLoad(Struct); // load program

 if (VM = nil) Then
  RaiseError('Couldn''t load specified program file: %s', [ParamStr(1)]);

 mInput.Init(VM);
 mOutput.Init(VM);
 mMath.Init(VM);
 mTime.Init(VM);

 Exit(True);
End;

(* RunJIT *)
Function RunJIT: Boolean;
Var JITStream: TMemoryStream;
    JITCode  : Pointer;
    I        : uint32;
Begin
 Result := False;

 // try to compile
 if (not SSJITCompile(VM)) Then
  RaiseError('JIT compiling failed!%s%s', [sLineBreak, SSGetJITError(VM)]);

 // save JIT output?
 if (Length(JITSaveTo) > 0) Then
 Begin
  JITStream := TMemoryStream.Create;
  Try
   JITCode := SSGetJITCodeData(VM);
   For I := 0 To SSGetJITCodeSize(VM)-1 Do
    JITStream.WriteByte(PByte(JITCode+I)^);
   JITStream.SaveToFile(JITSaveTo);
  Finally
   JITStream.Free;
  End;
 End;
End;

// main program block
Label VMEnd;
Begin
 Try
  opt_logo := False;
  opt_wait := False;
  opt_time := False;
  opt_jit  := False;

  DefaultFormatSettings.DecimalSeparator := '.';

  Time := GetMilliseconds;

  if (ParamCount < 1) Then // error: too few parameters
  Begin
   DisplayHeader;
   Writeln;
   DisplayHelp;
   Exit;
  End;

  { parse command line }
  ParseCommandLine;

  if (GCMemorySize = 0) Then // if zero, set it to the default value
   GCMemorySize := 256*1024*1024;

  if (opt_logo) Then // if '-logo' parameter passed
  Begin
   DisplayHeader;
   Exit;
  End;

  { prepare VM }
  if (not Prepare) Then
  Begin
   goto VMEnd;
  End;

  { if specified, run JIT compiler }
  if (opt_jit) and (not RunJIT) Then
  Begin
   goto VMEnd;
  End;

  if (VM = nil) Then
  Begin
   goto VMEnd;
  End;

  { execute VM }
  SSExecuteVM(VM);

  if (SSGetStopReason(VM) = srException) Then
  Begin
   Writeln('Virtual machine threw an exception:');
   Writeln(PChar(SSGetException(VM).Data));
  End;

  { free VM }
  SSFreeVM(VM);

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
 Except
  On E: EBogusException Do
  Begin
   Writeln(E.Message);

   if (opt_wait) Then
    Readln;

   Halt;
  End;

  On E: Exception Do
  Begin
   Writeln('Unexpected exception was raised:');
   Writeln(E.Message);
  End;
 End;
End.
