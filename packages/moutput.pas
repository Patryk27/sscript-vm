(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mOutput;

 Interface
 Uses Machine, os_functions;

 Procedure strdisplay(const Str: String);
 Procedure Init(M: TMachine);

 Implementation
Uses CRT, Variants, SysUtils, FileUtil, Opcodes;

Var isBuffered: Boolean = False;
    BufferData: String = '';

// strdisplay
Procedure strdisplay(const Str: String); inline;
Begin
 Write(UTF8ToConsole(Str));
End;

{ output.print }
Procedure _print(M: TMachine; Params: TCallValues; var Result: TCallValue);
Var Str: String;
Begin
 With M do
 Begin
  Str := VarToStr(Params[0].Value);

  if (isBuffered) Then
   BufferData += Str Else
   strdisplay(Str);
 End;
End;

{ output.clear }
Procedure _clear(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 if (isBuffered) Then
  BufferData := '' Else
  ClrScr;
End;

{ output.set_size }
Procedure _set_size(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 With M do
  SetConsoleSize(Params[0].Value, Params[1].Value, Params[2].Value, Params[3].Value);
End;

{ output.set_buffered }
Procedure _set_buffered(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 isBuffered := M.StackPop.getBool;
End;

{ output.flush }
Procedure _flush(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 strdisplay(BufferData);
 BufferData := '';
End;

{ output.cursor.hide }
Procedure _cursor_hide(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 CRT.CursorOff;
End;

{ output.cursor.show }
Procedure _cursor_show(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 CRT.CursorOn;
End;

// -------------------------------------------------------------------------- //
Procedure Init(M: TMachine);
Begin
 M.AddInternalCall('output', 'print', 1, @_print);
 M.AddInternalCall('output', 'clear', 0, @_clear);
 M.AddInternalCall('output', 'set_size', 4, @_set_size);
 M.AddInternalCall('output', 'set_buffered', 1, @_set_buffered);
 M.AddInternalCall('output', 'flush', 0, @_flush);
 M.AddInternalCall('output', 'cursor_hide', 0, @_cursor_hide);
 M.AddInternalCall('output', 'cursor_show', 0, @_cursor_show);
End;
End.
