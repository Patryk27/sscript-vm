(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit mOutput;

 Interface
 Uses vm_header, os_functions;

 Procedure strdisplay(const Str: String);
 Procedure Init(VM: Pointer);

 Implementation
Uses CRT, Variants, SysUtils, FileUtil;

Var isBuffered: Boolean = False;
    BufferData: String = '';

// strdisplay
Procedure strdisplay(const Str: String); inline;
Begin
 Write(UTF8ToConsole(Str));
End;

{ output.print }
Procedure _print(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Var Str: String;
Begin
 Case Params[0].Typ of
  mvBool     : Str := BoolToStr(Params[0].Value.Bool, 'true', 'false');
  mvChar     : Str := Params[0].Value.Char;
  mvInt      : Str := IntToStr(Params[0].Value.Int);
  mvFloat    : Str := FloatToStr(Params[0].Value.Float);
  mvString   : Str := Params[0].Value.Str;
  mvReference: Str := '<object: 0x'+IntToHex(Params[0].Value.Int, 8)+'>';
  else
   raise Exception.CreateFmt('Invalid "output.print" icall: unknown parameter type: %d', [ord(Params[0].Typ)]);
 End;

 if (isBuffered) Then
  BufferData += Str Else
  strdisplay(Str);
End;

{ output.clear }
Procedure _clear(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 if (isBuffered) Then
  BufferData := '' Else
  ClrScr;
End;

{ output.set_size }
Procedure _set_size(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 SetConsoleSize(getInt(Params[0]), getInt(Params[1]), getInt(Params[2]), getInt(Params[3]));
End;

{ output.set_buffered }
Procedure _set_buffered(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 isBuffered := getBool(Params[0]);
End;

{ output.flush }
Procedure _flush(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 strdisplay(BufferData);
 BufferData := '';
End;

{ output.cursor_hide }
Procedure _cursor_hide(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 CRT.CursorOff;
End;

{ output.cursor_show }
Procedure _cursor_show(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 CRT.CursorOn;
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 AddInternalCall(VM, 'output', 'print', 1, @_print);
 AddInternalCall(VM, 'output', 'clear', 0, @_clear);
 AddInternalCall(VM, 'output', 'set_size', 4, @_set_size);
 AddInternalCall(VM, 'output', 'set_buffered', 1, @_set_buffered);
 AddInternalCall(VM, 'output', 'flush', 0, @_flush);
 AddInternalCall(VM, 'output', 'cursor_hide', 0, @_cursor_hide);
 AddInternalCall(VM, 'output', 'cursor_show', 0, @_cursor_show);
End;
End.
