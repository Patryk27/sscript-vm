(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mOutput;

 Interface
 Uses os_functions;

 Procedure strdisplay(Str: String);

 Implementation
Uses
{$IFDEF WINDOWS}
 Windows,
{$ENDIF} CRT, Machine, Opcodes, SysUtils, FileUtil;

Var isBuffered: Boolean = False;
    BufferData: String = '';

// strdisplay
Procedure strdisplay(Str: String); inline;
Begin
 Write(UTF8ToConsole(Str));
End;

{ output.print }
Procedure _print(M: TMachine);
Var P  : TOpParam;
    Str: String;
Begin
With M do
Begin
 P := StackPop;

 With P do
 Case Typ of
  ptBool, ptBoolReg:
  if (Val = 0) Then
   Str := 'false' Else
   Str := 'true';

  ptChar, ptCharReg    : Str := chr(Val);
  ptInt, ptIntReg      : Str := IntToStr(Val);
  ptFloat, ptFloatReg  : Str := FloatToStr(fVal);
  ptString, ptStringReg: Str := sVal;
 End;

 if (isBuffered) Then
  BufferData += Str Else
  strdisplay(Str);
End;
End;

{ output.clear }
Procedure _clear(M: TMachine);
Begin
 if (isBuffered) Then
  BufferData := '' Else
  ClrScr;
End;

{ output.set_size }
Procedure _set_size(M: TMachine);
Var W, H, cW, cH: Integer;
Begin
With M do
Begin
 W  := StackPop.getInt;
 H  := StackPop.getInt;
 cW := StackPop.getInt;
 cH := StackPop.getInt;

 SetConsoleSize(W, H, cW, cH);
End;
End;

{ output.set_buffered }
Procedure _set_buffered(M: TMachine);
Begin
 isBuffered := M.StackPop.getBool;
End;

{ output.flush }
Procedure _flush(M: TMachine);
Begin
 strdisplay(BufferData);
 BufferData := '';
End;

{ output.cursor.hide }
Procedure _cursor_hide(M: TMachine);
Begin
 CRT.CursorOff;
End;

{ output.cursor.show }
Procedure _cursor_show(M: TMachine);
Begin
 CRT.CursorOn;
End;

initialization
 NewFunction('output', 'print', @_print);
 NewFunction('output', 'clear', @_clear);
 NewFunction('output', 'set_size', @_set_size);
 NewFunction('output', 'set_buffered', @_set_buffered);
 NewFunction('output', 'flush', @_flush);
 NewFunction('output', 'cursor.hide', @_cursor_hide);
 NewFunction('output', 'cursor.show', @_cursor_show);
End.
