{$H+}
Unit mOutput;

 Interface

 Procedure strdisplay(Str: String);

 Implementation
Uses CRT, Windows, Machine, Opcodes, SysUtils, FileUtil;
Var isBuffered  : Boolean=False;
    BufferString: String = '';

Procedure SetScreenSize(Width, Height: Integer);
Var Rect : TSmallRect;
    Coord: TCoord;
Begin
 Rect.Left   := 1;
 Rect.Top    := 1;
 Rect.Right  := Width;
 Rect.Bottom := Height;
 Coord.X     := Rect.Right+1-Rect.Left;
 Coord.Y     := Rect.Bottom+1-Rect.Top;

 SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
 SetConsoleWindowInfo(GetStdHandle(STD_OUTPUT_HANDLE), True, Rect);

 Window32(1, 1, Width, Height);
 CRT.WindMaxX := Width;
 CRT.WindMaxY := Height;
End;

Procedure strdisplay(Str: String);
Var Ch: Char;
Begin
 For Ch in Str Do
  if (Ch = #10) Then
   Writeln Else
   Write(UTF8ToConsole(AnsiToUTF8(Ch)));
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
  BufferString += Str Else
  strdisplay(Str);
End;
End;

{ output.set_size }
Procedure _set_size(M: TMachine);
Var W, H: Integer;
Begin
With M do
Begin
 W := StackPop.getInt;
 H := StackPop.getInt;

 SetScreenSize(W, H);
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
 strdisplay(BufferString);
 BufferString := '';
End;

initialization
 NewFunction('output', 'print', @_print);
 NewFunction('output', 'set_size', @_set_size);
 NewFunction('output', 'set_buffered', @_set_buffered);
 NewFunction('output', 'flush', @_flush);
End.
