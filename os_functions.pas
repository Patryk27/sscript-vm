(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 OS-dependent functions
*)
{$MACRO ON}

{$DEFINE OS_WINDOWS:=0}
{$DEFINE OS_LINUX:=0}

{$IFDEF WINDOWS} // if Windows
 {$DEFINE OS_WINDOWS:=1}
{$ENDIF}

{$IFDEF LINUX} // if Linux
 {$DEFINE OS_LINUX:=1}
{$ENDIF}

Unit os_functions;

 Interface
 Uses
{$IF OS_WINDOWS}
 Windows
{$ELSEIF OS_LINUX}
 Linux, BaseUnix, UnixType
{$ELSE}
 {$FATAL Unknown target operating system!}
{$ENDIF};

 Function GetTickCount: LongWord;
 Procedure SetConsoleSize(const Width, Height, WinWidth, WinHeight: Integer);

 Implementation

{ GetTickCount }
Function GetTickCount: LongWord;
{$IF OS_LINUX}
 Var T: timespec;
{$ENDIF}
Begin
 {$IF OS_WINDOWS} { Windows }
  Result := Windows.GetTickCount;
 {$ELSEIF OS_LINUX} { Linux }
  clock_gettime(CLOCK_MONOTONIC, @T);
  Result := T.tv_nsec;
 {$ELSE} { other }
  {$WARNING GetTickCount -> unimplemented!}
  Exit(0);
 {$ENDIF}
End;

{ SetConsoleSize }
Procedure SetConsoleSize(const Width, Height, WinWidth, WinHeight: Integer);
{$IF OS_WINDOWS}
Var Rect : TSmallRect;
    Coord: TCoord;
Begin
 Rect.Left   := 1; // don't change
 Rect.Top    := 1; // don't change
 Rect.Right  := Width;
 Rect.Bottom := Height;
 Coord.X     := WinWidth;
 Coord.Y     := WinHeight;

 SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
 SetConsoleWindowInfo(GetStdHandle(STD_OUTPUT_HANDLE), True, Rect);

 Window32(1, 1, WinWidth, WinHeight);
 CRT.WindMaxX := WinWidth;
 CRT.WindMaxY := WinHeight;
End;
{$ELSE} { other }
Begin
 {$WARNING SetConsoleSize -> unimplemented!}
End;
{$ENDIF}

End.
