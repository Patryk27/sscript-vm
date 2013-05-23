(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mInput;

 Interface
 Uses Machine;

 Procedure Init(M: TMachine);

 Implementation
Uses CRT, mOutput;

{ input.keypressed }
Procedure _keypressed(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 M.StackPush(KeyPressed);
End;

{ input.getchar }
Procedure _getchar(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 M.StackPush(ReadKey);
End;

{ input.readchar }
Procedure _readchar(M: TMachine; Params: TCallValues; var Result: TCallValue);
Var Ch: Char;
Begin
 With M do
 Begin
  Ch := ReadKey;
  StackPush(Ch);
  mOutput.strdisplay(Ch);
 End;
End;

{ input.read }
Procedure _read(M: TMachine; Params: TCallValues; var Result: TCallValue);
Var Str: String;
Begin
 With M do
 Begin
  Readln(Str);
  StackPush(Str);
 End;
End;

// -------------------------------------------------------------------------- //
Procedure Init(M: TMachine);
Begin
 M.AddInternalCall('input', 'keypressed', 0, @_keypressed);
 M.AddInternalCall('input', 'getchar', 0, @_getchar);
 M.AddInternalCall('input', 'readchar', 0, @_readchar);
 M.AddInternalCall('input', 'read', 0, @_read);
End;
End.
