(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mInput;

 Interface

 Implementation
Uses CRT, Machine, mOutput;

{ input.keypressed }
Procedure _keypressed(M: TMachine);
Begin
 M.StackPush(KeyPressed);
End;

{ input.getchar }
Procedure _getchar(M: TMachine);
Begin
 M.StackPush(ReadKey);
End;

{ input.readchar }
Procedure _readchar(M: TMachine);
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
Procedure _read(M: TMachine);
Var Str: String;
Begin
With M do
Begin
 Readln(Str);
 StackPush(Str);
End;
End;

initialization
 Add_icall('input', 'keypressed', @_keypressed);
 Add_icall('input', 'getchar', @_getchar);
 Add_icall('input', 'readchar', @_readchar);
 Add_icall('input', 'read', @_read);
End.
