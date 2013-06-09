(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mInput;

 Interface
 Uses vm_header;

 Procedure Init(VM: Pointer);

 Implementation
Uses CRT, mOutput;

{ input.keypressed }
Procedure _keypressed(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 StackPush(VM, KeyPressed);
End;

{ input.getchar }
Procedure _getchar(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Begin
 StackPush(VM, ReadKey);
End;

{ input.readchar }
Procedure _readchar(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Var Ch: Char;
Begin
 Ch      := ReadKey;
 Result^ := Ch;
 strdisplay(Ch);
End;

{ input.read }
Procedure _read(VM: Pointer; Params: PMixedValue; Result: PMixedValue);
Var Str: String;
Begin
 Readln(Str);
 Result^ := Str;
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 AddInternalCall(VM, 'input', 'keypressed', 0, @_keypressed);
 AddInternalCall(VM, 'input', 'getchar', 0, @_getchar);
 AddInternalCall(VM, 'input', 'readchar', 0, @_readchar);
 AddInternalCall(VM, 'input', 'read', 0, @_read);
End;
End.
