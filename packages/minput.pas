(*
 Copyright © by Patryk Wychowaniec, 2013-2014
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
Procedure _keypressed(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 SSStackPush(VM, KeyPressed);
End;

{ input.getchar }
Procedure _getchar(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Begin
 SSStackPush(VM, ReadKey);
End;

{ input.readchar }
Procedure _readchar(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Var Ch: Char;
Begin
 Ch      := ReadKey;
 Result^ := Ch;
 strdisplay(Ch);
End;

{ input.read }
Procedure _read(VM: Pointer; Params: PMixedValue; Result: PMixedValue); register;
Var Str: String;
Begin
 Readln(Str);
 Result^ := Str;
End;

// -------------------------------------------------------------------------- //
Procedure Init(VM: Pointer);
Begin
 SSAddInternalCall(VM, 'input', 'keypressed', 0, @_keypressed);
 SSAddInternalCall(VM, 'input', 'getchar', 0, @_getchar);
 SSAddInternalCall(VM, 'input', 'readchar', 0, @_readchar);
 SSAddInternalCall(VM, 'input', 'read', 0, @_read);
End;
End.
