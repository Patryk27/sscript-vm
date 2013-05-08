(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mString;

 Interface

 Implementation
Uses Machine;

{ string.length }
Procedure _length(M: TMachine);
Begin
With M do
Begin
 StackPush(Length(StackPop.getString));
End;
End;

initialization
 Add_icall('string', 'length', @_length);
End.
