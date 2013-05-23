(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit mString;

 Interface
 Uses Machine;

 Procedure Init(M: TMachine);

 Implementation

{ string.length }
Procedure _length(M: TMachine; Params: TCallValues; var Result: TCallValue);
Begin
 With M do
  StackPush(Length(Params[0].Value));
End;

// -------------------------------------------------------------------------- //
Procedure Init(M: TMachine);
Begin
 M.AddInternalCall('string', 'length', 1, @_length);
End;
End.
