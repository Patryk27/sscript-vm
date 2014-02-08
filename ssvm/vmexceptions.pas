(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit VMExceptions;

 Interface

 { TExceptionType }
 Type TExceptionType = (etNone=-1, etByObject, etByMessage);

 { TExceptionBlock }
 Type PExceptionBlock = ^TExceptionBlock;
      TExceptionBlock =
      Record
       Typ : TExceptionType; // exception type (an object or string)
       Data: Pointer;
      End;

 Implementation

End.
