(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMICall;

 Interface
 Uses VMStack, FGL;

 { TInternalCallHandler }
 Type TInternalCallHandler = Procedure (VM: Pointer; Params: PMixedValue; Result: PMixedValue); register; // single icall handler

 { TInternalCall }
 Type PInternalCall = ^TInternalCall;
      TInternalCall =
      Record
       PackageName, FunctionName, FullName: String; // package name, function name, full name ('package_name.function_name')
       ParamCount                         : uint8; // number of parameters required by the internal call
       Handler                            : TInternalCallHandler; // handler associated with this icall
      End;

 { TInternalCallList }
 Type TInternalCallList = specialize TFPGList<PInternalCall>;

 Implementation

End.
