(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit VMElement;

 Interface
 Uses VMTypes, BCLoader;

 { TVMElement }
 Type TVMElement =
      Class
       Protected
        VMPnt: Pointer;

       Protected
        Function getLoaderData: TBCLoaderData;

        Function getBoolReg(const ID: uint8): VMBool; //inline;
        Function getCharReg(const ID: uint8): VMChar; //inline;
        Function getIntReg(const ID: uint8): VMInt; //inline;
        Function getFloatReg(const ID: uint8): VMFloat; //inline;
        Function getStringReg(const ID: uint8): PVMString; //inline;
        Function getReferenceReg(const ID: uint8): VMReference; //inline;

        Function getStackPos: VMInt; inline;

        Procedure CheckMemory;

        Procedure ThrowException(const Message: String);
        Procedure ThrowException(const Format: String; const Args: Array of Const);

        Procedure WriteLog(const Message: String);
        Procedure WriteLog(const Format: String; const Args: Array of Const);

        Function BytecodeRelativeToAbsolute(const Rel: Pointer): Pointer;

       Public
        Constructor Create(const fVM: Pointer);
       End;

 Implementation
Uses VMStruct;

(* TVMElement.getLoaderData *)
{
 Returns VM's loader data
}
Function TVMElement.getLoaderData: TBCLoaderData;
Begin
 Result := PVM(VMPnt)^.LoaderData;
End;

(* TVMElement.getBoolReg *)
{
 Returns value of given VM bool register.
}
Function TVMElement.getBoolReg(const ID: uint8): VMBool;
Begin
 Result := PVM(VMPnt)^.Regs.b[ID];
End;

(* TVMElement.getCharReg *)
{
 Returns value of given VM char register.
}
Function TVMElement.getCharReg(const ID: uint8): VMChar;
Begin
 Result := PVM(VMPnt)^.Regs.c[ID];
End;

(* TVMElement.getIntReg *)
{
 Returns value of given VM int register.
}
Function TVMElement.getIntReg(const ID: uint8): VMInt;
Begin
 Result := PVM(VMPnt)^.Regs.i[ID];
End;

(* TVMElement.getFloatReg *)
{
 Returns value of given VM float register.
}
Function TVMElement.getFloatReg(const ID: uint8): VMFloat;
Begin
 Result := PVM(VMPnt)^.Regs.f[ID];
End;

(* TVMElement.getStringReg *)
{
 Returns value of given VM string register.
}
Function TVMElement.getStringReg(const ID: uint8): PVMString;
Begin
 Result := PVM(VMPnt)^.Regs.s[ID];
End;

(* TVMElement.getReferenceReg *)
{
 Returns value of given VM reference register.
}
Function TVMElement.getReferenceReg(const ID: uint8): VMReference;
Begin
 Result := PVM(VMPnt)^.Regs.r[ID];
End;

(* TVMElement.getStackPos *)
Function TVMElement.getStackPos: VMInt;
Begin
 Result := PVM(VMPnt)^.StackPos^;
End;

(* TVMElement.CheckMemory *)
{
 See TVM.CheckMemory()
}
Procedure TVMElement.CheckMemory;
Begin
 PVM(VMPnt)^.CheckMemory;
End;

(* TVMElement.ThrowException *)
{
 Raises a VM error which halts the virtual machine and returns control back to the user.
}
Procedure TVMElement.ThrowException(const Message: String);
Begin
 PVM(VMPnt)^.ThrowException(Message);
End;

(* TVMElement.ThrowException *)
{
 Raises a VM error which halts the virtual machine and returns control back to the caller.
}
Procedure TVMElement.ThrowException(const Format: String; const Args: Array of const);
Begin
 PVM(VMPnt)^.ThrowException(Format, Args);
End;

(* TVMElement.WriteLog *)
{
 See TVM.WriteLog()
}
Procedure TVMElement.WriteLog(const Message: String);
Begin
 PVM(VMPnt)^.WriteLog(Message);
End;

(* TVMElement.WriteLog *)
{
 See TVM.WriteLog()
}
Procedure TVMElement.WriteLog(const Format: String; const Args: Array of const);
Begin
 PVM(VMPnt)^.WriteLog(Format, Args);
End;

(* TVMElement.BytecodeRelativeToAbsolute *)
{
 Converts memory address from relative to absolute relative to the first byte of bytecode memory data.
}
Function TVMElement.BytecodeRelativeToAbsolute(const Rel: Pointer): Pointer;
Begin
 Result := Pointer(VMIReference(Rel) + VMIReference(@getLoaderData.CodeData[0]));
End;

(* TVMElement.Create *)
Constructor TVMElement.Create(const fVM: Pointer);
Begin
 VMPnt := fVM;
End;
End.
