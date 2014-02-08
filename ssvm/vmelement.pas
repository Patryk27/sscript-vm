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
        VM: Pointer;

       Protected
        Function getLoaderData: TBCLoaderData;

        Function getBoolReg(const ID: uint8): VMBool;
        Function getCharReg(const ID: uint8): VMChar;
        Function getIntReg(const ID: uint8): VMInt;
        Function getFloatReg(const ID: uint8): VMFloat;
        Function getStringReg(const ID: uint8): VMString;
        Function getReferenceReg(const ID: uint8): VMReference;

        Function getStackPos: VMInt;

        Function BytecodeRelativeToAbsolute(const Rel: Pointer): Pointer;

        Procedure ThrowException(const Message: String);
        Procedure ThrowException(const Format: String; const Args: Array of Const);

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
 Result := PVM(VM)^.LoaderData;
End;

(* TVMElement.getBoolReg *)
{
 Returns value of given VM bool register.
}
Function TVMElement.getBoolReg(const ID: uint8): VMBool;
Begin
 Result := PVM(VM)^.Regs.b[ID];
End;

(* TVMElement.getCharReg *)
{
 Returns value of given VM char register.
}
Function TVMElement.getCharReg(const ID: uint8): VMChar;
Begin
 Result := PVM(VM)^.Regs.c[ID];
End;

(* TVMElement.getIntReg *)
{
 Returns value of given VM int register.
}
Function TVMElement.getIntReg(const ID: uint8): VMInt;
Begin
 Result := PVM(VM)^.Regs.i[ID];
End;

(* TVMElement.getFloatReg *)
{
 Returns value of given VM float register.
}
Function TVMElement.getFloatReg(const ID: uint8): VMFloat;
Begin
 Result := PVM(VM)^.Regs.f[ID];
End;

(* TVMElement.getStringReg *)
{
 Returns value of given VM string register.
}
Function TVMElement.getStringReg(const ID: uint8): VMString;
Begin
 Result := PVM(VM)^.Regs.s[ID];
End;

(* TVMElement.getReferenceReg *)
{
 Returns value of given VM reference register.
}
Function TVMElement.getReferenceReg(const ID: uint8): VMReference;
Begin
 Result := PVM(VM)^.Regs.r[ID];
End;

(* TVMElement.getStackPos *)
Function TVMElement.getStackPos: VMInt;
Begin
 Result := PVM(VM)^.StackPos^;
End;

(* TVMElement.BytecodeRelativeToAbsolute *)
{
 Converts memory address from relative to absolute relative to the first byte of bytecode memory data.
}
Function TVMElement.BytecodeRelativeToAbsolute(const Rel: Pointer): Pointer;
Begin
 Result := Pointer(VMIReference(Rel) + VMIReference(@getLoaderData.CodeData[0]));
End;

(* TVMElement.ThrowException *)
{
 Raises a VM error which halts the virtual machine and returns control back to the user.
}
Procedure TVMElement.ThrowException(const Message: String);
Begin
 PVM(VM)^.ThrowException(Message);
End;

(* TVMElement.ThrowException *)
{
 Raises a VM error which halts the virtual machine and returns control back to the caller.
}
Procedure TVMElement.ThrowException(const Format: String; const Args: Array of const);
Begin
 PVM(VM)^.ThrowException(Format, Args);
End;

(* TVMElement.Create *)
Constructor TVMElement.Create(const fVM: Pointer);
Begin
 VM := fVM;
End;
End.
