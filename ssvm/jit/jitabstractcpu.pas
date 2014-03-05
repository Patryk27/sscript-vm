(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITAbstractCPU;

 Interface
 Uses VMStruct, VMTypes, Opcodes, JITOpcodeList;

 {$MACRO ON}
 {$DEFINE va := virtual abstract}

 { TJITAbstractCPU }
 Type TJITAbstractCPU =
      Class
       Private
        VM: PVM;

        AllocatedBlocks: Array of Pointer; // allocated by "JITMemAlloc" and freed in TJITAbstractCPU.Destroy()

       Public
        Constructor Create(const fVM: PVM);
        Destructor Destroy; override;

        Function Compile(const OpcodeList: TJITOpcodeList): Pointer; va;

       Public
        Function JITMemAlloc(const Size: uint32): Pointer;

        Function AllocateFloat(const Value: VMFloat): VMReference;
        Function AllocatePChar(const Value: String): VMReference;
        Function AllocateString(const Value: String): VMReference;

       Public
        Function AllocateFloatConstants: Boolean; va; // if 'true', *every* float constant will be reported as 'joa_memory' rather than 'joa_constant'
        Function AllocateStringConstants: Boolean; va; // if 'true', every string constant will be reported as 'joa_memory' rather than 'joa_constant'

        Function isRegNative(const Kind: TBytecodeRegister; const ID: uint8): Boolean; va;
        Function isRegNative(const Arg: TOpcodeArg): Boolean;

       Public
        Property getVM: PVM read VM;
       End;

 Implementation
Uses SysUtils;

(* TJITAbstractCPU.Create *)
Constructor TJITAbstractCPU.Create(const fVM: PVM);
Begin
 if (fVM = nil) Then
  raise Exception.Create('TJITAbstractCPU.Create() -> fVM = nil');

 VM := fVM;
End;

(* TJITAbstractCPU.Destroy *)
Destructor TJITAbstractCPU.Destroy;
Var Pnt: Pointer;
Begin
 For Pnt in AllocatedBlocks Do
  FreeMem(Pnt);

 inherited Destroy;
End;

(* TJITAbstractCPU.JITMemAlloc *)
Function TJITAbstractCPU.JITMemAlloc(const Size: uint32): Pointer;
Begin
 Result := GetMem(Size);

 {$DEFINE AB := AllocatedBlocks}
 SetLength(AB, Length(AB)+1);
 AB[High(AB)] := Result;
 {$UNDEF AB}
End;

(* TJITAbstractCPU.AllocateFloat *)
Function TJITAbstractCPU.AllocateFloat(const Value: VMFloat): VMReference;
Begin
 Result            := JITMemAlloc(sizeof(Value));
 PVMFloat(Result)^ := Value;
End;

(* TJITAbstractCPU.AllocatePChar *)
Function TJITAbstractCPU.AllocatePChar(const Value: String): VMReference;
Var I, Len: uint32;
Begin
 Result := JITMemAlloc(Length(Value)+1);
 Len    := Length(Value);

 For I := 1 To Len Do
  PChar(Result + I-1)^ := Value[I];
End;

(* TJITAbstractCPU.AllocateString *)
Function TJITAbstractCPU.AllocateString(const Value: String): VMReference;
Var Data, StrData: Pointer;
    I, Len       : uint32;
Begin
 Len := Length(Value);

 Result  := JITMemAlloc(sizeof(VMString));
 StrData := JITMemAlloc(Len);

 PVMString(Result)^.Data   := StrData;
 PVMString(Result)^.Length := Len;

 For I := 1 To Len Do
  PChar(StrData + I-1)^ := Value[I];

// VM^.VMStringList.Bind (?) - instead of JITMemAlloc()
End;

(* TJITAbstractCPU.isRegNative *)
Function TJITAbstractCPU.isRegNative(const Arg: TOpcodeArg): Boolean;
Begin
 Case Arg.ArgType of // @TODO: it's kinda a DRY-breaker :P
  ptBoolReg     : Result := isRegNative(reg_eb, Arg.RegID);
  ptCharReg     : Result := isRegNative(reg_ec, Arg.RegID);
  ptIntReg      : Result := isRegNative(reg_ei, Arg.RegID);
  ptFloatReg    : Result := isRegNative(reg_ef, Arg.RegID);
  ptStringReg   : Result := isRegNative(reg_es, Arg.RegID);
  ptReferenceReg: Result := isRegNative(reg_er, Arg.RegID);

  else
   Result := False;
 End;
End;
End.
