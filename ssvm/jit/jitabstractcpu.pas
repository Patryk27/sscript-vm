(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JITAbstractCPU;

 Interface
 Uses VM, Opcodes, JITOpcodeList;

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

       Public
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
 Result := AllocMem(Size);

 {$DEFINE AB := AllocatedBlocks}
 SetLength(AB, Length(AB)+1);
 AB[High(AB)] := Result;
 {$UNDEF AB}
End;

(* TJITAbstractCPU.isRegNative *)
Function TJITAbstractCPU.isRegNative(const Arg: TOpcodeArg): Boolean;
Begin
 Case Arg.ArgType of // @TODO: it's kinda DRY-breaker :P
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
