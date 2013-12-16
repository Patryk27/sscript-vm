(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JITOpcodes;

 Interface
 Uses VMTypes;

 { TJITOpcodeKind }
 Type TJITOpcodeKind = // don't change order of these!
      (
       jo_ipush, // ipush(int register/memory/contant)

       jo_iimov, // iimov(int register/memory, int register/memory/constant)

       jo_iiadd, // iiadd(int register/memory, int register/memory/constant)
       jo_iisub, // iisub(^)
       jo_iimul, // iimul(^)
       jo_iidiv, // iidiv(^)
       jo_iimod, // iimod(^)

       ji_ifcast, // @params? int -> float
       ji_ficast, // @params? float -> int

       jo_icall, // icall(memory ref) (memory reference is the address where the icall's data's located (the "TCall" structure pointer"))

       jo_jmp, // jmp(const int)

       jo_stop // stop()
      );

 { JITOpcodeParamCount }
 Const JITOpcodeParamCount:
       Array[TJITOpcodeKind] of uint8 =
       (
        1,

        2,

        2,
        2,
        2,
        2,
        2,

        2,
        2,

        1,
        1,

        0
       );

 { TJITOpcodeArgKind }
 Type TJITOpcodeArgKind =
      (
       joa_register,
       joa_memory,
       joa_constant
      );

 { TJITOpcodeArg }
 Type TJITOpcodeArg =
      Record
       Kind: TJITOpcodeArgKind;

       RegisterID: uint8; // joa_register (if supported by the CPU)
       MemoryAddr: VMReference; // joa_memory
       Constant  : Variant; // joa_constant
      End;

 { TJITOpcode }
 Type PJITOpcode = ^TJITOpcode;
      TJITOpcode =
      Record
       ID  : TJITOpcodeKind;
       Args: Array[0..2] of TJITOpcodeArg;
      End;

 Implementation

End.
