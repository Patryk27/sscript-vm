(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit JITOpcodes;

 Interface
 Uses Variants, VMTypes;

 { TJITOpcodeKind }
 Type TJITOpcodeKind = // don't change order of these!
      (
       jo_ipush, // ipush(int register/memory/contant)
       jo_spush, // spush(string register/memory); string constants are automatically allocated and reported as "memory"

       jo_bbmov, // bbmov(bool register/memory, bool register/memory/constant)
       jo_iimov, // iimov(int register/memory, int register/memory/constant)

       jo_iiadd, // iiadd(int register/memory, int register/memory/constant)
       jo_iisub, // iisub(^)
       jo_iimul, // iimul(^)
       jo_iidiv, // iidiv(^)
       jo_iimod, // iimod(^)

       ji_ifcast, // @params? int -> float
       ji_ficast, // @params? float -> int

       jo_jmp, // jmp(const int) -> parameter is JIT opcode index where to jump
       jo_tjmp, // tjmp(^)
       jo_fjmp, // fjmp(^)

       jo_call, // call(^)
       jo_ret, // ret()

       jo_icall, // icall(memory ref) (memory reference is the address where the icall's data's located (the "TCall" structure pointer"))

       jo_stop // stop()
      );

 { JITOpcodeParamCount }
 Const JITOpcodeParamCount:
       Array[TJITOpcodeKind] of uint8 =
       (
        // i/s  push
        1, 1,

        // **mov
        2, 2,

        // ii  add/sub/mul/div/mod
        2, 2, 2, 2, 2,

        // **cast
        2, 2,

        // jmp, tjmp, fjmp
        1, 1, 1,

        // call, ret
        1, 0,

        // icall
        1,

        // stop
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
