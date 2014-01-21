(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit JITOpcodes;

 Interface
 Uses Variants, VMTypes;

 { TJITOpcodeKind }
 Type TJITOpcodeKind = // don't change order of these!
      (
       // r -> register, m -> memory, c -> constant, ^ -> ditto

       jo_bpush, // bpush(bool r/m/c)
       jo_cpush, // cpush(char ^)
       jo_ipush, // ipush(int ^)
       jo_fpush, // fpush(float ^)
       jo_spush, // spush(string r/m); string constants are automatically allocated and reported as "memory"

       jo_bpop, // bpop(bool r/m)
       jo_cpop, // cpop(char ^)
       jo_ipop, // ipop(int ^)
       jo_fpop, // fpop(float ^)
       jo_spop, // spop(string ^)
       jo_rpop, // rpop(reference ^)

       jo_bbmov, // bbmov(bool r/m, bool r/m/c)
       jo_iimov, // iimov(int r/m, int r/m/c)

       jo_iiadd, // iiadd(int r/m, int r/m/c)
       jo_iisub, // iisub(^)
       jo_iimul, // iimul(^)
       jo_iidiv, // iidiv(^)
       jo_iimod, // iimod(^)

       jo_iicmpe, // iicmpe(int r/m, int r/m/c)
       jo_iicmpne, // iicmpne(^)
       jo_iicmpg, // iicmpg(^)
       jo_iicmpl, // iicmpl(^)
       jo_iicmpge, // iicmpge(^)
       jo_iicmple, // iicmple(^)

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
        // b/c/i/f/s  push
        1, 1, 1, 1, 1,

        // b/c/i/f/s/r pop
        1, 1, 1, 1, 1, 1,

        // **mov
        2, 2,

        // ii  add/sub/mul/div/mod
        2, 2, 2, 2, 2,

        // ii  cmpe/cmpne/cmpg/cmpl/cmpge/cmple
        2, 2, 2, 2, 2, 2,

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
       Kind: TJITOpcodeKind;
       Args: Array[0..2] of TJITOpcodeArg;
      End;

 Implementation

End.
