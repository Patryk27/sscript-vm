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
       // r -> register, m -> memory, c -> constant, s -> stackval, ^ -> ditto

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

       jo_bbmov, // bbmov(bool r/m/s, bool r/m/c/s)
       jo_ccmov, // ccmov(char ^, char ^)
       jo_iimov, // iimov(int ^, int ^)
       jo_ffmov, // ffmov(float ^, float ^)
       jo_ssmov, // ssmov(string ^, string ^)
       jo_rrmov, // rrmov(reference r, reference r/s)

       jo_iiadd, // iiadd(int r/m/s, int r/m/c/s) -> no double stackval-reference though
       jo_iisub, // iisub(^)
       jo_iimul, // iimul(^)
       jo_iidiv, // iidiv(^)
       jo_iimod, // iimod(^)

       jo_iicmpe, // iicmpe(int r/m/s, int r/m/c/s) -> no double stackval-reference
       jo_iicmpne, // iicmpne(^)
       jo_iicmpg, // iicmpg(^)
       jo_iicmpl, // iicmpl(^)
       jo_iicmpge, // iicmpge(^)
       jo_iicmple, // iicmple(^)

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

        // b/c/i/f/s/r  pop
        1, 1, 1, 1, 1, 1,

        // bb/cc/ii/ff/ss/rr mov
        2, 2, 2, 2, 2, 2,

        // ii  add/sub/mul/div/mod
        2, 2, 2, 2, 2,

        // ii  cmpe/cmpne/cmpg/cmpl/cmpge/cmple
        2, 2, 2, 2, 2, 2,

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
       joa_constant,
       joa_stackval
      );

 { TJITOpcodeArg }
 Type TJITOpcodeArg =
      Record
       Kind: TJITOpcodeArgKind;

       RegisterID : uint8; // joa_register (if supported by the CPU)
       MemoryAddr : VMReference; // joa_memory
       Constant   : Variant; // joa_constant
       StackvalPos: int32; // joa_stackval
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
