(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit Opcodes;

 Interface
 Uses VMTypes;

 { TOpcodeKind }
 Type TOpcodeKind = // don't change order of these!
      (
       o_nop, o_stop,
       o_push, o_pop,
       o_add, o_sub, o_mul, o_div, o_neg, o_mov,
       o_jmp, o_tjmp, o_fjmp, o_call, o_icall, o_acall, o_ret,
       o_if_e, o_if_ne, o_if_g, o_if_l, o_if_ge, o_if_le,
       o_strjoin,
       o_not, o_or, o_xor, o_and, o_shl, o_shr,
       o_mod,
       o_arset, o_arset1, o_arget, o_arget1, o_arcrt, o_arlen,
       o_strset, o_strget, o_strlen
      );

 { OpcodeArgCount }
 Const OpcodeArgCount: Array[TOpcodeKind] of uint8 =
       (
        0, 0,
        1, 1,
        2, 2, 2, 2, 1, 2,
        1, 1, 1, 1, 1, 1, 0,
        2, 2, 2, 2, 2, 2,
        2,
        1, 2, 2, 2, 2, 2,
        2,
        3, 3, 3, 3, 3, 2,
        3, 3, 2
       );

 { TOpcodeArgType }
 Type TOpcodeArgType =
      (ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg, ptBool, ptChar, ptInt, ptFloat, ptString, ptStackval, ptConstantMemRef);

 { TOpcodeArgTypeSet }
 Type TOpcodeArgTypeSet = Set of TOpcodeArgType;

 { TBytecodeRegister }
 Type TBytecodeRegister = // a helper type used in the JIT compiler
      (reg_eb, reg_ec, reg_ei, reg_ef, reg_es, reg_er);

 { TOpcodeArg }
 Type TOpcodeArg =
      Record
       Case ArgType: TOpcodeArgType of
        ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg: (RegID: uint8); // @TODO: 'ptBoolReg..ptReferenceReg' (but it makes Lazarus's code completion not working :/)

        ptBool    : (ImmBool: VMBool);
        ptChar    : (ImmChar: VMChar);
        ptInt     : (ImmInt: VMInt);
        ptFloat   : (ImmFloat: VMFloat);
        ptString  : (ImmString: ShortString);
        ptStackval: (StackvalPos: int32);

        ptConstantMemRef: (MemoryAddress: uint64);
       End;

 { TOpcodeArgArray }
 Type TOpcodeArgArray = Array of TOpcodeArg;

 // ------------------------------------------------------------------------- //
 Function StripRegFromArgType(const T: TOpcodeArgType): TOpcodeArgType;

 Implementation

(* StripRegFromType *)
Function StripRegFromArgType(const T: TOpcodeArgType): TOpcodeArgType;
Begin
 Result := T;

 Case Result of
  ptBoolReg  : Result := ptBool;
  ptCharReg  : Result := ptChar;
  ptIntReg   : Result := ptInt;
  ptFloatReg : Result := ptFloat;
  ptStringReg: Result := ptString;
//  ptReferenceReg: Result := ptInt;
 End;
End;
End.
