(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODE OBJFPC}
{$H+}
Unit Opcodes;

 Interface

 Type TPrimaryType = (ptBoolReg=0, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg,
                      ptBool, ptChar, ptInt, ptFloat, ptString, ptStackVal,
                      ptLabelReference, ptCallstackRef);

 Const PrimaryTypeNames: Array[TPrimaryType] of String =
                     ('bool reg', 'char reg', 'int reg', 'float reg', 'string reg', 'reference reg',
                      'bool', 'char', 'int', 'float', 'string', 'stackval',
                      'label reference', 'callstack ref');

 Const OPCODE_MAX = 39;

 Type TOpcode_E = (o_nop, o_stop,
                   o_push, o_pop,
                   o_add, o_sub, o_mul, o_div, o_neg, o_mov,
                   o_jmp, o_tjmp, o_fjmp, o_call, o_icall, o_acall, o_ret,
                   o_if_e, o_if_ne, o_if_g, o_if_l, o_if_ge, o_if_le,
                   o_strjoin,
                   o_not, o_or, o_xor, o_and, o_shl, o_shr,
                   o_mod,
                   o_arset, o_arget, o_arcrt, o_arlen,
                   o_objfree, o_objinc, o_objdec,
                   o_location);

 Const OpcodesParamCount: Array[TOpcode_E] of Byte = // used in internal disassembler
 (0, 0,
  1, 1,
  2, 2, 2, 2, 1, 2,
  1, 1, 1, 1, 1, 1, 0,
  2, 2, 2, 2, 2, 2,
  2,
  1, 2, 2, 2, 2, 2,
  2,
  3, 3, 3, 3,
  1, 1, 1,
  2);

 Function getOpcodeName(O: TOpcode_E): String;
 Function getOpcodeName(O: Byte): String;

 Implementation
Uses TypInfo;

{ getOpcodeName }
Function getOpcodeName(O: TOpcode_E): String;
Begin
 Result := GetEnumName(TypeInfo(TOpcode_E), Integer(O));

 Delete(Result, 1, 2);
End;

{ getOpcodeName }
Function getOpcodeName(O: Byte): String;
Begin
 Result := getOpcodeName(TOpcode_E(O));
End;
End.
