Unit Exceptions;

 Interface
 Uses SysUtils;

 { base types }
 Type VMException   = Class(Exception);
      CodeException = Class(Exception);

 { VM exceptions }
 Type eInternalError    = Class(VMException);
      eInvalidCasting   = Class(VMException);
      eInvalidReference = Class(VMException);
      eInvalidFile      = Class(VMException);
      eInvalidOpcode    = Class(VMException);
      eDivByZero        = Class(VMException);

 { code (user) exceptions }
 Type eThrow                = Class(CodeException);
      eInvalidAccess        = Class(CodeException);
      eOutOfBounds          = Class(CodeException);
      eNullPointerReference = Class(CodeException);

 Implementation

End.
