Unit Exceptions;

 Interface
 Uses SysUtils;

 { base types }
 Type EUncatchable = Class(Exception);
      ECatchable   = Class(Exception);

 { VM exceptions }
 Type eInternalError    = Class(EUncatchable);
      eInvalidCasting   = Class(EUncatchable);
      eInvalidReference = Class(EUncatchable);
      eInvalidFile      = Class(EUncatchable);
      eInvalidOpcode    = Class(EUncatchable);
      eDivByZero        = Class(EUncatchable);

 { catchable exceptions }
 Type eThrow                = Class(ECatchable);
      eInvalidAccess        = Class(ECatchable);
      eOutOfBounds          = Class(ECatchable);
      eNullPointerReference = Class(ECatchable);

 Implementation

End.
