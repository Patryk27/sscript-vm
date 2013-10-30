{$MACRO ON}

// enable console log? (debug only)
{.$DEFINE JIT_CONSOLE_LOG}

// define target CPU
{$DEFINE CPU := CPU_unknown}

{$IFDEF CPU32}
 {$DEFINE CPU := CPU_x86}
{$ENDIF}

// check if target CPU is supported
{$IF CPU = CPU_unknown}
 {$FATAL Unknown or unsupported target CPU!}
{$ENDIF}
