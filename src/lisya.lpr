program lisya;

uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    sysutils,
    lisya_repl,
    dlisp_eval, lisya_ifh;

begin
    {$if declared(UseHeapTrace)}
    if FileExists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    if (ParamCount=0) or not execute_file(paramStr(1)) then repl;

end.

