program lisya;

uses
    cthreads,
    cwstring,
    lisya_repl,
    dlisp_eval;

var
    i: integer;

begin
    {$if declared(UseHeapTrace)}
    if fileexists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    for i := 1 to ParamCount do if not execute_file(paramStr(i)) then repl;

    if ParamCount=0 then repl;

end.

