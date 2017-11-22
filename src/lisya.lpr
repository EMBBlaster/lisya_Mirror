program lisya;

uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    lisya_repl,
    dlisp_eval,
    dlisp_values, dlisp_read, lisya_packages; //для отладки

var fs: TVFileStream;

begin
    {$if declared(UseHeapTrace)}
    if fileexists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

//    fs := TVFileStream.Create('work.lisya', fmRead, seBOM);

    if (ParamCount=0) or not execute_file(paramStr(1)) then repl;

end.

