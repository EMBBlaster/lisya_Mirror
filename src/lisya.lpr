program lisya;


uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    {$IFDEF GUI}
    Interfaces, Forms, lisya_canvas,
    {$ENDIF}
    sysutils,
    lisya_repl,
    dlisp_eval, lisya_process, lisya_streams, lisya_exceptions,
lisya_string_predicates;

begin
    {$if declared(UseHeapTrace)}
    if FileExists('lisya.trc') then DeleteFile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    {$IFDEF GUI}
    RequireDerivedFormResource := True;
    {$ENDIF}

    if (ParamCount=0) or not EXEC(paramStr(1)) then REPL;


end.



