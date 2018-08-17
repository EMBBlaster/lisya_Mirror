program lisya;


uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    //cmem,
    {$ENDIF}
    {$IFDEF GUI}
    Interfaces, Forms, lisya_canvas,
    {$ENDIF}
    sysutils,
    lisya_repl,
    dlisp_eval, lisya_process, lisya_streams, lisya_exceptions, pipes,
    lisya_string_predicates, lisya_sign, lisya_symbols, lisya_gc, 
lisya_optimizer, lisya_heuristic;


begin
    {$if declared(UseHeapTrace)}
    if FileExists('lisya.trc') then DeleteFile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    {$IFDEF GUI}
    RequireDerivedFormResource := True;
    //Application.Initialize;
    {$ENDIF}


    if (ParamCount=0) or not EXEC(paramStr(1)) then REPL;

end.



