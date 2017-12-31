program lisya;


uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    {$IFDEF GUI}
    Interfaces, Forms,
    {$ENDIF}
    sysutils,
    lisya_repl,
    dlisp_eval, lisya_ifh, lisya_canvas;

begin
    {$if declared(UseHeapTrace)}
    if FileExists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    {$IFDEF GUI}
    RequireDerivedFormResource := True;
    {$ENDIF}


    if (ParamCount=0) or not execute_file(paramStr(1)) then repl;

end.

