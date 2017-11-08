program lisya;

uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    lisya_repl,
    dlisp_eval,
    dlisp_values, dlisp_read; //для отладки


var
    i: integer;

    v: TValue;

begin
    {$if declared(UseHeapTrace)}
    if fileexists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    //v := read_from_string('''2017-10-12 10:20''');

    for i := 1 to ParamCount do if not execute_file(paramStr(i)) then repl;

    if ParamCount=0 then repl;




end.

