program lisya;

uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    lisya_repl,
    dlisp_eval;

type t1=class end;
    t2=class(t1) end;

var
    i: integer;

   o1: t1; o2 :t2;

begin
    {$if declared(UseHeapTrace)}
    if fileexists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    for i := 1 to ParamCount do if not execute_file(paramStr(i)) then repl;

    if ParamCount=0 then repl;


    //WriteLn(t1.ClassType = t1.ClassType);

end.

