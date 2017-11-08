program lisya;

uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    lisya_repl,
    dlisp_eval,
    lisya_mysql,
    dlisp_values, dlisp_read; //для отладки


var
    i: integer;

    mysql: Tmodule_mysql;
    v: TValue;

begin
    {$if declared(UseHeapTrace)}
    if fileexists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    //v := read_from_string('''2017-10-12 10:20''');

    for i := 1 to ParamCount do if not execute_file(paramStr(i)) then repl;

    if ParamCount=0 then repl;


    ////////////////////////////////////////

    //mysql:= Tmodule_mysql.Create(nil);
    //
    //mysql.Query.Active:=false;
    //mysql.Query.SQL.Text:='SELECT * FROM test';
    //mysql.Query.Active:=true;
    //mysql.Query.First;
    //for i := 0 to mysql.Query.RecordCount-1 do begin
    //    Write(mysql.Query.Fields[0].AsString);
    //    WriteLn('  ',mysql.Query.Fields[1].AsString);
    //    mysql.Query.Next;
    //end;



    //WriteLn(t1.ClassType = t1.ClassType);

end.

