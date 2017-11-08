program lisya;

uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    lisya_repl,
    dlisp_eval,
    dlisp_values, dlisp_read; //для отладки

type
    Tparent = class
    private
        function GetItem(index: integer): byte; virtual; abstract;
    public
        property items[index: integer]: byte read GetItem; default;
    end;

    { Tchaild }

    Tchaild = class (Tparent)
        field: byte;
        function GetItem(index: integer): byte; override;
    end;

var
    i: integer;

    v: TValue;

    tt: Tchaild;

{ Tchaild }

function Tchaild.GetItem(index: integer): byte;
begin
    result := field;
end;

begin
    {$if declared(UseHeapTrace)}
    if fileexists('lisya.trc') then deletefile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    tt := tchaild.Create;
    //WriteLn('>> ', tt[2]);

    //v := read_from_string('''2017-10-12 10:20''');

    for i := 1 to ParamCount do if not execute_file(paramStr(i)) then repl;

    if ParamCount=0 then repl;




end.

