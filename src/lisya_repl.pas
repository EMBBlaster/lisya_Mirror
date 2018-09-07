unit lisya_repl;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}

    Classes, SysUtils, dlisp_eval, dlisp_values, dlisp_read, lisya_packages,
    lisya_exceptions;



procedure REPL;

function EXEC(filename: unicodestring): boolean;

implementation

var
    root_evaluation_flow: TEvaluationFlow;
    prompt: boolean = true;

procedure REPL;
var input_string, u_input_string: unicodestring; expr, res: TValue;
    last_error_stack: unicodestring;
begin
    last_error_stack := '';

    while true do begin
        if prompt then Write('> ');
        ReadLn(input_string);
        u_input_string := UnicodeUpperCase(input_string);
        if (input_string='') or (u_input_string='EXIT') then break;

        if u_input_string = 'ERROR STACK'
        then WriteLn(last_error_stack)
        else

        if (u_input_string = 'RELOAD') or (u_input_string = 'ПЕРЕЗАГРУЗИТЬ')
        then begin
            root_evaluation_flow.Free;
            root_evaluation_flow := TEvaluationFlow.Create(nil);
            FreePackages;
            if (ParamCount>0) then EXEC(paramStr(1));
        end
        else

        try
            expr := nil;
            expr := read_from_string(input_string);
            res := nil;
            res := root_evaluation_flow.Eval(expr);
            if prompt then begin Write('= '); print(res, nil); WriteLn(); end;
            FreeAndNil(res);
        except
            on E:ELE do begin
                Write('ERROR: ',E.Message);
                if E.EClass<>''
                then WriteLn(' (',E.EClass,')')
                else WriteLn();
                last_error_stack := E.EStack + E.Message + ' ('+E.EClass+')';
            end;
        end;
    end;
end;



function EXEC(filename: unicodestring): boolean;
var fn: unicodestring;
begin
    result := false;
    if filename='-' then begin prompt := false; REPL; result := true; exit; end;

    fn := FindLisyaFile(filename);
    if fn='' then raise ELE.Create(filename, 'script not found');
    try
        result := root_evaluation_flow.oph_execute_file(fn);
    except
        on E:ELE do begin
            if E.EClass<>'repl'
            then begin
                WriteLn('ERROR during execution ',fn);
                Write(E.EStack);
                WriteLn(E.Message,' (',E.EClass,')');
            end;
        end;
    end;
end;

initialization
    root_evaluation_flow := TEvaluationFlow.Create(nil);
finalization
    root_evaluation_flow.Free;
end.

