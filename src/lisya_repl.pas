unit lisya_repl;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}

    Classes, SysUtils, dlisp_eval, dlisp_values, dlisp_read, mar, lisya_packages;



procedure REPL;

function EXEC(filename: unicodestring): boolean;

implementation

var
    root_evaluation_flow: TEvaluationFlow;

procedure REPL;
var input_string: unicodestring; expr, res: TValue;
    last_error_stack: unicodestring;
begin
    last_error_stack := '';
    Write('> ');ReadLn(input_string);
    while input_string<>'' do begin

        if UpperCaseU(input_string) = 'ERROR STACK'
        then WriteLn(last_error_stack)
        else

        try
            expr := nil;
            expr := read_from_string(input_string);
            res := nil;
            res := root_evaluation_flow.Eval(expr);
            Write('= ');
            print(res, nil);
            WriteLn();
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
        Write('> ');ReadLn(input_string);
    end;
end;



function EXEC(filename: unicodestring): boolean;
var fn: unicodestring;
begin
    result := false;
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

