unit lisya_repl;

{$mode delphi}

interface

uses
    cwstring, Classes, SysUtils, dlisp_eval, dlisp_values, dlisp_read;



procedure REPL;

implementation

var
    root_evaluation_flow: TEvaluationFlow;

procedure REPL;
var input_string: unicodestring; expr, res: TValue;
begin
    Write('> ');ReadLn(input_string);
    while input_string<>'' do begin
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
            end;
        end;
        Write('> ');ReadLn(input_string);
    end;
end;

initialization
    root_evaluation_flow := TEvaluationFlow.Create(nil);
finalization
    root_evaluation_flow.Free;
end.

