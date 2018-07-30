unit lisya_optimizer;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils
    , lisya_predicates
    , lisya_exceptions
    , dlisp_values
    , dlisp_eval;

procedure inplace_operators(body: TVList);

implementation

const restricted_ops = [oeAPPEND, oeASSEMBLE, oeBLOCK, oeBREAK, oeCONST, oeCONTINUE,
    oeDEFAULT, oeDELETE, oeEXECUTE_FILE, oeFOR, oeGOTO, oeINSERT, oeLET, oeMACRO,
    oeMACRO_SYMBOL, oePACKAGE, oePOP, oePUSH, oeRETURN, oeSET, oeUSE, oeWHILE, oeWITH];

procedure inplace_operators_in_tail(body: TVList);
var i: integer;
begin
    for i := 1 to body.high do if tpList(body.look[i]) then inplace_operators(body.L[i]);
end;

procedure inplace_operators(body: TVList);
var op: TVOperator; i: integer;
begin
    if body.Count=0 then Exit;

    op := nil;
    if tpOrdinarySymbol(body.look[0]) then begin
        op := eval_operator(body.sym[0]);
        if op<>nil then begin
            body[0] := op;
            case op.op_enum of
                oeASSEMBLE: Exit;
                oeQUOTE: Exit;
                oeCASE: begin
                    if body.Count<2 then raise ELE.Malformed('CASE');
                    if tpList(body.look[1]) then inplace_operators(body.L[1]);
                    for i := 2 to body.high do inplace_operators_in_tail(body.L[i]);
                    Exit;
                end;
            end;
        end
    end
    else

    if tpList(body.look[0]) then inplace_operators(body.L[0]);

    inplace_operators_in_tail(body);
end;

procedure inplace_operators1(body: TVList);
var op: TVOperator; i: integer;
begin
    if body.Count=0 then Exit;

    //WriteLn(body.AsString());

    op := nil;
    if tpOrdinarySymbol(body.look[0]) then begin
        op := eval_operator(body.sym[0]);
        if op<>nil then begin
            //TODO: утечка при исключении;
            if op.op_enum in restricted_ops then raise ELE.Restricted(op.AsString);
            body[0] := op;
            case op.op_enum of
                oeQUOTE: Exit;
                oeCASE: begin
                    if body.Count<2 then raise ELE.Malformed('CASE');
                    if tpList(body.look[1]) then inplace_operators(body.L[1]);
                    for i := 2 to body.high do begin
                        if not vpListLength_1_2(body.look[i]) then raise ELE.Malformed('CASE');
                        inplace_operators_in_tail(body.L[i]);
                    end;
                end
                else inplace_operators_in_tail(body);
            end;
        end
        else inplace_operators_in_tail(body);
    end
    else if tpList(body.look[0]) then inplace_operators(body.L[0]);

    inplace_operators_in_tail(body);
end;


procedure inplace_internals(body: TVList);
begin

end;

end.

