unit lisya_optimizer;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils
    , mar
    , lisya_predicates
    , lisya_exceptions
    , lisya_ifh
    , dlisp_values
    , dlisp_eval;

procedure inplace_operators(body: TVList);
function extract_body_symbols(body, params: TVList; st: TVSymbolStack = nil): TVList;

implementation

const restricted_ops = [oeAPPEND, oeASSEMBLE, oeBLOCK, oeBREAK, oeCONST, oeCONTINUE,
    oeDEFAULT, oeDELETE, oeFOR, oeGOTO, oeINSERT, oeLET, oeMACRO,
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


function extract_body_symbols(body, params: TVList; st: TVSymbolStack): TVList;
var i: integer; tmp: TVList;
    stack, symbols: TIntegers;
    function in_stack(s: TVSymbol): boolean;
    var i: integer;
    begin
        result := true;
        for i := high(stack) downto 0 do if stack[i]=S.N then Exit;
        result := false;
    end;


    procedure push_symbols(V: TValue);
    var i: integer; L: TVList;
    begin
        if tpOrdinarySymbol(V) then append_integer(stack, (V as TVSymbol).n);
        if tpList(V) then begin
            L := V as TVList;
            for i := 0 to L.high do push_symbols(L.look[i]);
        end;
    end;

    procedure declared_symbols(expr: TVList);
    var i: integer;
    begin
        if expr.Count<2 then exit;
        if vpListVariableExpression(expr) or vpListRoutineExpression(expr)
        then push_symbols(expr.look[1]);
        //TODO: здесь надо обрабатывать LET
    end;

    procedure find_symbols(expr: TVList);
    var i: integer; start, from: integer;
    begin
        start := Length(stack);
        if vpListLambdaExpression(expr) then begin
            push_symbols(expr.L[1]);
            from := 2;
        end
        else if vpListRoutineExpression(expr) then begin
            push_symbols(expr.L[2]);
            from := 3;
        end
        else if vpListVariableExpression(expr) then begin
            from := 2;
        end
        else from := 0;
        //TODO: здесь надо обрабатывать LET
        for i := from to expr.high do begin
            if tpOrdinarySymbol(expr.look[i]) and not in_stack(expr.SYM[i])
            then append_integer(symbols,expr.SYM[i].N);
            if tpList(expr.look[i]) then begin
                find_symbols(expr.L[i]);
                declared_symbols(expr.L[i]);
            end;
        end;
        SetLength(stack, start);
    end;

begin
    //эта функция должна извлекать из тела процедуры или функции список,
    //используемых символов
    stack := nil;
    symbols := nil;

    push_symbols(params);
    find_symbols(body);
    result := TVList.Create;
    result.SetCapacity(Length(symbols));
    for i := 0 to high(symbols) do result.Add(TVSymbol.Create(symbols[i]));

    try
        //здесь ifh_union используется для удаления дубликатов
        tmp := TVList.Create([result]);
        result := ifh_union(tmp);
    finally
        FreeAndNil(tmp);
    end;

end;


procedure inplace_internals(body: TVList);
begin

end;

end.

