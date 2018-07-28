unit lisya_sign;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dlisp_values, lisya_predicates, mar
    ,lisya_symbols
    ,  lisya_exceptions;


function ifh_bind_params(const sign: TSubprogramSignature; PL: TVList): TValues;

function ifh_build_sign(sign: TVList): TSubprogramSignature;

implementation

procedure error(msg: unicodestring; V: TValue);
begin
    raise ELE.Create(msg+ ' ('+V.AsString+')','syntax/subprogram/parameters');
end;

procedure unique(names: TIntegers; e: TVSymbol);
var i: integer;
begin
    for i := 0 to high(names) do
        if e.N=names[i] then error('duplicated signature element', e);
    append_integer(names,e.N);
end;

function select_param_mode(V: TVSymbol): TSubprogramParmetersMode;
begin
    if vpKeyword_OPTIONAL(V) then result := spmOpt  else
    if vpKeyword_KEY(V)      then result := spmKey  else
    if vpKeyword_FLAG(V)     then result := spmFlag else
    if vpKeyword_REST(V)     then result := spmRest else
    error('invalid parameter mode',V);
end;


function ifh_bind_params(const sign: TSubprogramSignature; PL: TVList): TValues;
    function find_key(k: TValue): integer;
    var i: integer;
    begin
        if not tpKeyword(k) then error('invalid key', k);
        for i := sign.required_count to high(sign.p) do
            if sign.p[i].k=(k as TVSymbol).N then begin result := i; Exit; end;
        error('key not found', k);
    end;
var i: integer;
begin
    if PL.Count<sign.required_count then error('недостаточно параметров', PL);
try
    SetLength(result, Length(sign.p));
    for i := 0 to sign.required_count-1 do result[i] := PL[i];
    for i := sign.required_count to high(result) do result[i] := nil;

    case sign.mode of
        spmReq, spmOpt: begin
            if PL.Count>Length(sign.p) then error('слишком много параметров', PL);
            for i := sign.required_count to PL.Count-1 do result[i] := PL[i];
        end;
        spmKey: begin
            i := sign.required_count;
            while i<PL.Count do begin
                if i=PL.high then error('key without value', PL.look[i]);
                result[find_key(PL.look[i])] := PL[i+1];
                Inc(i,2);
            end;
        end;
        spmFlag: for i := sign.required_count to PL.high do
            result[find_key(PL.look[i])] := TVT.Create;
        spmRest:
            result[sign.required_count] := PL.subseq(sign.required_count);
    end;

    for i := sign.required_count to high(sign.p) do
        if result[i]=nil then result[i] := TVList.Create;
except
    for i := 0 to high(result) do FreeAndNil(result[i]);
    SetLength(result,0);
    raise;
end;
end;


function ifh_build_sign(sign: TVList): TSubprogramSignature;
var i, j: integer; s: TVSymbol;
begin
    result.mode := spmReq;
    result.required_count := sign.Count;
    SetLength(result.p,0);
    for i := 0 to sign.high do
    begin
        if not tpSymbol(sign.look[i]) then error('invalid signature element',sign.look[i]);
        s := sign.SYM[i];

        if (result.mode=spmReq) and tpKeyword(s)
        then begin
            result.mode := select_param_mode(s);
            result.required_count := i;
            if (result.mode=spmRest) and (i<>(sign.high-1))
            then error('malformed rest parameter', sign);
            Continue;
        end;

        if tpKeyword(s) then error('duplicated parameter mode', s);

        for j := 0 to high(result.p) do
            if s.N=result.p[j].n then error('duplicated signature element', s);

        SetLength(result.p, Length(result.p)+1);
        with result.p[high(result.p)] do begin
            n:=S.N;
            k := symbol_n(':'+S.uname);
        end;
    end;
end;


end.

