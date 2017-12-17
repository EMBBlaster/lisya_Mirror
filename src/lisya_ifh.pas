unit lisya_ifh;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, dlisp_values, lisya_predicates;

type TBindings = array of record
        nN: integer;
        V: TValue;
       // c: boolean;
        rest: boolean
    end;

function ifh_bind(sign, PL: TValue): TBindings;

function ifh_equal(const A,B: TValue): boolean;

function ifh_member(const L: TVList; const E: TValue): boolean;

function ifh_difference         (const A, B: TVList): TVList;

function ifh_union              (const L: TVList): TVList;

implementation



function ifh_bind(sign, PL: TValue): TBindings;

    procedure add_bind(_n: integer; _v: TValue; {_c: boolean = false;} _r: boolean = false);
    begin
        SetLength(result, Length(result)+1);
        with result[high(result)] do begin
            nN := _n;
            V := _v;
            //c := _c;
            rest := _r;
        end;
    end;

    procedure bind_to_nil(_v: TValue);
    var i: integer;
    begin
        if tpSymbol(_v)
        then add_bind((_v as TVSymbol).N, TVList.Create, true)
        else
            if tpList(_v)
            then for i := 0 to (_v as TVList).high do bind_to_nil((_v as TVList).look[i])
            else
                raise ELE.Create('invalid signature', 'synatax');
    end;

    procedure bind(_s, _v: TValue);
    var i: integer; mode: (necessary, optional, key, flag, rest);
        pl, sign: TVList;
        key_start: integer;

        procedure invalid_bind;
        begin
            raise ELE.Create('invalid binding '+_v.AsString+' to '+_s.AsString,
                'syntax');
        end;

        procedure invalid_sign;
        begin
            raise ELE.Create('invalid signature '+_s.AsString, 'syntax');
        end;

        procedure select_param_mode(V: TVKeyword);
        begin
            if vpKeyword_OPTIONAL(V) then mode := optional
            else
            if vpKeyword_KEY(V) then mode := key
            else
            if vpKeyword_FLAG(V) then mode := flag
            else
            if vpKeyword_REST(V) then mode := rest
            else
            raise ELE.Create(V.AsString+' - invalid parameter mode','syntax');
        end;

        procedure bind_key(V: TVSymbol);
        var i: integer; nN: integer;
        begin
            nN := TVSymbol.symbol_n(':'+V.uname);
            i := pl.high-1;
            while i>=key_start do begin
                if tpKeyword(pl.look[i]) and (nN=pl.SYM[i].N)
                then begin
                    add_bind(V.N, pl[i+1]);
                    Exit;
                end;
                Dec(i, 2);
            end;
            bind_to_nil(V);
        end;

        procedure bind_flag(V: TVSymbol);
        var i: integer; nN: integer;
        begin
            nN := TVSymbol.symbol_n(':'+V.uname);
            for i := key_start to pl.high do
                if tpKeyword(pl.look[i]) and (nN=pl.SYM[i].N)
                then begin
                    add_bind(V.N, TVT.Create, true);
                    Exit;
                end;
            add_bind(V.N, TVList.Create, true);
        end;

        procedure bind_rest(V: TVSymbol);
        var i: integer; L: TVList;
        begin
            L := TVList.Create;
            for i := key_start to PL.high do L.Add(pl[i]);
            add_bind(V.N, L, {false,} true);
        end;

    begin
        //WriteLn('PL>> ',_v.AsString);
        ///WriteLn('sign>> ',_s.AsString);
        if tpOrdinarySymbol(_s)
        then begin
            add_bind((_s as TVSymbol).N, _v.Copy);
            Exit;
        end;

        if tpList(_s) and tpList(_v) then begin
            pl := _v as TVList;
            sign := _s as TVList;
        end
        else invalid_bind;

        mode := necessary;
        for i := 0 to sign.high do
            if tpKeyword(sign.look[i]) then begin
                if mode<>necessary then invalid_sign;
                select_param_mode(sign.look[i] as TVKeyword);
                key_start := i;
            end
            else case mode of
                necessary: if i<=pl.high
                    then bind(sign.look[i], pl.look[i])
                    else invalid_bind;
                optional: if (i-1)<=pl.high
                    then bind(sign.look[i], pl.look[i-1])
                    else bind_to_nil(sign.look[i]);
                key: bind_key(sign.SYM[i]);
                flag: bind_flag(sign.SYM[i]);
                rest: bind_rest(sign.SYM[i]);
            end;
    end;
var i: integer;
begin

   // WriteLn('bind_PL>> ',PL.AsString);
    bind(sign, PL);
   // for i := 0 to high(result) do begin
   //     WriteLn(symbols[result[i].nN],'  =>  ', result[i].V.AsString);
   // end;

end;

////////////////////////////////////////////////////////////////////////////////
/// equal //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function ifh_equal(const A,B: TValue): boolean;
var type_v: (int, num, str, sym, t, lst, struct, any, bytevec); i: integer;
begin
    if tpInteger(A) and tpInteger(B)
    then type_v := int
    else
        if tpReal(A) and tpReal(B)
        then type_v := num
        else
            if tpString(A) and tpString(B)
            then type_v := str
            else
                if tpSymbol(A) and tpSymbol(B)
                then type_v := sym
                else
                    if tpT(A) and tpT(B)
                    then type_v := t
                    else
                        if tpList(A) and tpList(B)
                        then type_v := lst
                        else
                            if tpRecord(A) and tpRecord(B)
                            then type_v := struct
                            else
                                if tpByteVector(A) and tpByteVector(B)
                                then type_v := bytevec
                                else
                                    type_v := any;

    case type_v of
        int: result := (A as TVInteger).fI=(B as TVInteger).fI;
        num: result := (A as TVReal).F=(B as TVReal).F;
        str: result := (A as TVString).S=(B as TVString).S;
        sym: result := (A as TVSymbol).N=(B as TVSymbol).N;
          t: result := true;
        lst: begin
                result := (A as TVList).Count = (B as TVList).Count;
                if result then
                for i:= 0 to (A as TVList).Count-1 do begin
                    result := ifh_equal((A as TVList).look[i],
                                        (B as TVList).look[i]);
                    if not result then break;
                end;
        end;
        struct: begin
            result := (A as TVRecord).count = (B as TVRecord).count;
            if result then
                for i := 0 to (A as TVRecord).count-1 do begin
                    result :=
                        ((A as TVRecord).name_n(i)=(B as TVRecord).name_n(i))
                        and ifh_equal((A as TVRecord).look[i],
                                        (B as TVRecord).look[i]);
                    if not result then Break;
                end;
        end;
        bytevec: result := (A as TVByteVector).equal_to(B as TVByteVector);
        any: result := false;
    end;

end;

////////////////////////////////////////////////////////////////////////////////
/// неупорядоченные множества //////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function ifh_member(const L: TVList; const E: TValue): boolean;
var i: integer;
begin
    result := false;
    for i := 0 to L.High do
        if ifh_equal(L.look[i], E) then begin
            result := true;
            break;
        end;
end;
//------------------------------------------------------------------------------
function ifh_difference         (const A, B: TVList): TVList;
var i: integer;
begin
    result := TVList.Create;
    for i := 0 to A.high do
        if not ifh_member(B, A.look[i])
        then result.add(A[i]);
end;
//------------------------------------------------------------------------------
function ifh_union              (const L: TVList): TVList;
var i, j: integer;
begin
    result := TVList.Create;
    for i := 0 to L.high do
        for j := 0 to L.L[i].high do
            if not ifh_member(result, L.L[i].look[j])
            then result.Add(L.L[i][j]);
end;

end.

