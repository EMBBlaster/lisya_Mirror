unit lisya_ifh;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, dlisp_values, lisya_predicates, mar,  math, lisya_exceptions;

type TBindings = array of record
        nN: integer;
        V: TValue;
       // c: boolean;
        rest: boolean
    end;

function ifh_bind(sign, PL: TValue): TBindings;

function ifh_equal(const A,B: TValue): boolean;

function ifh_member(const L: TVList; const E: TValue): boolean;
function ifh_member1(const L: TVList; const E: TValue): boolean;

function ifh_difference         (const A, B: TVList): TVList;

function ifh_union              (const L: TVList): TVList;
function ifh_union1             (const L: TVList): TVList;

function ifh_intersection       (const L: TVList): TVList;
function ifh_set_include        (const A, B: TVList): boolean;

function ifh_map(call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;
function ifh_fold(call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;

function ifh_like (str1, str2: unicodestring): integer;

implementation

type THashes = array of DWORD;
type THashesList = array of THashes;

procedure ifhh_hash_list(L: TVList; out hashes: THashes); inline;
var i: integer;
begin
    SetLength(hashes, L.Count);
    for i := 0 to L.high do hashes[i] := L.look[i].hash;
end;

procedure ifhh_hash_lists(L: TVList; out hashes: THashesList); inline;
var i: integer;
begin
    SetLength(hashes, L.Count);
    for i := 0 to L.high do ifhh_hash_list(L.L[i], hashes[i]);
    // begin
    //    SetLength(hashes[i], L.L[i].Count);
    //    for j := 0 to L.L[i].high do hashes[i][j] := L.L[i].look[j].hash;
    //end;
end;

function ifhh_member_hashed(const hL: THashes; L: TVList;
                                  hE: DWORD; E: TValue): boolean;
var i: integer;
begin
    result := true;
    for i := 0 to L.high do
        if (hE=hL[i]) and ifh_equal(L.look[i], E) then Exit;

    result := false;
end;

////////////////////////////////////////////////////////////////////////////////
/// bind ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
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
        then add_bind((_v as TVSymbol).N, TVList.Create)
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
                    add_bind(V.N, TVT.Create);
                    Exit;
                end;
            add_bind(V.N, TVList.Create);
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
//var i: integer;
begin
   //curry := false;
   // WriteLn('bind_PL>> ',PL.AsString);
    bind(sign, PL);
  //  for i := 0 to high(result) do begin
  //      WriteLn(symbols[result[i].nN],'  =>  ', result[i].V.AsString,
  //          '  ', result[i].rest);
  //  end;

end;

////////////////////////////////////////////////////////////////////////////////
/// equal //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function ifh_equal(const A,B: TValue): boolean;
var type_v: (int, num, str, sym, t, lst, struct, any, bytevec); i: integer;
begin
    result := A.equal(B);
    Exit;
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

   // WriteLn('A>> ', A.AsString);
   // WriteLn('B>> ', B.AsString);

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
function ifh_member1(const L: TVList; const E: TValue): boolean;
var i: integer;
    hashes: array of DWORD;
    hash: DWORD;
begin
    result := false;
    SetLength(hashes, L.Count);
    for i := 0 to L.high do hashes[i] := L.look[i].hash;
    hash := E.hash;
    for i := 0 to L.High do
        if (hash=hashes[i]) and ifh_equal(L.look[i], E) then begin
            result := true;
            break;
        end;
    SetLength(hashes, 0);
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
function ifh_union1             (const L: TVList): TVList;
var i, j: integer;
    hashes: THashesList;
    res: THashes;

begin
    result := TVList.Create;
    if L.Count=0 then exit;

    ifhh_hash_lists(L, hashes);
    SetLength(res, 0);

    for i := 0 to L.high do
        for j := 0 to L.L[i].high do
            if not ifhh_member_hashed(res, result, hashes[i][j], L.L[i].look[j])
            then begin
                result.Add(L.L[i][j]);
                SetLength(res, Length(res)+1);
                res[high(res)] := hashes[i][j];
        end;

    for i := 0 to high(hashes) do SetLength(hashes[i], 0);
    SetLength(hashes, 0);
    SetLength(res, 0);
end;
//------------------------------------------------------------------------------
function ifh_intersection       (const L: TVList): TVList;
var hashes: array of array of DWORD;
    res: array of DWORD;
    i,j, min_length, res_count :integer;
    m: boolean;

    procedure add_in_res(v: integer);
    var k: integer;
    begin
        for k := 0 to res_count-1 do
            if (hashes[0][v]=res[k]) and ifh_equal(L.L[0].look[v], result.look[k])
            then exit;
        res[res_count] := hashes[0][v];
        result.Add(L.L[0][v]);
        inc(res_count);
    end;

begin
    //TODO: intersection может быть ускорена если предварительно отсортировать
    //множества по длине
    //левая свёртка этой функцией работает на 10% быстрее чем сама функция
    //от множества аргументов
    result := TVList.Create;
    if (L.Count=0) then Exit;

    //вычисление хэшей
    ifhh_hash_lists(L, hashes);

    //результат не может быть больше самого маленького из множеств
    min_length := Length(hashes[0]);
    for i := 1 to High(hashes) do
        if Length(hashes[i])<min_length then min_length := Length(hashes[i]);
    result.SetCapacity(min_length);
    SetLength(res, min_length);
    res_count := 0;

    for i := 0 to L.L[0].high do begin
        for j := 1 to L.high do begin
            m := ifhh_member_hashed(hashes[j], L.L[j], hashes[0][i], L.L[0].look[i]);
            if not m then break;
        end;
        if m then add_in_res(i);
    end;

    for i := 0 to high(hashes) do SetLength(hashes[i], 0);
    SetLength(hashes, 0);
    SetLength(res, 0);
end;
//------------------------------------------------------------------------------
function ifh_set_include        (const A, B: TVList): boolean;
var hashes_A, hashes_B: THashes; i: integer;
begin
    ifhh_hash_list(A, hashes_A);
    ifhh_hash_list(B, hashes_B);

    result := true;
    for i := 0 to B.high do begin
        result := ifhh_member_hashed(hashes_A, A, hashes_B[i], B.look[i]);
        if not result then exit;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
/// threads ////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function ifh_fold(call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;
var expr: TVList; i: integer;
begin try
    expr := TVList.Create([P.Copy, PL[b], PL[b+1]]);
    result := call(expr);
    for i := b+2 to e do begin
        expr[1] := result;
        expr[2] := PL[i];
        result := call(expr);
    end;
finally
    expr.Free;
end;
end;
//------------------------------------------------------------------------------
function ifh_map(call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;
var expr: TVList;
    i, j: integer;
begin try try
    result := TVList.Create;
    (result as TVList).SetCapacity(e-b+1);

    expr := TVList.Create([P.Copy]);
    for i := 0 to PL.high do expr.Add(nil);
    for i := b to e do begin
        for j := 0 to PL.high do expr[j+1] := PL.L[j][i];
        //WriteLn('ifh_map>> ', expr.AsString);
        (result as TVList).Add(call(expr));
    end;
except
    FreeAndNil(result);
    raise;
end;
finally
    expr.Free;
end;
end;

////////////////////////////////////////////////////////////////////////////////
/// строки /////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function ifh_like (str1, str2: unicodestring): integer;
var v: array of record s: unicodestring; n: array[1..2] of integer; end;
    procedure add_point(n: byte; p: unicodestring);
    var i: integer;
    begin
        for i := 0 to high(v) do
            if v[i].s=p then begin Inc(v[i].n[n]); Exit; end;
        SetLength(v, Length(v)+1);
        v[high(v)].s:=p;
        v[high(v)].n[1]:=0;
        v[high(v)].n[2]:=0;
        v[high(v)].n[n]:=1;
    end;

    procedure vectorize(n: byte; s: unicodestring);
    var i: integer;
    begin
        for i:=1 to length(s)   do add_point(n, s[i..i]);
        for i:=1 to length(s)-1 do add_point(n, s[i..i+1]);
        for i:=1 to length(s)-2 do add_point(n, s[i..i+2]);
        for i:=1 to length(s)-2 do add_point(n, s[i]+#1+s[i+2]);
    end;

    function distance: integer;
    var i: integer;
    begin
        result := 0;
        for i := 0 to high(v) do result := result+min(v[i].n[1],v[i].n[2]);
    end;

    function abbr(s: unicodestring): unicodestring;
    var i: integer; word_start: boolean;
    begin
        result := '';
        word_start := true;
        for i := 1 to Length(s) do
            case s[i] of
                ' ': word_start:=true;
                else
                    if word_start
                    then begin result := result+s[i]; word_start:=false; end;
            end;
    end;

var s1, s2: unicodestring;
begin
    s1 := UpperCaseU(str1);
    s2 := UpperCaseU(str2);
    if (Length(s1)=0) or (Length(s2)=0) then begin result:=0; Exit; end;
    //построение вектора
    SetLength(v, 0);
    vectorize(1, s1);
    vectorize(1, abbr(s1));
    vectorize(2, s2);
    vectorize(2, abbr(s2));

    //for i := 0 to high(v) do WriteLn(v[i].s, #9, #9, v[i].n[1], #9, v[i].n[2]);

    result := distance;
    if length(s1)=length(s2) then Inc(result);
    if (PosU(str1, str2)>0) or (PosU(str2, str1)>0) then Inc(result);
    if s1=s2 then Inc(result);
    if str1=str2 then Inc(result);
end;

end.

