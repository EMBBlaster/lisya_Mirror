unit dlisp_eval;

{$mode delphi}

{$ASSERTIONS ON}


interface

uses
    {$IFDEF WINDOWS}
    Windows, ShellApi,
    {$ENDIF}
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    {$IFDEF GUI}
    Interfaces, forms,
    lisya_canvas,
    {$ENDIF}
    process,
    Classes, SysUtils, math, ucomplex,
    zstream,
    fphttpclient in './fpc_backport/fphttpclient.pp',
    regexpr in './fpc_backport/regexpr.pas'
    , dlisp_values, dlisp_read, lisya_xml, mar,
    lisya_packages
    ,lisya_predicates
    ,lisya_ifh
    ,lisya_zip
    ,lisia_charset
    ,lisya_exceptions
    ,lisya_streams
    {$IFDEF mysql55}
    ,mysql_55
    {$ENDIF}
    {$IFDEF mysql50}
    ,mysql_50
    {$ENDIF}
    ,db
    ,variants;


type

    { TEvaluationFlow }

    TEvaluationFlow = class
        stack: TVSymbolStack;
        main_stack: TVSymbolStack;

        constructor Create(parent_stack: TVSymbolStack);
        destructor Destroy; override;

        function oph_block(PL: TVList; start: integer; with_frame: boolean): TValue;
        procedure oph_bind(s, P: TValue; constant: boolean;
                        st: TVSymbolStack=nil; rests: TVRecord=nil);
        function oph_bind_to_list(s, P: TVList): TVList;
        function oph_execute_file(fn: unicodestring): boolean;
        procedure oph_bind_package(name: unicodestring; import: boolean = false);
        function oph_eval_indices(V, target: TValue): TValue;
        function oph_index(target: TVSequence; i: TValue): integer;
        procedure oph_eval_link_for_modification(out CP: TVChainPointer; P: TValue);

        function opl_elt(PL: TVList): TVChainPointer;
        function opl_key(PL: TVList): TVChainPointer;

        function op_and(PL: TVList): TValue;
    {m} function op_append(PL: TVList): TValue;
        function op_assemble(PL: TVList): TValue;
        function op_block(PL: TVList): TValue;
        function op_case(PL: TVList): TValue;
        function op_cond(PL: TVList): TValue;
        function op_const(PL: TVList): TValue;
        function op_debug(PL: TVList): TValue;
    {m} function op_default(PL: TVList): TValue;
    {m} function op_delete(PL: TVList): TValue;
        function op_elt(PL: TVList): TValue;
        function op_error(PL: TVList): TValue;
        function op_execute_file(PL: TVList): TValue;
        function op_for(PL: TVList): TValue;
        function op_goto(PL: TVList): TValue;
        function op_if(PL: TVList): TValue;
        function op_if_nil(PL: TVList): TValue;
    {m} function op_insert(PL: TVList): TValue;
        function op_key(PL: TVList): TValue;
        function op_let(PL: TVList): TValue;
        function op_macro_symbol(PL: TVList): TValue;
        function op_or(PL: TVList): TValue;
        function op_package(PL: TVList): TValue;
    {m} function op_pop(PL: TVList): TValue;
        function op_procedure(PL: TVList): TValue;
    {m} function op_push(PL: TVList): TValue;
    {m} function op_set(PL: TVList): TValue;
        function op_return(PL: TVList): TValue;
        function op_var(PL: TVList): TValue;
        function op_when(PL: TVList): TValue;
        function op_while(PL: TVList): TValue;
        function op_with(PL: TVList; export_symbols: boolean): TValue;


        function extract_body_symbols(body: TVList): TVList;
        procedure fill_subprogram_stack(sp: TVProcedure; symbols: TVList);

        function call(PL: TVList): TValue;
        function call_procedure(PL: TVList): TValue; inline;
        function call_macro({%H-}PL: TVList): TValue;
        function call_internal(PL: TVList): TValue; //inline;
        function call_operator(PL: TVList): TValue;
        function call_predicate(PL: TVList): TValue;

        function procedure_call(PL: TVList): TValue;
        function eval_parameters(call: TCallProc; PL: TVList): TValue;

        function eval_link(P: TValue): TVChainPointer;

        function eval(V: TValue): TValue;
    end;

implementation


var base_stack: TVSymbolStack = nil;
    T: TVT;
    NULL: TVList;

function value_by_key(L: TVList; key: unicodestring): TValue;
var i: integer;
begin
    i := L.High - 1;
    while i>=0 do begin
        if (tpSymbol(L.look[i]) and (key=L.uname[i]))
            or (tpString(L.look[i]) and (key=L.S[i]))
        then begin
            result := L[i+1];
            exit;
        end;
       Dec(i, 2);
    end;

    result := TVList.Create;
end;

function key_pos(L: TVList; key: unicodestring; start: integer;
    out p: integer): boolean;
begin
    result := true;
    p := L.High - 1;
    while p>=start do begin
        if (tpKeyword(L.look[p]) and (key=L.uname[p])) then exit;
        Dec(p, 2);
    end;
    result := false;
end;

function flag_exists(L :TVList; key: unicodestring; start: integer): boolean;
var i: integer;
begin
    for i := start to L.high do
        if tpKeyword(L.look[i]) and (key=L.uname[i])
        then begin
            result := true;
            exit;
        end;
    result := false;
end;

function min_list_length(L: TVList; from: integer = 0): integer;
var i: integer;
begin
    result := MaxInt;
    if tpNIL(L)
    then result := 0
    else
        for i := from to L.High do
            if L.L[i].Count < result then result := L.L[i].Count;
end;

procedure replace_value(var variable: TValue; value: TValue);
begin
    variable.Free;
    variable := value;
end;

function bind_parameters_list(PL: TVList; sign: TVList): TVList;
var i, opt_start, p: integer;
    mode: TSubprogramParmeterMode;
begin
    //TODO: bind_parameters_list не проверяет переданный список на наличие лишнего

    result := TVList.Create;
    mode := spmNec;
    for i := 0 to sign.Count-1 do
        if tpKeyword(sign.look[i])
        then begin
            // Предполагается, что эта функция получает уже верифицированную
            //сигнатуру
            if vpKeyword_KEY(sign.look[i]) then mode := spmKey;
            if vpKeyword_OPTIONAL(sign.look[i]) then mode := spmOpt;
            if vpKeyword_REST(sign.look[i]) then mode := spmRest;
            if vpKeyword_FLAG(sign.look[i]) then mode := spmFlag;
            opt_start := i;
        end
        else
            case mode of
                spmNec:
                    if i<PL.Count
                    then result.Add(PL[i])
                    else raise ELE.Create('insufficient parameters count', 'invalid parameters');
                spmOpt:
                    if (i-1)<PL.Count
                    then result.Add(PL[i-1])
                    else result.Add(TVList.Create);
                spmKey: if key_pos(PL, ':'+sign.uname[i], opt_start, p)
                    then result.Add(PL[p+1])
                    else result.Add(TVList.Create);
                spmRest:
                    result.Add(PL.subseq(i-1, PL.Count));
                spmFlag: if flag_exists(PL, ':'+sign.uname[i], opt_start)
                    then result.add(TVT.Create)
                    else result.add(TVList.Create);
            end;
end;


function valid_sign(const PL: TVList; out E: TValue; tp: array of const): integer;
var i,l, p: integer;
    case_n, cmax, cmin, tpc, Pcount: integer;
label return, next;
begin
    //  Эта функция проверяет переданный список параметров на предмет
    // соответствия параметров шаблону.
    //  Если среди параметров есть ошибки первая из них возвращается через
    // вариантный параметр E в вызвавшую функция где должна использоваться в
    // качестве результата. Наличие ошибки сигнализируется возвратом значения -1
    // через результат функции. Таким образом если каждая функция будет проверять
    // свои параметры посредством этой, то ошибки будут распространяться
    // назад по стеку вызовов.
    //  Если ошибок в параметрах нет, то производится поиск подходящего шаблона
    // номер первого подошедшего возвращается как результат. Если ни один не
    // подошёл - возвращается 0.
    //  Здесь применена новая для меня фича языка - параметр типа
    //  "array of const". Со стороны вызывающей функции выглядит как открытый
    // массив значений произвольного типа, но изнутри как массив TVarRec. Этот
    // механизм не позволяет передавать массивы и записи.


    E := nil;
    pCount := PL.Count - 1;

    case_n := 0;
    if tpNIL(PL) or (not TTypePredicate(tp[0].VPointer)(PL.look[0]))
    then goto return;
    case_n := 1;
    p := 1;
    while p<length(tp) do begin
        assert((tp[p].VType=vtinteger) and (tp[p+1].VType=vtinteger),
                'Нарушение структуры шаблона проверки типов');
        cmin := tp[p].VInteger;
        cmax := tp[p+1].VInteger;
        if cmax>=cmin then tpc := cmax else tpc := cmin + 1;
        assert(length(tp)>=(p+tpc+2),
                'Нарушение структуры шаблона проверки типов');
        if Pcount<cmin then goto next;
        if (cmax>=0) and (Pcount>cmax) then goto next;
        if Pcount<tpc then l := Pcount else l := tpc;
        for i:=0 to l-1 do
            if not TTypePredicate(tp[p+2+i].VPointer)(PL.look[i+1])
            then goto next;
            if Pcount>tpc then
                for i := tpc to Pcount do
                    if not TTypePredicate(tp[p+2+tpc-1].VPointer)(PL.look[i])
                        then goto next;
        goto return;
    next:
        Inc(case_n);
        p := p+2+tpc;
    end;
    case_n := 0;


return:
    result := case_n;
end;
//------------------------------------------------------------------------------
function params_is(PL: TVList;
                    out E: TValue;
                    const tp: array of TTypePredicate): integer;
    function pr_confirm(case_n: integer): boolean; inline;
    var i: integer;
    begin
        result := false;
        for i := (case_n-1)*PL.Count to (case_n)*PL.Count-1 do
            if not tp[i](PL.look[i mod PL.Count]) then exit;
        result := true;
    end;
var case_count, case_n :integer;
begin
    //  Эта функция проверяет переданный список параметров на предмет
    //соответствия параметров шаблону.
    //  Производится поиск подходящего шаблона
    //номер первого подошедшего возвращается как результат. Если ни один не
    //подошёл - возбуждается исключение
    E := nil;
    result := -1;

    case_count := Length(tp) div PL.Count;
    for case_n := 1 to case_count+1 do begin
        if case_n>case_count then raise ELE.InvalidParameters;

        if pr_confirm(case_n) then begin
            result := case_n;
            break;
        end;
    end;
end;
//------------------------------------------------------------------------------

//////////////////////////////////////////////
//// Internal Functions //////////////////////
//////////////////////////////////////////////


type TInternalFunctionRec = record
        n: unicodestring;
        f: TInternalFunctionBody;
        s: unicodestring;
    end;

procedure bool_to_TV(b: boolean; out E: TValue);
begin
    // Это вспомогательная функция.
    // Используется операторами сравнения, чтобы преобразовать
    // boolean в NIL или T

    if b then E := TVT.Create else E := TVList.Create;
end;


function ifh_format(const L: TVList): unicodestring;
var i: integer;
begin
    result := '';
    for i := 0 to L.High do
        if tpString(L.look[i])
        then result := result + L.S[i]
        else result := result + L.look[i].AsString();
end;

function ifh_write_string(stream: TVStreamPointer; s: unicodestring): boolean;
var i: integer;
begin
    result := true;
    //for i := 1 to Length(s) do result := stream.stream.write_char(s[i]);
    for i := 1 to Length(s) do result := stream.body.write_char(s[i]);
end;


function ifh_keyword_to_encoding(const V: TValue): TStreamEncoding;
begin
    if tpNIL(V)             then result := seUTF8 else
    if vpKeyword_UTF8(V)    then result := seUTF8 else
    if vpKeyword_UTF16BE(V) then result := seUTF16BE else
    if vpKeyword_UTF16LE(V) then result := seUTF16LE else
    if vpKeyword_UTF32BE(V) then result := seUTF32BE else
    if vpKeyword_UTF32LE(V) then result := seUTF32LE else
    if vpKeyword_CP1251(V)  then result := seCP1251 else
    if vpKeyword_CP1252(V)  then result := seCP1252 else
    if vpKeyword_CP866(V)   then result := seCP866 else
    if vpKeyword_KOI8R(V)   then result := seKOI8R else

    if vpKeyword_BOM(V)     then result := seBOM else
    raise ELE.Create('invalid encoding '+V.AsString, 'invalid parameters');
end;


function ifh_keyword_to_file_mode(const V: TValue): WORD;
begin
    if tpNIL(V)             then result := fmOpenRead else
    if vpKeyword_READ(V)    then result := fmOpenRead else
    if vpKeyword_WRITE(V)   then result := fmCreate else
    if vpKeyword_APPEND(V)  then result := fmOpenReadWrite else
    raise ELE.Create('invalid file mode '+V.AsString, 'invalid parameters');
end;

procedure ifh_elt(var C: TVCompound; ind: TValue; call: TCallProc; var indices: TIntegers);
var index, f_ind: TValue; expr, il: TVList; i,j: integer;
begin try
    index := nil;
    f_ind := nil;
    if tpSubprogram(ind)
    then try
        expr := TVList.Create([ind, C], false);
        f_ind := call(expr);
        index := f_ind;
    finally
        expr.free;
    end
    else index := ind;

    if tpListNotEmpty(index) then begin
        il := index as TVList;
        ifh_elt(C, il.look[0], call, indices);
        for i := 1 to il.high do begin
            C := C.look[indices[high(indices)]] as TVCompound;
            ifh_elt(C, il.look[i], call, indices);
        end;
    end
    else

    if tpSequence(C) and tpInteger(index) then begin
        i := (index as TVInteger).fI;
        if i in [0..(C as TVSequence).high]
        then append_integer(indices, i)
        else
            if (i<0) and (i>=-C.Count)
            then append_integer(indices, C.Count+i)
            else
                raise ELE.Create('index out of bounds', 'out of bounds');
    end
    else

    if tpSequence(C) and (C.Count>0) and (vpSymbol_LAST(index) or vpKeyword_LAST(index))
    then  append_integer(indices, C.Count-1)
    else

    if tpRecord(C) and tpSymbol(index)
    then append_integer(indices, (C as TVRecord).index_of((index as TVSymbol).N))
    else

    if tpHashTable(C)
    then append_integer(indices, (C as TVHashTable).GetIndex(index))
    else

    raise ELE.InvalidParameters;

finally
    f_ind.Free;
end; end;

function ifh_div_sequence(s: TVSequence; d: integer; tail: boolean): TVList;
var l,tl,i: integer;
begin
  result := TVList.Create;
  l := s.Count div d;
  tl := s.Count mod d;
  for i := 0 to d-1 do result.Add(s.subseq(i*l, i*l+l));
  if (tl>0) and tail then result.Add(s.subseq(s.Count-tl,s.Count));
end;

function if_structure_p         (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpRecord, tpNIL,
        tpRecord, tpRecord,
        tpAny,       tpNIL,
        tpAny,       tpRecord]) of
        1: result := TVT.Create;
        2: if (PL.look[0] as TVRecord).is_class(PL.look[1] as TVRecord)
            then result := TVT.Create
            else result := TVList.Create;
        3,4: result := TVList.Create;
    end;
end;

function if_add                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; fres: double; ires: Int64; cres: COMPLEX;
begin
        case params_is(PL, result, [
            tpListOfIntegers,
            tpListOfReals,
            tpListOfNumbers,
            tpListOfLists]) of
            1: begin
                ires := 0;
                for i := 0 to PL.L[0].high do
                    ires := ires + PL.L[0].I[i];
                result := TVInteger.Create(ires);
            end;
            2: begin
                fres := 0;
                for i := 0 to PL.L[0].high do
                    fres := fres + PL.L[0].F[i];
                result := TVFloat.Create(fres);
            end;
            3: begin
                cres := _0;
                for i := 0 to PL.L[0].high do
                    cres := cres + PL.L[0].C[i];
                result := TVComplex.Create(cres);
            end;
            4: result := ifh_union(PL.L[0]);
        end;
end;

function if_sub                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
    {1} tpInteger,  tpNIL,
    {2} tpReal,     tpNIL,
    {3} tpInteger,  tpInteger,
    {4} tpReal,     tpReal,
    {5} tpDateTime, tpDatetime,
    {6} tpDateTime, tpTimeInterval,
    {7} tpTimeInterval, tpTimeInterval,
    {8} tpNumber,   tpNumber,
    {9} tpList,     tpList]) of
        1: result := TVInteger.Create(-PL.I[0]);
        2: result := TVFloat.Create(-PL.F[0]);
        3: result := TVInteger.Create(PL.I[0] - PL.I[1]);
        4: result := TVFloat.Create(PL.F[0] - PL.F[1]);
        5,7: result := TVTimeInterval.Create(
            (PL.look[0] as TVTime).fDT - (PL.look[1] as TVTime).fDT);
        6: result := TVDateTime.Create(
            (PL.look[0] as TVTime).fDT - (PL.look[1] as TVTime).fDT);
        8: result := TVComplex.Create(PL.C[0] - PL.C[1]);
        9: result := ifh_difference(PL.L[0], PL.L[1]);
    end;
end;

function if_mul                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var i, j, k: integer; fres: double; ires: Int64; A, B, C, R: TVList;
    cres : COMPLEX;
begin
    ires := 1;
    fres := 1.0;
    A := PL.L[0];
    case params_is(PL, result, [
        tpListOfIntegers,
        tpListOfReals,
        tpListOfNumbers,
        tpListOfLists]) of
        1: begin
            for i := 0 to A.high do ires := ires * A.I[i];
            result := TVInteger.Create(ires);
        end;
        2: begin
            for i := 0 to A.high do fres := fres * A.F[i];
            result := TVFloat.Create(fres);
        end;
        3: begin
            cres.im := 0;
            cres.re := 1;
            for i := 0 to A.high do cres := cres * A.C[i];
            result := TVComplex.Create(cres);
        end;
        4: begin //декартово произведение множеств
            A := nil;
            B := nil;
            C := nil;
            R := TVList.Create;
            if PL.L[0].Count>0 then begin
                A := PL.L[0].L[0];
                for i := 0 to A.high do R.Add(TVList.Create([A[i]]));
            end;
            for j := 1 to PL.L[0].high do begin
                A := PL.L[0].L[j];
                B.Free;
                B := R.Copy as TVList;
                R.Clear;
                for i := 0 to A.high do begin
                    C := B.Copy as TVList;
                    C.CopyOnWrite;
                    for k := 0 to C.high do C.L[k].Add(A[i]);
                    R.Append(C);
                end;
            end;
            B.Free;
            result := R;
        end;
    end;
end;

function if_div                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
    {1} vpIntegerAbsOne,    tpNIL,
    {2} vpRealNotZero,      tpNIL,
    {3} vpComplexNotZero,   tpNIL,
    {4} tpInteger,          vpIntegerNotZero,
    {5} tpReal,             vpRealNotZero,
    {6} tpNumber,           vpNumberNotZero,
    {7} tpSequence,         vpIntegerPositive]) of
        1: result := TVInteger.Create(1 div PL.I[0]);
        2: result := TVFloat.Create(1 / PL.F[0]);
        3: result := TVComplex.Create(cinv(PL.C[0]));
        4: if ((PL.I[0] mod PL.I[1]) = 0)
            then result := TVInteger.Create(PL.I[0] div PL.I[1])
            else result := TVFloat.Create(PL.I[0] / PL.I[1]);
        5: result := TVFloat.Create(PL.F[0] / PL.F[1]);
        6: result := TVComplex.Create(PL.C[0] / PL.C[1]);
        7: result := ifh_div_sequence(PL.look[0] as TVSequence, PL.I[1], true);
    end;
end;

function if_div_int             (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, vpIntegerNotZero,
        tpSequence, vpIntegerPositive]) of
        1: result := TVInteger.Create(PL.I[0] div PL.I[1]);
        2: result := ifh_div_sequence(PL.look[0] as TVSequence, PL.I[1], false);
    end;
end;

function if_mod                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, vpIntegerNotZero]) of
        1: result := TVInteger.Create(PL.I[0] mod PL.I[1]);
    end;
end;

function if_abs                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger,
        tpReal,
        tpComplex]) of
        1: result := TVInteger.Create(abs(PL.I[0]));
        2: result := TVFloat.Create(abs(PL.F[0]));
        3: result := TVFloat.Create(cmod(PL.C[0]));
    end;
end;

function if_power               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger,            vpIntegerNotNegative,
        vpRealNotNegative,    tpReal,
        tpReal,               vpRealAbsOneOrMore,
        tpNumber,             tpNumber]) of
        1: result := TVInteger.Create(PL.I[0] ** PL.I[1]);
        2,3: result := TVFloat.Create(PL.F[0] ** PL.F[1]);
        4: result := TVComplex.Create(PL.C[0] ** PL.C[1]);
    end;
end;

function if_max                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; max_i: integer; max_r: double; index: integer;
begin
    case params_is(PL, result, [
        tpInteger,            tpListOfIntegers,
        tpReal,               tpListOfReals,
        tpNIL,                tpAny,
        tpListOfIntegers,     tpNIL,
        tpListOfReals,        tpNIL]) of
        1: begin
            max_i := PL.I[0];
            for i:=0 to PL.L[1].high do max_i:=max(max_i, PL.L[1].I[i]);
            result := TVInteger.Create(max_i);
        end;
        2: begin
            max_r := PL.F[0];
            for i:=0 to PL.L[1].high do max_r:=max(max_r, PL.L[1].F[i]);
            result := TVFloat.Create(max_r);
        end;
        3: result := TVList.Create;
        4: begin
            index := 0;
            max_i := PL.L[0].I[index];
            for i:=1 to PL.L[0].high do
                if PL.L[0].I[i]>max_i
                then begin index:=i; max_i:=PL.L[0].I[i]; end;
            result := TVInteger.Create(index);
        end;
        5: begin
            index := 0;
            max_r := PL.L[0].F[index];
            for i:=1 to PL.L[0].high do
                if PL.L[0].F[i]>max_r
                then begin index:=i; max_r:=PL.L[0].F[i]; end;
            result := TVInteger.Create(index);
        end;
    end;
end;

function if_min                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; min_i: integer; min_r: double; index: integer;
begin
    case params_is(PL, result, [
        tpInteger,            tpListOfIntegers,
        tpReal,               tpListOfReals,
        tpNIL,                tpAny,
        tpListOfIntegers,     tpNIL,
        tpListOfReals,        tpNIL]) of
        1: begin
            min_i := PL.I[0];
            for i:=0 to PL.L[1].high do min_i:=min(min_i, PL.L[1].I[i]);
            result := TVInteger.Create(min_i);
        end;
        2: begin
            min_r := PL.F[0];
            for i:=0 to PL.L[1].high do min_r:=min(min_r, PL.L[1].F[i]);
            result := TVFloat.Create(min_r);
        end;
        3: result := TVList.Create;
        4: begin
            index := 0;
            min_i := PL.L[0].I[index];
            for i:=1 to PL.L[0].high do
                if PL.L[0].I[i]<min_i
                then begin index:=i; min_i:=PL.L[0].I[i]; end;
            result := TVInteger.Create(index);
        end;
        5: begin
            index := 0;
            min_r := PL.L[0].F[index];
            for i:=1 to PL.L[0].high do
                if PL.L[0].F[i]<min_r
                then begin index:=i; min_r:=PL.L[0].F[i]; end;
            result := TVInteger.Create(index);
        end;
    end;
end;

function if_sqrt                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpRealNotNegative,
        tpNumber]) of
        1: result := TVFloat.Create(PL.F[0] ** 0.5);
        2: result := TVComplex.Create(csqrt(PL.C[0]));
    end;
end;

function if_round               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpReal,     tpNIL,
        tpReal,     vpIntegerRoundToRange]) of
        1: result := TVInteger.Create(round(PL.F[0]));
        2: result := TVFloat.Create(roundto(PL.F[0], TRoundToRange(PL.I[0])));
    end;
end;

function if_range               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger,            tpInteger,
        tpSequence,           tpNIL,
        vpIntegerNotNegative, tpNIL,
        vpIntegerNotNegative, tpSequence]) of
        1: result := TVRange.Create(PL.I[0], PL.I[1]);
        2: result := TVRange.Create(0, (PL.Look[0] as TVSequence).Count);
        3: result := TVRange.Create(0, PL.I[0]);
        4: result := TVRange.Create(PL.I[0], (PL.Look[1] as TVSequence).Count);
    end;
end;

function if_symbol              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString,
        tpNIL]) of
        1: begin
            result := read_from_string(PL.S[0]);
            if not tpSymbol(result) then begin
                FreeAndNil(result);
                raise ELE.Create('invalid symbol name', 'syntax');
            end;
        end;
        2: result := TVSymbol.Gensym;
    end;
end;

function if_random              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative,
        tpListNotEmpty,
        tpNIL]) of
        1: result := TVInteger.Create(system.Random(PL.I[0]));
        2: result := PL.L[0][system.Random(PL.L[0].Count)];
        3: result := TVFloat.Create(system.Random);
    end;
end;

function if_re                  (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpReal,
        tpComplex]) of
        1: result := PL[0];
        2: result := TVFloat.Create((PL.look[0] as TVComplex).C.re);
    end;
end;

function if_im                  (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpReal,
        tpComplex]) of
        1: result := TVFloat.Create(0);
        2: result := TVFloat.Create((PL.look[0] as TVComplex).C.im);
    end;
end;

function if_hash                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpAny]) of
        1: result := TVInteger.Create(PL.look[0].hash);
    end;
end;

function if_split_string        (const PL: TVList; {%H-}call: TCallProc): TValue;
var sa: TStringArray; var i: integer;
begin
    case params_is(PL, result, [
        tpString, tpNIL,
        tpString, tpString]) of
        1,2: begin
            result := TVList.Create;
            if tpNIL(PL.look[1])
            then sa := SplitString(PL.S[0], ' ')
            else sa := SplitString(PL.S[0], PL.S[1]);
            for i := 0 to high(sa) do
                (result as TVList).Add(TVString.Create(sa[i]));
        end;
    end;
end;

function if_trim                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(trim(PL.S[0]));
    end;
end;

function if_equal               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [tpAny, tpAny]) of
        1: if equal(PL.look[0], PL.look[1])
            then result := TVT.Create
            else result := TVList.Create;
    end;
end;

function if_more                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]>PL.I[1] , result);
        2: bool_to_TV( PL.F[0]>PL.F[1] , result);
        3: bool_to_TV( PL.S[0]>PL.S[1] , result);
    end;
end;

function if_less                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]<PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<PL.S[1] , result);
    end;
end;

function if_more_or_equal       (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString,
        tpList,    tpList]) of
        1: bool_to_TV( PL.I[0]>=PL.I[1] , result);
        2: bool_to_TV( PL.F[0]>=PL.F[1] , result);
        3: bool_to_TV( PL.S[0]>=PL.S[1] , result);
        4: bool_to_TV(ifh_set_include(PL.L[0], PL.L[1]), result);
    end;
end;

function if_less_or_equal       (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString,
        tpList,    tpList]) of
        1: bool_to_TV( PL.I[0]<=PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<=PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<=PL.S[1] , result);
        4: bool_to_TV(ifh_set_include(PL.L[1], PL.L[0]), result);
    end;
end;

function if_not_equal           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString,
        tpBytes,   tpBytes]) of
        1: bool_to_TV( PL.I[0]<>PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<>PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<>PL.S[1] , result);
        4: bool_to_TV(not equal(PL.look[0], PL.look[1]), result);
    end;
end;

function if_and                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        tpNIL,
        tpList]) of
        1: result := TVT.Create;
        2: begin
            for i := 0 to PL.L[0].high do
                if tpNIL(PL.L[0].look[i]) then begin
                    result := TVList.Create;
                    Exit;
                end;
            result := PL.L[0][PL.L[0].high];
        end;
    end;
end;

function if_or                  (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        tpNIL,
        tpList]) of
        1: result := TVList.Create;
        2: begin
            for i := 0 to PL.L[0].high do
                if tpTrue(PL.L[0].look[i]) then begin
                    result := PL.L[0][i];
                    Exit;
                end;
            result := TVList.Create;
        end;
    end;
end;

function if_xor                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var res, a: boolean; i, n: integer;
begin
    case params_is(PL, result, [
        tpNIL,
        tpList]) of
        1: result := TVList.Create;
        2: begin
            res := tpTrue(PL.L[0].look[0]);
            n := 0;
            for i:=1 to PL.L[0].high do begin
                a := tpTrue(PL.L[0].look[i]);
                res := res xor a;
                if a then n := i; //положение последнего значащего аргумента
            end;
            if res
            then result := PL.L[0][n]
            else result := TVList.Create;
        end;
    end;
end;

function if_not                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpNIL,
        tpTrue]) of
        1: result := TVT.Create;
        2: result := TVList.Create;
    end;
end;

function if_equal_case_insensitive(const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString, tpString]) of
        1: if UnicodeUpperCase(PL.S[0])=UnicodeUpperCase(PL.S[1])
            then result := TVT.Create
            else result := TVList.Create;
    end;
end;

function if_equal_sets            (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; r: boolean; A, B: TVList;
begin
    case params_is(PL, result, [
        tpNIL, tpNIL,
        tpList, tpList]) of
        1: result := TVT.Create;
        2: begin
            r := true;
            A := PL.L[0];
            B := PL.L[1];
            for i := 0 to A.high do
                if not ifh_member(B, A.look[i])
                then begin
                    r := false;
                    break;
                end;
            if r then for i := 0 to B.high do
                if not ifh_member(A, B.look[i])
                then begin
                    r := false;
                    break;
                end;
            bool_to_TV(r, result);
        end;
    end;
end;


function if_length_more           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpList, tpList,
        tpString, tpString]) of
        1: bool_to_TV(PL.L[0].Count > PL.L[1].Count, result);
        2: bool_to_TV(Length(PL.S[0]) > Length(PL.S[1]), result);
    end;
end;

function if_length_less           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpList, tpList,
        tpString, tpString]) of
        1: bool_to_TV(PL.L[0].Count < PL.L[1].Count, result);
        2: bool_to_TV(Length(PL.S[0]) < Length(PL.S[1]), result);
    end;
end;

function if_identity              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpAny]) of
        1: result := PL[0];
    end;
end;


function if_test_dyn            (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
{$IFDEF GUI}
    Application.Initialize;
    Application.CreateForm(TCanvasForm, CanvasForm);
    for i:= 0 to PL.L[0].high do
    CanvasForm.Post(PL.L[0].L[i]);
    Application.Run;
{$ENDIF}
    result := TVT.Create;
end;

function if_likeness              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString, tpString]) of
        1: result := TVInteger.Create(ifh_like(PL.S[0],PL.S[1]));
    end;
end;


function if_extract_file_ext    (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(ExtractFileExt(PL.S[0]));
    end;
end;

function if_extract_file_name   (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(ExtractFileName(PL.S[0]));
    end;
end;

function if_extract_file_path   (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(DirSep(ExtractFilePath(PL.S[0])));
    end;
end;

function if_file_exists         (const PL: TVList; {%H-}call: TCallProc): TValue;
var fn: unicodestring;
begin
    case params_is (PL, result, [
        tpString]) of
        1: begin
            fn := ExpandFileName(DirSep(PL.S[0]));
            if FileExists(fn)
            then begin
                if (FileGetAttr(fn) and faDirectory)<>0
                then result := TVString.Create(IncludeTrailingPathDelimiter(fn))
                else result := TVString.Create(fn);
            end
            else result := TVList.create;
        end;
    end;
end;

function if_directory_exists    (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if DirectoryExists(DirSep(PL.S[0]))
            then result := TVString.Create(DirSep(PL.S[0]))
            else result := TVList.create;
    end;
end;

function if_command_line        (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    Assert(PL.Count=0, 'command-line не нуждается в параметрах');
    result := TVList.Create;
    for i := 2 to paramCount do
        (result as TVList).Add(TVString.Create(paramStr(i)));
end;

function if_environment_variable(const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: result := TVString.Create(GetEnvironmentVariable(PL.S[0]));
    end;
end;

function if_change_directory    (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if SetCurrentDir(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('change-directory error: '+PL.S[0]);
    end;
end;

function if_delete_file         (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if DeleteFile(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('delete-file error: '+PL.S[0]);
    end;
end;

function if_rename_file         (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString, tpString]) of
        1: if RenameFile(DirSep(PL.S[0]), DirSep(PL.S[1]))
            then result := TVT.Create
            else raise ELE.Create('rename-file error: '+PL.S[0]);
    end;
end;

function if_create_directory    (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if CreateDir(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('create-directory error: '+PL.S[0]);
    end;
end;

function if_remove_directory    (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if RemoveDir(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('remove-directory error: '+PL.S[0]);
    end;
end;

function if_guid                (const PL: TVList; {%H-}call: TCallProc): TValue;
var g: TGUID;
begin
    Assert(PL.Count=0, 'GUID не нуждается в параметрах');
    if CreateGUID(g)=0
    then result := TVSymbol.Create(GUIDToString(g))
    else raise ELE.Create('GUID error');
end;


function if_every               (const PL: TVList; call: TCallProc): TValue;  //HIGH
var i, j, count: integer; tmp: TVList;
begin
    case params_is(PL, result, [
        tpSubprogram, tpListOfLists]) of
        1: begin
            count := min_list_length(PL.L[1]);
            for i := 0 to count-1 do begin
                result := TVList.Create([PL[0]]);
                (result as TVList).SetCapacity(1+PL.L[1].Count);
                for j := 0 to PL.L[1].High do
                    (result as TVList).Add(PL.L[1].L[j][i]);
                tmp := result as TVList;
                result := call(tmp);
                tmp.Free;
                if tpNil(result) then exit;
                result.Free;
            end;
            result := TVT.Create;
        end;
    end;
end;

function if_some                (const PL: TVList; call: TCallProc): TValue;  //HIGH
var i, j, count: integer; tmp: TVList;
begin
    case params_is(PL, result, [
        tpSubprogram, tpListOfLists]) of
        1: begin
            count := min_list_length(PL.L[1]);
            for i := 0 to count-1 do begin
                result := TVList.Create([PL[0]]);
                (result as TVList).SetCapacity(1+PL.L[1].Count);
                for j := 0 to PL.L[1].High do
                    (result as TVList).Add(PL.L[1].L[j][i]);
                tmp := result as TVList;
                result := call(tmp);
                tmp.Free;
                if not tpNil(result) then exit;
                result.Free;
            end;
            result := TVList.Create;
        end;
    end;
end;

function if_car                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
    {1} tpNIL,
    {2} tpList,
    {3} tpAny]) of
        1: result := TVList.Create;
        2: result := PL.L[0][0];
        3: result := PL[0];
    end;
end;

function if_last                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
    {1} vpSequenceNotEmpty]) of
        1: result := (PL.look[0] as TVSequence)[(PL.look[0] as TVSequence).high];
    end;
end;


function if_subseq              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpSequence, vpIntegerNotNegative, tpNIL,
        tpSequence, vpIntegerNotNegative, vpIntegerNotNegative]) of
        1: result := (PL.look[0] as TVSequence).subseq(PL.I[1]);
        2: result := (PL.look[0] as TVSequence).subseq(PL.I[1], PL.I[2]);
    end;
end;


function if_sort                 (const PL: TVList; call: TCallProc): TValue; //HIGH
var list: array of TValue; expr: TVList; if_compare: TInternalFunctionBody;

    procedure to_list;
    var i: integer;
    begin
        SetLength(list, PL.L[0].Count);
        for i := 0 to PL.L[0].high do list[i] := PL.L[0].look[i];
    end;

    procedure to_result;
    var i: integer;
    begin
        result := TVList.Create;
        (result as TVList).SetCapacity(Length(list));
        for i := 0 to high(list) do (result as TVList).Add(list[i].Copy);
    end;

    procedure swap(a, b: integer); inline;
    var tmp: TValue;
    begin
        tmp := list[a];
        list[a] := list[b];
        list[b] := tmp;
    end;

    function compare(e: integer): boolean;
    var tmp: TValue;
    begin
        expr[1] := list[e];
        try
            tmp := nil;
            tmp := call(expr);
            result := tpTrue(tmp);
        finally
            tmp.Free;
        end;
    end;

    procedure sort(lo,hi: integer);
    var a, b: integer;
    begin
        if lo>=hi then exit;

        expr[2] := list[hi];

        a := lo;
        b := hi;
        for b := lo to hi-1 do
            if compare(b) then begin
                swap(a,b);
                Inc(a);
            end;
        swap(a,hi);

        sort(lo,a-1);
        sort(a+1,hi);
    end;

    function compare_by_internal(e: integer): boolean;
    var tmp: TValue;
    begin
        expr[0] := list[e];
        try
            tmp := nil;
            tmp := if_compare(expr, nil);
            result := tpTrue(tmp);
        finally
            tmp.Free;
        end;
    end;

    procedure sort_by_internal(lo,hi: integer);
    var a, b: integer;
    begin
        if lo>=hi then exit;

        expr[1] := list[hi];

        a := lo;
        b := hi;
        for b := lo to hi-1 do
            if compare_by_internal(b) then begin
                swap(a,b);
                Inc(a);
            end;
        swap(a,hi);

        sort_by_internal(lo,a-1);
        sort_by_internal(a+1,hi);
    end;

begin try
    expr := nil;
    if_compare := nil;
    case params_is(PL, result, [
        tpList, tpInternalFunction,
        tpList, tpSubprogram,
        tpList, tpNIL]) of
        1: begin
            to_list;
            if_compare := (PL.look[1] as TVInternalFunction).body;
            expr := TVList.Create([nil, nil], false);
            sort_by_internal(0, high(list));
            to_result;
        end;
        2: begin
            to_list;
            expr := TVList.Create([PL.look[1], nil, nil], false);
            sort(0, high(list));
            to_result;
        end;
        3: begin
            to_list;
            if_compare := if_less;
            expr := TVList.Create([nil, nil], false);
            sort_by_internal(0, high(list));
            to_result;
        end;
    end;

finally
    expr.Free;
end;
end;

function if_slots               (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        tpRecord,
        tpHashTable]) of
        1: begin
            result := TVList.Create;
            for i := 0 to (PL.look[0] as TVRecord).count-1 do
                (result as TVList).Add(
                    TVSymbol.Create((PL.look[0] as TVRecord).name_n(i)));
        end;
        2: result := (PL.look[0] as TVHashTable).GetKeys;
    end;
end;

function if_curry               (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; f: TVProcedure; bind: TBindings;
    procedure del_sym(L: TVList; n: integer);
    var i: integer;
    begin
        for i := L.high downto 0 do
            if tpList(L.look[i]) then del_sym(L.L[i], n)
            else
                if L.SYM[i].N=n then L.delete(i);
    end;
begin
    case params_is(PL, result, [
        tpProcedure, tpList]) of
        1: try
            result := PL[0];
            f := result as TVProcedure;
            f.sign.CopyOnWrite;
            f.nN := 0;

            bind := ifh_bind(f.sign,PL.L[1]);
            for i := 0 to high(bind) do begin
                if vpSymbol__(bind[i].V)
                then bind[i].V.Free
                else begin
                    if bind[i].rest
                    then (f.rests.LookSlot(bind[i].nN) as TVList).Append(bind[i].V as TVList)
                    else begin
                        f.stack.new_var(bind[i].nN, bind[i].V, true);
                        del_sym(f.sign, bind[i].nN);
                    end;
                end;
            end;
        finally

        end;
    end;
end;

function ifh_filter(const PL: TVList; call: TCallProc; P: TTypePredicate): TValue; inline;
var expr: TVList; c: TValue; i: integer;
begin
    case params_is(PL, result, [
        tpSubprogram, tpList]) of
        1: try
            c := nil;
            result := TVList.Create;
            expr := TVList.Create([PL[0],nil]);
            for i := 0 to PL.L[1].high do begin
                expr[1] := PL.L[1][i];
                c := call(expr);
                if P(c) then (result as TVList).Add(PL.L[1][i]);
                FreeAndNil(c);
            end;
        finally
            expr.Free;
            c.Free;
        end;
    end;
end;


function if_filter              (const PL: TVList; call: TCallProc): TValue;
begin
    result := ifh_filter(PL, call, tpTrue);
end;

function if_reject              (const PL: TVList; call: TCallProc): TValue;
begin
    result := ifh_filter(PL, call, tpNIL);
end;

function if_fold                (const PL: TVList; call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpSubprogram, tpList]) of
        1: case PL.L[1].Count of
            0: result := TVList.Create;
            1: result := PL.L[1][0];
            else result := ifh_fold(call, PL.look[0] as TVSubprogram, PL.L[1], 0, PL.L[1].high);
        end;
    end;
end;

function if_map                 (const PL: TVList; call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpSubprogram, tpNIL,
        tpSubprogram, vpListOfListsEqualLength]) of
        1: result := TVList.Create;
        2: result := ifh_map(
                call,
                PL.look[0] as TVSubprogram,
                PL.L[1],
                0, PL.L[1].L[0].high); //эти параметры рудимент многопоточности
    end;
end;

function if_map_concatenate     (const PL: TVList; call: TCallProc): TValue;
var L: TVList; i: integer;
begin
    L := if_map(PL, call) as TVList;
    result := TVList.Create;
    for i := 0 to L.high do (result as TVList).Append(L[i] as TVList);
    L.Free;
end;

function if_apply               (const PL: TVList; call: TCallProc): TValue;
var expr: TVList; i: integer;
begin
    case params_is(PL, result, [
        tpSubprogram, vpListWithLastList]) of
        1: try
            expr := TVList.Create([PL.look[0]], false);
            for i := 0 to PL.L[1].high-1 do expr.Add(PL.L[1].look[i]);
            expr.Append(PL.L[1].L[PL.L[1].high]);
            result := call(expr);
        finally
            expr.Free;
        end;
    end;
end;

function if_union               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [tpListOfLists]) of
        1: result := ifh_union(PL.L[0]);
    end;
end;

function if_intersection        (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [tpNIL, tpListOfLists]) of
        1: result := TVList.Create;
        2: result := ifh_intersection(PL.L[0]);
    end;
end;

function if_difference          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [tpList, tpList]) of
        1: result := ifh_difference(PL.L[0], PL.L[1]);
    end;
end;

function if_reverse             (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; A, R: TVList;
begin
    case params_is(PL, result, [tpList]) of
        1: begin
            A := PL.L[0];
            R := TVList.Create;
            result := R;
            R.SetCapacity(A.Count);
            for i := A.high downto 0 do R.Add(A[i]);
        end;
    end;
end;

function if_member              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [tpList, tpAny]) of
        1: if ifh_member(PL.L[0], PL.look[1])
            then result := PL[1]
            else result := TVList.Create;
    end;
end;

function if_position            (const PL: TVList; {%H-}call: TCallProc): TValue;
var i, p: integer;
begin
    p := -1;
    case params_is(PL, result, [
    {1} tpList, tpAny,
    {2} tpString, tpString]) of
        1: for i := 0 to PL.L[0].high do
            if equal(PL.L[0].look[i], PL.look[1])
            then begin p := i; break; end;
        2: p := PosU(PL.S[1], PL.S[0]) - 1;
    end;
    if p>=0 then result := TVInteger.Create(p) else result := TVList.Create;
end;

function if_length              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpSequence]) of
        1: result := TVInteger.Create((PL.look[0] as TVSequence).Count);
    end;
end;

function if_list                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpList]) of
        1: result := PL.L[0].Copy;
    end;
end;

function if_empty_list          (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative, tpAny]) of
        1: begin
            result := TVList.Create;
            (result as TVList).SetCapacity(PL.I[0]);
            for i := 0 to PL.I[0]-1 do (result as TVList).Add(PL[1]);
        end;
    end;
end;

function if_record              (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        vpListSymbolValue]) of
        1: begin
            result := TVRecord.Create;
            i := 0;
            while i<(PL.L[0].Count div 2) do begin
                (result as TVRecord).AddSlot(PL.L[0].SYM[i*2], PL.L[0][i*2+1]);
                Inc(i);
            end;
        end;
    end;
end;

function if_record_as           (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        tpRecord, vpListSymbolValue]) of
        1: begin
            result := PL[0];
            i := 0;
            while i<(PL.L[1].Count div 2) do begin
                (result as TVRecord).slot[PL.L[1].SYM[i*2].N] := PL.L[1][i*2+1];
                Inc(i);
            end;
        end;
    end;
end;

function if_hash_table          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    assert(PL.Count=0, 'Invalid parameters');
    result := TVHashTable.Create;
end;

function if_concatenate         (const PL: TVList; {%H-}call: TCallProc): TValue;
var sres: unicodestring; i,j: integer;
begin
    case params_is(PL, result, [
        tpListOfLists,
        tpListOfStrings,
        tpListOfByteses]) of
        1: begin
            result := TVList.Create;
            for i := 0 to PL.L[0].high do
                (result as TVList).Append(PL.L[0][i] as TVList);
        end;
        2: begin
            sres := '';
            for i := 0 to PL.L[0].high do
                sres := sres + PL.L[0].S[i];
            result := TVString.Create(sres);
        end;
        3: begin
            result := TVBytes.Create;
            //TODO: неэффективная конкатенация TVByteVector
            for i := 0 to PL.L[0].high do begin
                for j := 0 to (PL.L[0].look[i] as TVBytes).High do
                    (result as TVBytes).Add(
                        (PL.L[0].look[i] as TVBytes).bytes[j]);
            end;
        end;
    end;
end;

function if_group               (const PL: TVList; call: TCallProc): TValue;
var i, p: integer; cnd: TValue;
    source, predicates, groups, group, expr: TVList;
begin
    cnd := nil;
    expr := nil;
    //функция группировки. Разбивает переданный список на группы в соответствии
    //с переданными предикатами. Элементы, соответствующие более чем одному
    //предикату попадают в первую подходящую группу, не подошедшие никуда
    // попадают в последнюю
    case params_is(PL, result, [
        tpList, tpListOfSubprograms]) of
        1: try
            source := PL.L[0].Copy as TVList;
            predicates := PL.L[1];

            expr := TVList.Create([nil,nil],false);
            result := TVList.Create;
            groups := result as TVList;
            groups.SetCapacity(predicates.Count+1);
            for p := 0 to predicates.high do begin
                expr[0] := predicates.look[p];
                group := TVList.Create;
                for i := source.high downto 0 do begin
                    expr[1] := source.look[i];
                    FreeAndNil(cnd);
                    cnd := call(expr);
                    if tpTRUE(cnd) then begin
                        group.Add(source[i]);
                        source.delete(i);
                    end;
                end;
                groups.Add(group);
            end;
        finally
            groups.Add(source);
            cnd.Free;
            expr.Free;
        end;
    end;
end;

function if_strings_mismatch    (const PL: TVList; {%H-}call: TCallProc): TValue;
var mm: integer;
begin
    case params_is(PL, result, [
        tpString, tpString, tpNIL,
        tpString, tpString, vpKeyword_CASE_INSENSITIVE]) of
        1: mm := ifh_strings_mismatch(PL.S[0],PL.S[1]);
        2: mm := ifh_strings_mismatch(UnicodeUpperCase(PL.S[0]),UnicodeUpperCase(PL.S[1]));
    end;
    if mm<0
    then result := TVList.Create
    else result := TVInteger.Create(mm);
end;

function if_associations        (const PL: TVList; {%H-}call: TCallProc): TValue;
var res, L,ll: TVList; i, j: integer; lazy,by_head: boolean; k: TValue;
begin
    case params_is(PL, result, [
        tpListOfLists, tpAny, tpBoolean, tpBoolean]) of
        1: try
            L := PL.L[0];
            k := PL.look[1];
            lazy := tpTrue(PL.look[2]);
            by_head := tpTrue(PL.look[3]);
            res := TVList.Create;
            for i := L.high downto 0 do
                for j := 0 to L.L[i].high do begin
                    if equal(L.L[i].look[j], k) then begin
                        ll := L[i] as TVList;
                        ll.delete(j);
                        res.Append(ll);
                        if lazy then Exit;
                    end;
                    if by_head then break;
                end;
        finally
            result := res;
        end;
    end;

end;

function if_subrange            (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        tpListOfReals, tpReal]) of
        1: begin
            for i := 0 to PL.L[0].high do
                if PL.F[1]<=PL.L[0].F[i] then begin
                    result := TVInteger.Create(i);
                    Exit;
                end;
            result := TVInteger.Create(PL.L[0].Count);
        end;
    end;
end;

function if_element             (const PL: TVList; {%H-}call: TCallProc): TValue;
var C: TVCompound; i: TIntegers;
begin
    case params_is(PL, result, [
        tpCompound, tpNIL,
        tpCompound, tpList]) of
        1: result := PL[0];
        2: begin
            C := PL.look[0] as TVCompound;
            i := nil;
            ifh_elt(C, PL.look[1], call, i);
            if i=nil
            then result := TVList.Create
            else result := C[i[high(i)]];
        end;
    end;
end;


function if_bytes               (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        vpListOfByte]) of
        1: begin
            result := TVBytes.Create;
            (result as TVBytes).SetCount(PL.L[0].Count);
            (result as TVBytes).SetCount(0);
            for i:= 0 to PL.L[0].High do
                (result as TVBytes).Add(PL.L[0].I[i]);
        end;
    end;
end;

function if_bitwise_or          (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a, b: TVBytes;
begin
    //TODO: побитовые операторы имеют много общего кода нужно разделить
    case params_is(PL, result, [
        tpBytes,   tpBytes,
        tpInteger, tpInteger]) of
        1: begin
            a := PL.look[0] as TVBytes;
            b := PL.look[1] as TVBytes;
            if a.Count<>b.Count then raise ELE.Create('inequal length', 'invalid parameters');
            result := TVBytes.Create;
            (result as TVBytes).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVBytes).fBytes[i] := a.fBytes[i] or b.fBytes[i];
        end;
        2: result := TVInteger.Create(PL.I[0] or PL.I[1]);
    end;
end;

function if_bitwise_not         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a: TVBytes;
begin
    case params_is(PL, result, [
        tpBytes,
        tpInteger]) of
        1: begin
            a := PL.look[0] as TVBytes;
            result := TVBytes.Create;
            (result as TVBytes).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVBytes).fBytes[i] := not a.fBytes[i];
        end;
        2: result := TVInteger.Create(not PL.I[0]);
    end;
end;

function if_bitwise_and         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a, b: TVBytes;
begin
    case params_is(PL, result, [
        tpBytes,   tpBytes,
        tpInteger, tpInteger]) of
        1: begin
            a := PL.look[0] as TVBytes;
            b := PL.look[1] as TVBytes;
            if a.Count<>b.Count then raise ELE.Create('inequal length', 'invalid parameters');
            result := TVBytes.Create;
            (result as TVBytes).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVBytes).fBytes[i] := a.fBytes[i] and b.fBytes[i];
        end;
        2: result := TVInteger.Create(PL.I[0] and PL.I[1]);
    end;
end;

function if_bitwise_xor         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a, b: TVBytes;
begin
    case params_is(PL, result, [
        tpBytes,   tpBytes,
        tpInteger, tpInteger]) of
        1: begin
            a := PL.look[0] as TVBytes;
            b := PL.look[1] as TVBytes;
            if a.Count<>b.Count then raise ELE.Create('inequal length', 'invalid parameters');
            result := TVBytes.Create;
            (result as TVBytes).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVBytes).fBytes[i] := a.fBytes[i] xor b.fBytes[i];
        end;
        2: result := TVInteger.Create(PL.I[0] xor PL.I[1]);
    end;
end;

function if_crc32               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpBytes,
        tpString]) of
        1: result := TVInteger.Create((PL.look[0] as TVBytes).crc32);
        2: result := TVInteger.Create((PL.look[0] as TVString).crc32);
    end;
end;

function if_character           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative]) of
        1: result := TVString.Create(unicodechar(PL.I[0]));
    end;
end;

function if_byte_vector_to_string(const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpBytes, vpKeywordEncodingOrNIL]) of
        1: result := TVString.Create(
                lisia_charset.bytes_to_string(
                    (PL.look[0] as TVBytes).fBytes,
                    ifh_keyword_to_encoding(PL.look[1])));
    end;
end;

function if_assertion           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpTrue, tpList,
        tpNIL,  tpList]) of
        1: result := TVT.Create;
        2: raise ELE.Create(ifh_format(PL.L[1]), 'assertion');
    end;
end;

function if_documentation       (const PL: TVList; {%H-}call: TCallProc): TValue;
var proc: TVProcedure; int_fun: TVInternalFunction;
begin
    case params_is(PL, result, [
        tpProcedure,
        tpInternalFunction]) of
        1: begin
            proc := PL.look[0] as TVProcedure;
            WriteLn(proc.AsString);
            WriteLn(proc.sign.AsString);
            if tpString(proc.body.look[0]) then WriteLn(proc.body.look[0].AsString);
            WriteLn(proc.rests.AsString);
        end;
        2: begin
            int_fun := PL.look[0] as TVInternalFunction;
            WriteLn(int_fun.AsString);
            WriteLn(int_fun.signature.AsString);
        end;
    end;
    result := TVT.Create;
end;

function if_directory           (const PL: TVList; {%H-}call: TCallProc): TValue;
var sr: TSearchRec; found: boolean; dir, fn, wc: unicodestring;
    exclude_files: boolean;
label next;
begin
    case params_is(PL, result, [
        tpString]) of
        1: begin
            wc := DirSep(PL.S[0]);
            if wc[Length(wc)]=dir_separator
            then begin
                SetLength(wc, Length(wc)-1);
                exclude_files := true;
            end
            else exclude_files := false;

            dir := ExpandFileName(ExtractFileDir(wc));
            result := TVList.Create;

            found := findfirst(wc, faAnyFile, sr)=0;
            while found do begin
                fn := IncludeTrailingPathDelimiter(dir)+sr.name;
                if (sr.name='..') or (sr.name='.') then goto next;
                if exclude_files and ((sr.Attr and faDirectory)=0)
                    then goto next;
                //TODO: не кросплатформенный способ обнаружения скрытых файлов
                //if (((PL.Count>=3) and tpNIL(PL.look[2])) or (PL.Count=2))
                //    and (sr.name[1]='.')
                //then goto next;
                if (sr.Attr and faDirectory)<>0
                then (result as TVList).Add(
                            TVString.Create(IncludeTrailingPathDelimiter(fn)))
                else (result as TVList).Add(
                            TVString.Create(fn));
                next:
                found := findNext(sr)=0;
            end;
            FindClose(sr);
        end;

    end;
end;

function if_sleep               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative]) of
        1: begin
            Sleep(PL.I[0]);
            result := TVT.Create;
        end;
    end;
end;


function if_run_command         (const PL: TVList; {%H-}call: TCallProc): TValue;
var output: string;
    {$IFDEF WINDOWS}
    procedure run(cmdln, dir: unicodestring);
    var cmd, params: unicodestring; p, i, h: integer; q: boolean;
    begin
        q := false;
        p := Length(cmdln)-1;
        for i := 1 to length(cmdln) do
            case cmdln[i] of
                '"': q := not q;
                ' ': if not q then begin
                    p := i;
                    Break;
                end;
            end;
        cmd := cmdln[1..p-1];
        params := cmdln[p+1..length(cmdln)];
        if dir<>''
        then h := ShellExecuteW(0, nil, PWideChar(cmd), PWideChar(params), PWideChar(dir), 4)
        else h := ShellExecuteW(0, nil, PWideChar(cmd), PWideChar(params), nil, 4);

        case h of
            0: raise ELE.Create('0', 'ShellExecute');
            ERROR_FILE_NOT_FOUND: raise ELE.Create('ERROR_FILE_NOT_FOUND', 'ShellExecute');
            ERROR_PATH_NOT_FOUND: raise ELE.Create('ERROR_PATH_NOT_FOUND', 'ShellExecute');
            ERROR_BAD_FORMAT: raise ELE.Create('ERROR_BAD_FORMAT', 'ShellExecute');
            SE_ERR_ACCESSDENIED : raise ELE.Create('SE_ERR_ACCESSDENIED', 'ShellExecute');
            SE_ERR_ASSOCINCOMPLETE: raise ELE.Create('SE_ERR_ASSOCINCOMPLETE', 'ShellExecute');
            SE_ERR_DDEBUSY: raise ELE.Create('SE_ERR_DDEBUSY', 'ShellExecute');
            SE_ERR_DDEFAIL: raise ELE.Create('SE_ERR_DDEFAIL', 'ShellExecute');
            SE_ERR_DDETIMEOUT: raise ELE.Create('SE_ERR_DDETIMEOUT', 'ShellExecute');
            SE_ERR_DLLNOTFOUND: raise ELE.Create('SE_ERR_DLLNOTFOUND', 'ShellExecute');
            SE_ERR_NOASSOC: raise ELE.Create('SE_ERR_NOASSOC', 'ShellExecute');
            SE_ERR_OOM: raise ELE.Create('SE_ERR_OOM', 'ShellExecute');
            SE_ERR_SHARE: raise ELE.Create('SE_ERR_SHARE', 'ShellExecute');
        end;
    end;
    {$ENDIF}

begin
    result := nil;
    case params_is(PL, result, [
        tpString, tpString,
        tpString, tpNIL]) of
        {$IFDEF WINDOWS}
        1: run(PL.S[0], PL.S[1]);
        2: run(PL.S[0], '');
        {$ELSE}
        1: {%H-}process.RunCommandInDir(PL.S[1], PL.S[0], output{%H-});
        2: {%H-}process.RunCommand(PL.S[0], output);
        {$ENDIF}
    end;
    result := TVString.Create(output);
end;

function if_now                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    Assert(PL.Count=0, 'NOW не нуждается в параметрах');
    result := TVDateTime.Create(now);
end;


function if_open_file           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString, vpKeywordFileModeOrNIL, vpKeywordEncodingOrNIL]) of
        1: result := TVStreamPointer.Create(
            TLFileStream.Create(
                        DirSep(PL.S[0]),
                        ifh_keyword_to_file_mode(PL.look[1]),
                        ifh_keyword_to_encoding(PL.look[2])));
    end;
end;


function if_set_encoding        (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
    {1} vpStreamPointerActive, tpKeywordOrNIL]) of
        1: begin
            (PL.look[0] as TVStreamPointer).body.Encoding :=
                                            ifh_keyword_to_encoding(PL.look[1]);
            result := TVT.Create;
        end;
    end;
end;

function if_zip_open            (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString, vpKeywordFileModeOrNIL, vpKeywordEncodingOrNIL]) of
        1: result := TVZipArchivePointer.Create(
                TZipArchive.Open(DirSep(PL.S[0]),
                    ifh_keyword_to_file_mode(PL.look[1]),
                    ifh_keyword_to_encoding(PL.look[2])));
    end;
end;

function if_zip_filelist        (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; file_names: TStringArray;
begin
    case params_is(PL, result, [
        tpZIPArchivePointer]) of
        1: begin
            file_names := (PL.look[0] as TVZIPArchivePointer).Z.FileList;
            result := TVList.Create;
            for i := 0 to high(file_names) do
                (result as TVList).Add(TVString.Create(file_names[i]));
        end;
    end;
end;


function if_zip_file            (const PL: TVList; {%H-}call: TCallProc): TValue;
var zf: TLZIPFile;  enc: TStreamEncoding; mode: WORD;
begin
    case params_is(PL, result, [
        tpZIPArchivePointer, tpString, vpKeywordFileModeOrNIL, vpKeywordEncodingOrNIL]) of
        1: begin
            mode := ifh_keyword_to_file_mode(PL.look[2]);
            enc := ifh_keyword_to_encoding(PL.look[3]);

            zf := TLZipFile.Create(
                (PL.look[0] as TVZIPArchivePointer).Z.Ref as TZIPArchive,
                PL.S[1], mode, enc);
            if zf.active
            then result := TVStreamPointer.Create(zf)
            else begin
                zf.Free;
                result := TVList.Create;
            end;
        end;
    end;
end;

function if_zip_delete          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpZIPArchivePointer, tpString]) of
        1: begin
            (PL.look[0] as TVZipArchivePointer).Z.Delete(PL.S[1]);
            result := TVT.Create;
        end;
    end;
end;

function if_close_stream        (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpStreamPointer]) of
        1: begin
            (PL.look[0] as TVStreamPointer).close_stream;
            result := TVT.Create;
        end;
    end;
end;


function if_deflate             (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; bv: TVBytes; cs: TCompressionStream; ms: TMemoryStream;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpKeywordOrNIL, tpAny,
        tpBytes,               tpKeywordOrNil, tpAny,
        tpStreamPointer, tpKeywordOrNIL, tpAny]) of
        1:  begin
            result := TVStreamPointer.Create(
                    TLDeflateStream.Create(
                        (PL.look[0] as TVStreamPointer).body.Ref as TLStream,
                        tpTrue(PL.look[2]),
                        ifh_keyword_to_encoding(PL.look[1])));
        end;
        2: begin
            bv := PL.look[0] as TVBytes;
            ms := TMemoryStream.Create;
            ms.Position :=0 ;
            cs := TCompressionStream.create(clDefault, ms, not tpTrue(PL.look[2]));
            for i := 0 to high(bv.fBytes) do cs.WriteByte(bv.fBytes[i]);
            cs.Free;
            ms.Position := 0;
            result := TVBytes.Create;
            bv := result as TVBytes;
            bv.SetCount(ms.Size);
            for i := 0 to high(bv.fBytes) do bv.fBytes[i] := ms.ReadByte;
            ms.Free;
        end;
        3: raise ELE.Create('inactive stream', 'invalid parameters');
    end;
end;

function if_inflate             (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; bv: TVBytes; ds: TDecompressionStream; ms: TMemoryStream;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpKeywordOrNIL, tpAny,
        tpBytes,               tpKeywordOrNil, tpAny,
        tpStreamPointer, tpKeywordOrNIL, tpAny]) of
        1:  begin
            result := TVStreamPointer.Create(
                    TLInflateStream.Create(
                        (PL.look[0] as TVStreamPointer).body.Ref as TLStream,
                        tpTrue(PL.look[2]),
                        ifh_keyword_to_encoding(PL.look[1])));
        end;
        2: begin
            bv := PL.look[0] as TVBytes;
            ms := TMemoryStream.Create;
            ms.SetSize(Length(bv.fBytes));
            ms.Position :=0 ;
            for i := 0 to high(bv.fBytes) do ms.WriteByte(bv.fBytes[i]);
            try
                ms.Position := 0;
                ds := TDecompressionStream.create(ms, not tpTrue(PL.look[2]));
                result := TVBytes.Create;
                bv := result as TVBytes;
                try
                    while true do bv.Add(ds.ReadByte);
                except
                    //on E:EDecompressionError do;
                end;
            finally
                ms.Free;
                ds.Free;
            end;
        end;
        3: raise ELE.Create('inactive stream', 'invalid parameters');
    end;
end;


function if_memory_stream       (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpBytes,
        tpString,
        tpNIL]) of
        1: result := TVStreamPointer.Create(
                    TLMemoryStream.Create(
                        (PL.look[0] as TVBytes).fBytes,
                        seUTF8));
        2: result := TVStreamPointer.Create(
                    TLMemoryStream.Create(
                        (PL.look[0] as TVString).S));
        3: result := TVStreamPointer.Create(TLMemoryStream.Create(seUTF8));
    end;
end;


function if_stream_position     (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, vpIntegerNotNegative,
        vpStreamPointerActive, tpNIL]) of
        1: begin
            (PL.look[0] as TVStreamPointer).body.Position := PL.I[1];
            result := TVT.Create;
        end;
        2: result := TVInteger.Create(
            (PL.look[0] as TVStreamPointer).body.Position);
    end;
end;


function if_stream_length       (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpNIL,
        vpStreamPointerActive, vpIntegerNotNegative]) of
        1: result := TVInteger.Create(
            (PL.look[0] as TVStreamPointer).body.Size);
        2: begin
            (PL.look[0] as TVStreamPointer).body.Size := PL.I[1];
            result := TVT.Create;
        end;
    end;
end;

function if_read_byte           (const PL: TVList; {%H-}call: TCallProc): TValue;
var b: byte;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpNIL,
        vpStreamPointerActive, vpIntegerNotNegative,
        vpStreamPointerActive, vpKeyword_ALL]) of
        1: if (PL.look[0] as TVStreamPointer).body.read_byte(b)
            then result := TVInteger.Create(b)
            else result := TVList.Create;
        2: begin
            result := TVBytes.Create;
            (PL.look[0] as TVStreamPointer).body.read_bytes(
                (result as TVBytes).fBytes, PL.I[1]);
        end;
        3: begin
            result := TVBytes.Create;
            (PL.look[0] as TVStreamPointer).body.read_bytes(
                (result as TVBytes).fBytes, -1);
        end;
    end;
end;


function if_write_byte          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, vpIntegerByte,
        vpStreamPointerActive, tpBytes]) of
        1: begin
            (PL.look[0] as TVStreamPointer).body.write_byte(PL.I[1]);
            result := TVT.Create
        end;
        2: begin
            (PL.look[0] as TVStreamPointer).body.write_bytes(
                (PL.look[1] as TVBytes).fBytes);
            result := TVT.Create;
        end;
    end;
end;

function if_read_character      (const PL: TVList; {%H-}call: TCallProc): TValue;
var ch: unicodechar; s: unicodestring; i: integer;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpNIL,
        vpStreamPointerActive, vpIntegerNotnegative,
        vpStreamPointerActive, vpKeyword_ALL,
        tpNIL, tpAny]) of
        1: if (PL.look[0] as TVStreamPointer).body.read_char(ch)
            then result := TVString.Create(ch)
            else result := TVList.Create;
        2: begin
            s := '';
            for i := 1 to PL.I[1] do begin
                if (PL.look[0] as TVStreamPointer).body.read_char(ch)
                then s := s + ch
                else break;
            end;
            result := TVString.Create(s);
        end;
        3: begin
            s := '';
            while (PL.look[0] as TVStreamPointer).body.read_char(ch) do
                s := s + ch;
            result := TVString.Create(s);
        end;
        4: begin
            System.Read(ch);
            result := TVString.Create(ch);
        end;
    end;
end;

function if_write_string        (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpString,
        tpNil,                 tpString]) of
        1: ifh_write_string(PL.look[0] as TVStreamPointer, PL.S[1]);
        2: System.Write(PL.S[1]);
    end;
    result := TVT.Create;
end;


function if_read_line           (const PL: TVList; {%H-}call: TCallProc): TValue;
var ch: unicodechar; s: unicodestring;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,
        tpNIL]) of
        1: begin
            s := '';
            try
                while (PL.look[0] as TVStreamPointer).body.read_char(ch) do
                    case ch of
                        #13,#10: if s<>'' then break;
                        else s := s + ch;
                    end;
            finally

            end;
            if s<>''
            then result := TVString.Create(s)
            else result := TVList.Create;
        end;
        2: begin
            System.ReadLn(s);
            result := TVString.Create(s);
        end;
    end;
end;

function if_write_line          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpString,
        tpNIL,                 tpString]) of
        1: ifh_write_string(PL.look[0] as TVStreamPointer, PL.S[1]+new_line);
        2: System.WriteLn(PL.S[1]);
    end;
    result := TVT.Create
end;

function if_read_bom            (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1:  (PL.look[0] as TVStreamPointer).body.encoding := seBOM;
    end;
    result := TVT.Create;
end;

function if_write_bom           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: (PL.look[0] as TVStreamPointer).body.write_BOM;
    end;
    result := TVT.Create;
end;


function if_read                (const PL: TVList; {%H-}call: TCallProc): TValue;
var s: unicodestring;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,
        tpNIL,
        tpString]) of
        1: begin
            result := dlisp_read.read(PL.look[0] as TVStreamPointer);

            if result is TVEndOfStream then begin
                FreeAndNil(result);
                result := TVList.Create;
            end;
        end;
        2: begin
            ReadLn(s);
            result := dlisp_read.read_from_string(s);
        end;
        3: result := dlisp_read.read_from_string(PL.S[0]);
    end;
end;

function if_write               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpAny,
        tpNIL,           tpAny]) of
        1: begin
            //WriteLn('==1',PL.AsString());
            dlisp_read.print(PL.look[1], PL.look[0] as TVStreamPointer);
            result := TVT.Create;
            //TODO: не возвращается ошибка при записи в файл
        end;
        2: begin
            result := TVT.Create;
            dlisp_read.print(PL.look[1], nil);
        end;
    end;
end;

function if_print               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
        case params_is(PL, result, [
            vpStreamPointerActive, tpAny,
            tpNIL,                 tpAny,
            vpKeyword_RESULT,      tpAny]) of
            1: begin
                print(PL.look[1], PL.look[0] as TVStreamPointer, true);
                result := TVT.Create;
            end;
            2: begin
                print(PL.look[1], nil, true);
                result := TVT.Create;
            end;
            3: result := TVString.Create(PL[1].AsString);
        end;
end;

function if_input               (const PL: TVList; {%H-}call: TCallProc): TValue;
var s: unicodestring;
begin
        case params_is(PL, result, [
            tpString, tpList]) of
            1: begin;
                if tpTrue(PL.look[1]) then WriteLn(ifh_format(PL.L[1]));
                System.Write(PL.S[0]);
                System.Read(S);
                result := TVList.Create([TVString.Create(S)]);
                (result as TVList).Append(read_from_string('('+S+')') as TVList);
            end;
        end;
end;

function if_fmt                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,   tpList,
        tpNIL,                   tpList,
        vpKeyword_RESULT,        tpList,
        tpT,                     tpList]) of
        1: begin
            ifh_write_string(PL.look[0] as TVStreamPointer, ifh_format(PL.L[1]));
            result := TVT.Create;
        end;
        2: begin
            System.Write(ifh_format(PL.L[1]));
            result := TVT.Create;
        end;
        3,4: result := TVString.Create(ifh_format(PL.L[1]));
    end;
end;

function if_log                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpList]) of
        1: begin
            System.WriteLn(ifh_format(PL.L[0]));
            result := TVT.Create;
        end;
    end;
end;

function if_hex                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative,  tpNIL,
        vpIntegerNotNegative,  vpIntegerNotNegative]) of
        1: result := TVString.Create(IntToHex(PL.I[0], 0));
        2: result := TVString.Create(IntToHex(PL.I[0], PL.I[1]));
    end;
end;

function if_fixed               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpReal,  vpIntegerNotNegative]) of
        1: result := TVString.Create(FloatToStrF(PL.F[0], ffFixed, 0, PL.I[1]));
    end;
end;

function if_col                 (const PL: TVList; {%H-}call: TCallProc): TValue;
var s: unicodestring; w,i: integer; lr: boolean;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative, tpAny, vpKeywordAlignOrNIL]) of
        1: begin
            if tpString(PL.look[1])
            then s := PL.S[1]
            else s := PL.look[1].AsString;
            w := PL.I[0];
            lr := false;
            if Length(s)<w
            then
                for i := 1 to W-Length(s) do
                    if tpNIL(PL.look[2]) or vpKeyword_LEFT(PL.look[2])
                    then s := s + ' '
                    else
                        if vpKeyword_RIGHT(PL.look[2])
                        then s := ' ' + s
                        else
                            if vpKeyword_CENTER(PL.look[2])
                            then begin
                                if lr then s := s + ' ' else s := ' ' + s;
                                lr := not lr;
                            end;
            result := TVString.Create(s);
        end;
    end;
end;

function if_fmt_list            (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; res, s, b, e: unicodestring;
begin
    case params_is(PL, result, [
        tpList, tpString, tpString, tpString,
        tpList, tpString, tpString, tpNIL,
        tpList, tpString, tpNIL,    tpNIL,
        tpList, tpNIL,    tpNIL,    tpNIL]) of
        1,2,3,4: begin
            if tpString(PL.look[3]) then e := PL.S[3] else e := '';
            if tpString(PL.look[2]) then b := PL.S[2] else b := '';
            if tpString(PL.look[1]) then s := PL.S[1] else s := ' ';
            res := b;
            if PL.L[0].Count>=1 then
                if tpString(PL.L[0].look[0])
                then res := res + PL.L[0].S[0]
                else res := res + PL.L[0].look[0].AsString;
            for i := 1 to PL.L[0].High do
                if tpString(PL.L[0].look[i])
                then res := res + s + PL.L[0].S[i]
                else res := res + s + PL.L[0].look[i].AsString;
            res := res + e;
            Result := TVString.Create(res);
        end;
    end;
end;

function if_upper_case          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(UnicodeUpperCase(PL.S[0]));
    end;
end;

function if_lower_case          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(UnicodeLowerCase(PL.S[0]));
    end;
end;

function if_fmt_table           (const PL: TVList; {%H-}call: TCallProc): TValue;
var data: array of array of unicodestring; cols: array of integer; i,j,k,w,h: integer;
    table: TVList; stdout: boolean;
    hs: unicodestring;
    mode: (pseudo, csv, html);
    procedure put(s: unicodestring);
    begin
        if stdout then Write(s)
        else (PL.look[0] as TVStreamPointer).body.write_string(s);
    end;

begin
    case params_is(PL, result, [
        tpNIL,                 vpListOfListsEqualLength, tpStringOrNIL, vpKeywordTableModeOrNIL,
        vpStreamPointerActive, vpListOfListsEqualLength, tpStringOrNIL, vpKeywordTableModeOrNIL]) of
        1,2: begin
            result := TVT.Create;
            stdout := tpNIL(PL.look[0]);
            table := PL.L[1];
            if tpNIL(PL.look[2]) then hs:='' else hs:=PL.S[2];
            if tpNIL(PL.look[3]) then mode:=pseudo else
            if vpKeyword_HTML(PL.look[3]) then mode := html else mode:=csv;
            if (mode=csv) and (hs='') then hs:=';';
            h := table.Count;
            if h=0 then Exit;
            w := table.L[0].Count;
            SetLength(data, h);
            for i := 0 to h-1 do SetLength(data[i], w);
            SetLength(cols, w);
            for j := 0 to w-1 do cols[j]:=0;
            for i := 0 to h-1 do
                for j := 0 to w-1 do begin
                    if tpList(table.L[i].look[j])
                    then data[i,j] := ifh_format(table.L[i].L[j])
                    else
                        if tpString(table.L[i].look[j])
                        then data[i,j] := table.L[i].S[j]
                        else data[i,j] := table.L[i].look[j].AsString;
                    if Length(data[i,j])>cols[j] then cols[j]:=Length(data[i,j]);
                end;

            case mode of
                pseudo: for i := 0 to h-1 do begin
                    put(hs);
                    for j := 0 to w-1 do begin
                        put(data[i,j]);
                        for k := Length(data[i,j]) to cols[j] do Put(' ');
                        put(hs);
                    end;
                    put(new_line);
                end;
                csv: for i := 0 to h-1 do begin
                    for j := 0 to w-1 do begin
                        put(data[i,j]);
                        put(hs);
                    end;
                    put(#13#10);
                end;
                html: begin
                    put('<table>'); put(#13#10);
                    for i := 0 to h-1 do begin
                        put('<tr>');
                        for j := 0 to w-1 do begin
                            put('<td>');
                            put(data[i,j]);
                            put('</td>');
                        end;
                        put('</tr>'); put(#13#10);
                    end;
                    put('</table>'); put(#13#10);
                end;
            end;
        end;
    end;
end;


function if_xml_read            (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,
        tpString]) of
        1: result := xml_read((PL.look[0] as TVStreamPointer).body);
        2: result := xml_from_string(PL.S[0]);
    end;
end;


function if_xml_write           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,  tpList,
        tpT,                    tpList]) of
        1: begin
            xml_write((PL.look[0] as TVStreamPointer).body, PL.L[1]);
            result := TVT.Create;
        end;
        2: result := TVString.Create(xml_to_string(PL.L[1]));
    end;
end;

function if_sql_mysql_connection(const PL: TVList; {%H-}call: TCallProc): TValue;
var database, username, host, password: unicodestring; port: integer;
const
    P_database = 0;
    P_host = 1;
    P_port = 2;
    P_username = 3;
    P_password = 4;
begin
    case params_is(PL, result, [
        tpString, tpStringOrNIL,vpIntegerNotNegativeOrNIL, tpStringOrNIL, tpStringOrNIL]) of
        1: begin
            database := PL.S[P_database];
            if tpNIL(PL.look[P_host])
            then host := 'localhost' else host := PL.S[P_host];
            if tpNIL(PL.look[P_port])
            then port := 3306 else port := PL.I[P_port];
            if tpNIL(PL.look[P_username])
            then username := '' else username := PL.S[P_username];
            if tpNIL(PL.look[P_password])
            then password := '' else password := PL.S[P_password];
            result := TVSQLPointer.CreateMySQL(database, host, port, username, password);
        end;
    end;
end;


function ifh_SQL_datatype(F: TField): TValue;
begin
    if VarIsNull(F.Value)
    then result := TVList.Create
    else case F.DataType of
        ftUnknown, ftString, ftWideString,ftFmtMemo, ftMemo, ftFixedWideChar,
        ftWideMemo,ftFixedChar:
            result := TVString.Create(F.AsString);

        ftSmallint, ftInteger, ftWord, ftLargeInt:
            result := TVInteger.Create(F.AsInteger);

        ftBoolean:
            bool_to_TV(F.AsBoolean, result);

        ftFloat:
            result := TVFloat.Create(F.AsFloat);

        ftDateTime, ftDate, ftTimeStamp:
            result := TVDateTime.Create(F.AsDateTime);

        ftTime: result := TVTimeInterval.Create(F.AsDateTime);

        else result := TVString.Create(F.AsString);

        //ftCurrency, ftBCD,
        //ftBytes, ftVarBytes, ftAutoInc, ftBlob, , ftGraphic, ,
        //ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
        //, ftADT, ftArray, ftReference,
        //ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
        //ftIDispatch, ftGuid, ftFMTBcd, );
    end;
end;

function if_sql_query           (const PL: TVList; {%H-}call: TCallProc): TValue;
var rec: TVRecord; i,j: integer;
    ucommand: unicodestring;
begin
    case params_is(PL, result, [
        vpSQLPointerActive, tpList]) of
        1: with (PL.look[0] as TVSQLPointer).query do try
            rec := nil;

            Active := false;
            SQL.Text := ifh_format(PL.L[1]);
            ucommand := UnicodeUpperCase(SQL.Text);
            if (Pos('SELECT', ucommand)=1)
                or (Pos('SHOW', ucommand)=1)
                or (Pos('DESCRIBE', ucommand)=1)
            then Active := true
            else begin
                result := TVT.Create;
                ExecSQL;
                Exit;
            end;
            Last;

            rec := TVRecord.Create;
            for j := 0 to FieldCount-1 do
                rec.AddSlot(Fields[j].DisplayName, TVT.Create);
            result := TVList.Create;

            First;
            for i := 0 to RecordCount-1 do begin
                for j := 0 to FieldCount-1 do
                    rec[j] := ifh_SQL_datatype(Fields[j]);
                (result as TVList).Add(rec.Copy);
                Next;
            end;
        finally
            rec.Free;
        end;
    end;
end;

function if_sql_query_list      (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
    ucommand: unicodestring;
begin
    case params_is(PL, result, [
        vpSQLPointerActive, tpList]) of
        1: with (PL.look[0] as TVSQLPointer).query do begin

            Active := false;
            SQL.Text := ifh_format(PL.L[1]);
            ucommand := UnicodeUpperCase(SQL.Text);
            if (Pos('SELECT', ucommand)=1)
                or (Pos('SHOW', ucommand)=1)
            then Active := true
            else raise ELE.Create('invalid query for list', 'sql');

            Last;

            if FieldCount<>1 then raise ELE.Create('запрос вернул не одну колонку', 'sql');

            result := TVList.Create;
            First;
            for i := 0 to RecordCount-1 do begin
                (result as TVList).Add(ifh_SQL_datatype(Fields[0]));
                Next;
            end;
        end;
    end;
end;


function if_http_get            (const PL: TVList; {%H-}call: TCallProc): TValue;
var http: TFPHTTPClient; scp: integer;
begin
    case params_is(PL, result, [
        tpString, tpNIL,
        tpString, tpString]) of
        1,2: try
            http := TFPHTTPClient.Create(nil);//TFPHTTPClient.Create(nil);
            http.AllowRedirect:=true;
            if tpString(PL.look[1]) then begin
                scp:= PosU(':',PL.S[1]);
                if scp>0
                then begin
                    http.Proxy.host := PL.S[1][1..scp-1];
                    http.proxy.port := StrToIntDef(PL.S[1][scp+1..Length(PL.S[0])],0);
                end
                else http.Proxy.host := PL.S[1];
            end;
            http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
            result := TVString.Create(http.Get(PL.S[0]));
        finally
            http.Free;
        end;
    end;
end;

function if_regexp_match        (const PL: TVList; {%H-}call: TCallProc): TValue;
var re: TRegExpr; start: integer; s, e: unicodestring;
begin
    case params_is(PL, result, [
        tpString, tpString, vpIntegerNotNegativeOrNIL]) of
        1: try
            re := TRegExpr.Create(PL.S[1]);
            e:=pl.s[1];
            //re.InputString:=PL.S[0];
            result := TVList.Create;
            if tpInteger(PL.look[2]) then start := PL.I[2]+1 else start := 1;
            s := PL.S[0][start..Length(PL.S[0])];
            if re.Exec(s) then begin
                (result as TVList).Add(TVString.Create(re.Match[0]));
                while re.ExecNext do
                begin
                    (result as TVList).Add(TVString.Create(re.Match[0]));
                end;
            end;
        finally
            re.Free;
        end;
    end;
end;


function if_if                  (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpAny, tpAny, tpAny]) of
        1: if tpTrue(PL.look[0])
            then result := PL[1]
            else result := PL[2];
    end;
end;

function if_if_nil              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpAny, tpAny]) of
        1: if tpTrue(PL.look[0])
            then result := PL[0]
            else result := PL[1];
    end;
end;


const int_fun_count = 140;
var int_fun_sign: array[1..int_fun_count] of TVList;
const int_fun: array[1..int_fun_count] of TInternalFunctionRec = (
(n:'RECORD?';                   f:if_structure_p;           s:'(s :optional type)'),

(n:'+';                         f:if_add;                   s:'(:rest n)'),
(n:'-';                         f:if_sub;                   s:'(a :optional b)'),
(n:'*';                         f:if_mul;                   s:'(:rest n)'),
(n:'/';                         f:if_div;                   s:'(a :optional b)'),
(n:'DIV';                       f:if_div_int;               s:'(a b)'),
(n:'MOD';                       f:if_mod;                   s:'(a b)'),
(n:'ABS';                       f:if_abs;                   s:'(a)'),
(n:'**';                        f:if_power;                 s:'(a b)'),
(n:'MAX';                       f:if_max;                   s:'(a :rest b)'),
(n:'MIN';                       f:if_min;                   s:'(a :rest b)'),
(n:'SQRT КОРЕНЬ';               f:if_sqrt;                  s:'(a)'),
(n:'ROUND';                     f:if_round;                 s:'(a)'),
(n:'RANGE';                     f:if_range;                 s:'(l :optional h)'),
(n:'SYMBOL';                    f:if_symbol;                s:'(:optional n)'),
(n:'RANDOM';                    f:if_random;                s:'(:optional r)'),
(n:'RE';                        f:if_re;                    s:'(a)'),
(n:'IM';                        f:if_im;                    s:'(a)'),
(n:'HASH ОКРОШКА ХЭШ';          f:if_hash;                  s:'(a)'),
(n:'SPLIT-STRING';              f:if_split_string;          s:'(s :optional separator)'),
(n:'TRIM';                      f:if_trim;                  s:'(s)'),

(n:'=';                         f:if_equal;                 s:'(a b)'),
(n:'>';                         f:if_more;                  s:'(a b)'),
(n:'<';                         f:if_less;                  s:'(a b)'),
(n:'>=';                        f:if_more_or_equal;         s:'(a b)'),
(n:'<=';                        f:if_less_or_equal;         s:'(a b)'),
(n:'<>';                        f:if_not_equal;             s:'(a b)'),
(n:'AND И';                     f:if_and;                   s:'(:rest a)'),
(n:'OR ИЛИ';                    f:if_or;                    s:'(:rest a)'),
(n:'XOR';                       f:if_xor;                   s:'(:rest a)'),
(n:'NOT НЕ';                    f:if_not;                   s:'(a)'),
(n:'EQUAL-CASE-INSENSITIVE';    f:if_equal_case_insensitive;s:'(s s)'),
(n:'EQUAL-SETS';                f:if_equal_sets;            s:'(a b)'),
(n:'LENGTH-MORE';               f:if_length_more;           s:'(a b)'),
(n:'LENGTH-LESS';               f:if_length_less;           s:'(a b)'),
(n:'IDENTITY';                  f:if_identity;              s:'(v)'),

(n:'TEST-DYN';                  f:if_test_dyn;              s:'(:rest msgs)'),
(n:'LIKENESS СХОДСТВО';         f:if_likeness;              s:'(s1 s2)'),


(n:'EXTRACT-FILE-EXT';          f:if_extract_file_ext;      s:'(s)'),
(n:'EXTRACT-FILE-NAME';         f:if_extract_file_name;     s:'(s)'),
(n:'EXTRACT-FILE-PATH';         f:if_extract_file_path;     s:'(s)'),
(n:'FILE-EXISTS';               f:if_file_exists;           s:'(n)'),
(n:'DIRECTORY-EXISTS';          f:if_directory_exists;      s:'(d)'),
(n:'COMMAND-LINE';              f:if_command_line;          s:'()'),
(n:'ENVIRONMENT-VARIABLE';      f:if_environment_variable;  s:'(name)'),
(n:'CHANGE-DIRECTORY';          f:if_change_directory;      s:'(d)'),
(n:'DELETE-FILE УДАЛИТЬ-ФАЙЛ';  f:if_delete_file;           s:'(f)'),
(n:'RENAME-FILE ПЕРЕИМЕНОВАТЬ-ФАЙЛ';f:if_rename_file;           s:'(o n)'),
(n:'CREATE-DIRECTORY СОЗДАТЬ-ПАПКУ';f:if_create_directory;      s:'(d)'),
(n:'REMOVE-DIRECTORY УДАЛИТЬ-ПАПКУ';f:if_remove_directory;      s:'(d)'),
(n:'GUID';                      f:if_guid;                  s:'()'),

(n:'EVERY КАЖДЫЙ';              f:if_every;                 s:'(p :rest l)'),
(n:'SOME ЛЮБОЙ';                f:if_some;                  s:'(p :rest l)'),
(n:'CAR HEAD ГОЛОВА';           f:if_car;                   s:'(l)'),
(n:'LAST';                      f:if_last;                  s:'(l)'),
(n:'SUBSEQ';                    f:if_subseq;                s:'(s b :optional e)'),
(n:'SORT';                      f:if_sort;                  s:'(s :optional p)'),
(n:'SLOTS';                     f:if_slots;                 s:'(r)'),
(n:'CURRY ШФ';                  f:if_curry;                 s:'(f :rest p)'),
(n:'FILTER';                    f:if_filter;                s:'(p l)'),
(n:'REJECT';                    f:if_reject;                s:'(p l)'),
(n:'FOLD';                      f:if_fold;                  s:'(p l)'),
(n:'MAP ОТОБРАЖЕНИЕ';           f:if_map;                   s:'(p :rest l)'),
(n:'MAP-CONCATENATE';           f:if_map_concatenate;       s:'(p :rest l)'),
(n:'APPLY ПРИМЕНИТЬ';           f:if_apply;                 s:'(p :rest params)'),

(n:'UNION';                     f:if_union;                 s:'(:rest a)'),
(n:'INTERSECTION ПЕРЕСЕЧЕНИЕ';  f:if_intersection;          s:'(:rest a)'),
(n:'DIFFERENCE';                f:if_difference;            s:'(a b)'),
(n:'REVERSE';                   f:if_reverse;               s:'(a)'),
(n:'MEMBER';                    f:if_member;                s:'(l e)'),
(n:'POSITION';                  f:if_position;              s:'(l e)'),
(n:'LENGTH ДЛИНА';              f:if_length;                s:'(l)'),
(n:'LIST СПИСОК';               f:if_list;                  s:'(:rest e)'),
(n:'EMPTY-LIST ПУСТОЙ-СПИСОК';  f:if_empty_list;            s:'(l :optional e)'),
(n:'RECORD ЗАПИСЬ';             f:if_record;                s:'(:rest slots)'),
(n:'RECORD-AS ЗАПИСЬ-КАК';      f:if_record_as;             s:'(template :rest slots)'),
(n:'HASH-TABLE';                f:if_hash_table;            s:'()'),
(n:'CONCATENATE';               f:if_concatenate;           s:'(:rest a)'),
(n:'GROUP ГРУППИРОВКА';         f:if_group;                 s:'(s :rest p)'),
(n:'STRINGS-MISMATCH';          f:if_strings_mismatch;      s:'(a b)'),
(n:'ASSOCIATIONS';              f:if_associations;          s:'(al k :flag lazy by-head)'),
(n:'SUBRANGE';                  f:if_subrange;              s:'(r v)'),
(n:'ELEMENT ЭЛЕМЕНТ';           f:if_element;               s:'(c :rest k)'),

(n:'BYTES';                     f:if_bytes;                 s:'(:rest b)'),
(n:'BITWISE-AND';               f:if_bitwise_and;           s:'(a b)'),
(n:'BITWISE-NOT';               f:if_bitwise_not;           s:'(a)'),
(n:'BITWISE-OR';                f:if_bitwise_or;            s:'(a b)'),
(n:'BITWISE-XOR';               f:if_bitwise_xor;           s:'(a b)'),
(n:'CRC32';                     f:if_crc32;                 s:'(b)'),
(n:'CHARACTER';                 f:if_character;             s:'(n)'),
(n:'BYTES-TO-STRING';           f:if_byte_vector_to_string; s:'(bv :optional encoding)'),

(n:'ASSERTION';                 f:if_assertion;             s:'(c :rest m)'),
(n:'DOCUMENTATION';             f:if_documentation;         s:'(a)'),
(n:'DIRECTORY';                 f:if_directory;             s:'(d)'),
(n:'SLEEP';                     f:if_sleep;                 s:'(m)'),

(n:'RUN-COMMAND';               f:if_run_command;           s:'(c :optional d)'),
(n:'NOW';                       f:if_now;                   s:'()'),


(n:'OPEN-FILE';                 f:if_open_file;             s:'(n :key mode encoding)'),
(n:'FILE ФАЙЛ';                 f:if_open_file;             s:'(n :optional mode encoding)'),
(n:'CLOSE-FILE';                f:if_close_stream;          s:'(s)'),
(n:'DEFLATE';                   f:if_deflate;               s:'(s :key encoding header)'),
(n:'INFLATE';                   f:if_inflate;               s:'(s :key encoding header)'),
(n:'MEMORY-STREAM';             f:if_memory_stream;         s:'(:optional bv)'),
(n:'SET-ENCODING';              f:if_set_encoding;          s:'(s e)'),

(n:'ZIP:OPEN';                  f:if_zip_open;              s:'(n :key mode encoding)'),
(n:'ZIP:FILELIST';              f:if_zip_filelist;          s:'(z)'),
(n:'ZIP:FILE';                  f:if_zip_file;              s:'(z fn :optional mode encoding)'),
(n:'ZIP:DELETE';                f:if_zip_delete;            s:'(z fn)'),

(n:'STREAM-POSITION';           f:if_stream_position;       s:'(s :optional p)'),
(n:'STREAM-LENGTH';             f:if_stream_length;         s:'(s :optional l)'),
(n:'READ-BYTE';                 f:if_read_byte;             s:'(s :optional count)'),
(n:'WRITE-BYTE';                f:if_write_byte;            s:'(s i)'),
(n:'READ-CHARACTER';            f:if_read_character;        s:'(:optional s count)'),
(n:'WRITE-STRING';              f:if_write_string;          s:'(s s)'),
(n:'READ-LINE';                 f:if_read_line;             s:'(:optional s)'),
(n:'WRITE-LINE';                f:if_write_line;            s:'(s l)'),
(n:'READ-BOM';                  f:if_read_bom;              s:'(s)'),
(n:'WRITE-BOM';                 f:if_write_bom;             s:'(s)'),

(n:'READ';                      f:if_read;                  s:'(:optional s)'),
(n:'WRITE';                     f:if_write;                 s:'(s a)'),
(n:'PRINT';                     f:if_print;                 s:'(s a)'),
(n:'INPUT ВВОД';                f:if_input;                 s:'(prompt :rest query)'),

(n:'FMT';                       f:if_fmt;                   s:'(stream :rest s)'),
(n:'LOG';                       f:if_log;                   s:'(:rest s)'),
(n:'HEX';                       f:if_hex;                   s:'(i :optional d)'),
(n:'FIXED';                     f:if_fixed;                 s:'(f :optional d)'),
(n:'COL';                       f:if_col;                   s:'(w v :optional a)'),
(n:'LST';                       f:if_fmt_list;              s:'(l :optional s b e)'),
(n:'UPPER-CASE';                f:if_upper_case;            s:'(s)'),
(n:'LOWER-CASE';                f:if_lower_case;            s:'(s)'),
(n:'FMT-TABLE';                 f:if_fmt_table;             s:'(stream data :key hs mode)'),


(n:'XML:READ';                  f:if_xml_read;              s:'(stream)'),
(n:'XML:WRITE';                 f:if_xml_write;             s:'(stream xml)'),

(n:'SQL:MYSQL-CONNECTION';      f:if_sql_mysql_connection;  s:'(database :key host port username password)'),
(n:'SQL:QUERY';                 f:if_sql_query;             s:'(db :rest q)'),
(n:'SQL:QUERY-LIST';            f:if_sql_query_list;        s:'(db :rest q)'),

(n:'HTTP:GET';                  f:if_http_get;              s:'(url :key proxy)'),

(n:'REGEXP:MATCH';              f:if_regexp_match;          s:'(s expr :key from)'),

(n:'?';                         f:if_if;                    s:'(cnd a :optional b)'),
(n:'F-IF-NIL';                  f:if_if_nil;                s:'(cnd a)')

);


const predicates: array[1..18] of record n: unicodestring; f: TTypePredicate; end = (
(n:'T';                    f:tpT),
(n:'NIL';                  f:tpNIL),
(n:'TRUE';                 f:tpTRUE),
//(n:'NUMBER?';               f:tpNumber),
(n:'REAL';                 f:tpReal),
(n:'INTEGER';              f:tpInteger),
(n:'FLOAT';                f:tpFloat),
(n:'ATOM';                 f:tpAtom),
(n:'SUBPROGRAM';           f:tpSubprogram),
(n:'LIST';                 f:tpList),
(n:'EMPTY';                f:vpEmpty),
(n:'SYMBOL';               f:tpSymbol),
(n:'KEYWORD';              f:tpKeyword),
(n:'STRING';               f:tpString),
(n:'POSITIVE';             f:vpRealPositive),
(n:'NEGATIVE';             f:vpRealNegative),
(n:'BYTES';                f:tpBytes),
(n:'END-OF-STREAM';        f:vpStreamEnd),
(n:'END-OF-FILE';          f:vpStreamEnd)

);


var ops: array[TOperatorEnum] of record n: unicodestring; nN: integer; end;

procedure fill_ops_array;
var o: TOperatorEnum;
    procedure op(name: unicodestring);
    begin
        ops[o].n := name;
        ops[o].nN := TVSymbol.symbol_n(name);
    end;
begin
    for o := low(ops) to high(ops) do
        case o of
            oeAND_THEN  : op('AND-THEN');
            oeAPPEND    : op('APPEND');
            oeASSEMBLE  : op('ASSEMBLE');
            oeBLOCK     : op('BLOCK');
            oeBREAK     : op('BREAK');
            oeCASE      : op('CASE');
            oeCOND      : op('COND');
            oeCONST     : op('CONST');
            oeCONTINUE  : op('CONTINUE');
            oeDEBUG     : op('DEBUG');
            oeDEFAULT   : op('DEFAULT');
            oeDELETE    : op('DELETE');
            oeELT       : op('ELT');
            oeERROR     : op('ERROR');
            oeEXECUTE_FILE: op('EXECUTE-FILE');
            oeFOR       : op('FOR');
            oeGOTO      : op('GOTO');
            oeIF        : op('IF');
            oeIF_NIL    : op('IF-NIL');
            oeINSERT    : op('INSERT');
            oeKEY       : op('KEY');
            oeLET       : op('LET');
            oeMACRO     : op('MACRO');
            oeMACRO_SYMBOL: op('MACRO-SYMBOL');
            oeOR_THEN   : op('OR-THEN');
            oePACKAGE   : op('PACKAGE');
            oePOP       : op('POP');
            oePROCEDURE : op('PROCEDURE');
            oePUSH      : op('PUSH');
            oeQUOTE     : op('QUOTE');
            oeRETURN    : op('RETURN');
            oeSET       : op('SET');
            oeUSE       : op('USE');
            oeVAR       : op('VAR');
            oeWHEN      : op('WHEN');
            oeWHILE     : op('WHILE');
            oeWITH      : op('WITH');
            else raise Exception.Create('неизвестный оператор');
        end
end;

procedure fill_int_fun_signs;
var i: integer;
begin
    for i := low(int_fun) to high(int_fun) do
        int_fun_sign[i] := read_from_string(int_fun[i].s) as TVList;
end;

procedure free_int_fun_signs;
var i: integer;
begin
    for i := low(int_fun) to high(int_fun) do int_fun_sign[i].Free;
end;

procedure fill_base_stack;
var i, j: integer; fun_names: TStringArray;
begin
    base_stack := TVSymbolStack.Create(nil);

    //загрузка внутренних функций
    for i := low(int_fun) to high(int_fun) do begin
        fun_names := SplitString(int_fun[i].n);
        for j := 0 to high(fun_names) do
            base_stack.new_var(
                fun_names[j],
                TVInternalFunction.Create(
                        int_fun_sign[i],
                        int_fun[i].f,
                        fun_names[j]),
                true);
    end;

    //загрузка предикатов
    for i := low(predicates) to high(predicates) do begin
        base_stack.new_var(
            predicates[i].n+'?',
            TVPredicate.Create(predicates[i].n, predicates[i].f, false),
            true);
        base_stack.new_var(
            predicates[i].n+'!',
            TVPredicate.Create(predicates[i].n, predicates[i].f, true),
            true);
    end;

    //загрузка констант
    base_stack.new_var('NL', TVString.Create(new_line), true);
    base_stack.new_var('CR', TVString.Create(#10), true);
    base_stack.new_var('LF', TVString.Create(#13), true);
    base_stack.new_var('TAB', TVString.Create(#09), true);
    base_stack.new_var('_', TVSymbol.Create('_'));
end;


{ TEvaluationFlow }

constructor TEvaluationFlow.Create(parent_stack: TVSymbolStack);
begin
    if parent_stack<>nil
    then main_stack := parent_stack
    else main_stack := base_stack.Copy as TVSymbolStack;
    stack := main_stack;
end;

destructor TEvaluationFlow.Destroy;
begin
    main_stack.Free;
    inherited Destroy;
end;


function TEvaluationFlow.oph_block(PL: TVList; start: integer; with_frame: boolean): TValue;
var frame_start, exception_frame_start: integer; pc, i: integer; V: TValue;
    exception_message, exception_class, exception_stack: unicodestring;
label return;
    function val(): boolean;
    begin try
        FreeAndNil(V);
        V := eval(PL[pc]);
        result := true;
    except
        on E:ELE do begin
            result := false;
            exception_message := E.Message;
            exception_class := E.EClass;
            exception_stack := E.EStack;

        end;
        on E:Exception do begin
            result := false;
            exception_message := E.Message;
            exception_class := E.ClassName;
        end;
    end; end;

    function matched_exception: boolean;
    var ec, eh: unicodestring;
    begin
        result := false;
        ec := UnicodeUpperCase(exception_class);
        if vpListHeaded_EXCEPTION(PL.look[pc]) then
             if (PL.L[pc].Count>=2) and tpString(PL.L[pc].look[1])
             then begin
                eh := UnicodeUpperCase(PL.L[pc].S[1]);
                if (Length(eh)<=Length(ec)) and (eh=ec[1..length(eh)])
                then result := true;
             end
             else result := true;
    end;

begin

    frame_start := stack.Count;
    if with_frame then stack.new_var( ' <block>', TVT.Create);

    pc := start;
    V := TVList.Create;

    while (pc<PL.Count) and not vpListHeaded_EXCEPTION(PL.look[pc]) do begin
        if val
        then begin //выполнение блока
            Inc(pc);
            if tpGoto(V)
            then begin
                pc := -1;
                for i := 1 to PL.Count-1 do
                    if tpKeyword(PL.look[i]) and (PL.SYM[i].N=(V as TVGoto).N)
                    then pc := i;
                if pc<0 then break;
            end
            else
                if tpBreak(V) or tpContinue(V) or tpReturn(V) then break;

        end
        else begin //поиск обработчика исключений
            inc(pc);
            while pc<PL.Count do begin
                if matched_exception
                then begin
                    exception_frame_start := stack.Count;

                    stack.new_var('EXCEPTION-MESSAGE',
                        TVString.Create(exception_message));
                    stack.new_var('EXCEPTION-CLASS',
                        TVString.Create(exception_class));
                    stack.new_var('EXCEPTION-STACK',
                        TVString.Create(exception_stack));
                    result := oph_block(PL.L[pc], 1, false);
                    stack.clear_frame(exception_frame_start);
                    goto return;
                end;
                inc(pc);
            end;
            //если обработчик не найден
            if with_frame then stack.clear_frame(frame_start);
            raise ELE.Create(exception_message, exception_class, exception_stack);
        end;
    end;
    result := V;

return:
    if with_frame then stack.clear_frame(frame_start);
end;


procedure TEvaluationFlow.oph_bind(s, P: TValue; constant: boolean;
    st: TVSymbolStack; rests: TVRecord);
var bind: TBindings; i: integer; restV: TVList;
begin
    if st=nil then st := stack;
    bind := ifh_bind(s, p);
    for i := 0 to high(bind) do begin
        if not (_.N = bind[i].nN)
        then begin
            if (rests<>nil) and bind[i].rest then begin
                restV := rests.GetSlot(bind[i].nN) as TVList;
                restV.Append(bind[i].V as TVList);
                st.new_var(bind[i].nN, restV, true);
            end
            else st.new_var(bind[i].nN, bind[i].V, constant)
        end
        else bind[i].V.Free;
    end;
end;


function TEvaluationFlow.oph_bind_to_list(s, P: TVList): TVList;
var bind: TBindings; i: integer;
    params: TVList;
begin
    params := P.CDR;
    result := TVList.Create;
    bind := ifh_bind(s, params);
    for i := 0 to high(bind) do result.Add(bind[i].V);
    params.Free;
end;

function TEvaluationFlow.oph_execute_file(fn: unicodestring): boolean;
var prog_file: TVStreamPointer; expr, res: TValue;
begin
    result := true;
    try
        prog_file := nil;
        res := nil;
        //prog_file := TVStreamPointer.Create(
        //        NewVariable(
        //            TVFileStream.Create(DirSep(fn), fmOpenRead, seBOM)));
        prog_file := TVStreamPointer.Create(
                    TLFileStream.Create(DirSep(fn), fmOpenRead, seBOM));
        while true do begin
            expr := nil;
            expr := dlisp_read.read(prog_file);

            if ((expr IS TVSymbol) and ((expr as TVSymbol).uname='EXIT'))
                or (expr is TVEndOfStream)
            then begin expr.Free; break; end;

            if ((expr is TVSymbol) and ((expr as TVSymbol).uname='REPL'))
            then begin expr.Free; result := false; break; end;

            res := eval(expr);
            if res is TVReturn then break;
            FreeAndNil(res);
        end;
    finally
        FreeAndNil(prog_file);
        FreeAndNil(res);
    end;
end;


procedure TEvaluationFlow.oph_bind_package(name: unicodestring; import: boolean);
var pack: TPackage;
    package_file: unicodestring;
    built_in_stream: TLStream;
    procedure bind_pack;
    var i: integer; prefix: unicodestring;
    begin
        pack := FindPackage(name);
        if pack=nil then raise ELE.Create('package '+name+' not found');
        //WriteLn(pack.export_list.AsString());
        //pack.stack.Print();
        prefix := UnicodeUpperCase(name)+':';
        for i := 0 to pack.export_list.high do
            stack.new_ref(
                prefix+pack.export_list.uname[i],
                pack.stack.find_ref(pack.export_list.SYM[i]));
        if import then for i := 0 to pack.export_list.high do
                stack.new_ref(
                    pack.export_list.uname[i],
                    pack.stack.find_ref(pack.export_list.SYM[i]));
    end;

    procedure load_pack(fn: unicodestring);
    var expr: TValue; f: TLFileStream;
    begin try
        expr := nil;
        f := nil;
        f := TLFileStream.Create(fn, fmOpenRead, seBOM);
        expr := read(f);
        if (expr is TVList)
            and ((expr as TVList).Count>=3)
            and ((expr as TVList).look[0] is TVSymbol)
            and (((expr as TVList).uname[0]='PACKAGE') or ((expr as TVList).uname[0]='ПАКЕТ'))
            and ((expr as TVList).look[1] is TVSymbol)
            and ((expr as TVList).uname[1]=UnicodeUpperCase(name))
        then eval(expr).Free
        else raise ELE.Create(fn+' is not package '+name,'package');
    finally
        f.Free;
    end end;

begin
    pack := FindPackage(name);
    if pack<>nil
    then begin
        bind_pack;
        Exit;
    end;

    package_file := FindLisyaFile(name);
    if package_file<>'' then begin load_pack(package_file); bind_pack; Exit; end;

try
    built_in_stream := TLStream.FindBuiltIn(UnicodeLowerCase(name));
    if built_in_stream.active then begin
        eval(read(built_in_stream)).Free;
        bind_pack;
        Exit;
    end;
finally
    built_in_stream.Free;
end;

    raise ELE.Create('package '+name+' not found');
end;

function TEvaluationFlow.oph_eval_indices(V, target: TValue): TValue;
var expr: TVList;
begin
    //эта функция предназначена для случая когда вместо индексов передаётся
    //функция, возвращающая индексы. Использование этой функции блокирует
    //использование подпрограмм в качестве индексов для списков свойств и
    //хэш-таблиц
    if tpSubprogram(V)
    then try
        result := nil;
        expr := TVList.Create([V, target], false);
        result := call(expr);
        V.Free;
    finally
        expr.Free;
    end
    else result := V;
end;

function TEvaluationFlow.oph_index(target: TVSequence; i: TValue): integer;
begin
    if (vpKeyword_LAST(i) or vpSymbol_LAST(i)) and (target.Count>0)
    then result := -1
    else
        if tpInteger(i) then result := (i as TVInteger).fI
        else
            raise ELE.Create(i.AsString+' invalid index','invalid parameters');

    if result in [0..target.high] then result := result
    else
        if (result<0) and (result>=-target.Count) then result := target.Count+result
        else
            raise ELE.Create(i.AsString+' out of range 0..'+IntToStr(target.high));
end;

procedure TEvaluationFlow.oph_eval_link_for_modification(
    out CP: TVChainPointer; P: TValue);
begin
    CP := nil;
    CP := eval_link(P);
    if CP.constant then begin
        FreeAndNil(CP);
        raise ELE.Create('target is not variable');
    end;
    CP.CopyOnWrite;
end;


function TEvaluationFlow.opl_elt(PL: TVList): TVChainPointer;
var i: integer; ind: TVList; C: TVCompound;
begin
    //  Эта процедура должна вернуть указатель на компонент составного типа
    //для дальнейшего извлечения или перезаписи значения.
    result := nil;
    ind := nil;
try
    if PL.Count<2 then raise ELE.Malformed('ELT');

    result := eval_link(PL.look[1]);
    //TODO: result не освобождается при возникновении исключения

    C := result.look as TVCompound;
    ind := PL.subseq(2) as TVList;
    for i := 0 to ind.high do ind[i]:=eval(ind[i]);

    ifh_elt(C, ind, call, result.index);
finally
    ind.Free;
end;
end;


function TEvaluationFlow.opl_key(PL: TVList): TVChainPointer;
var i: integer; L: TVList; HT: TVHashTable; key: TValue;
begin try
    result := nil;
    result := eval_link(PL.look[1]);
    key := nil;
    key := eval(PL[2]);

    if tpList(result.look)
    then begin
        L := result.look as TVList;
        i := L.high-1;
        while i>=0 do
            if equal(key, L.look[i])
            then begin
                result.add_index(i+1);
                Exit;
            end
            else Dec(i,2);
        //если ничего не найдено
        result.Free;
        result := TVChainPointer.Create(NewVariable(TVList.Create, true));
    end
    else

    if tpHashTable(result.look)
    then begin
        HT := result.look as TVHashTable;
        i := HT.GetIndex(key);
        if i>=0
        then result.add_index(i)
        else begin
            result.Free;
            result := TVChainPointer.Create(NewVariable(TVList.Create, true));
        end;
    end
    else

    raise ELE.Create(result.look.AsString + '  is not key-value list or hash table');
finally
    key.Free;
end;
end;

function TEvaluationFlow.eval_link(P: TValue): TVChainPointer;
var i: integer; head: TValue;
begin try
    //эта функция должна попытаться вычислить указатель на место
    //если выражение не является корректным указанием на место,
    //то создать новую константу, содержащую значение выражения, и вернуть
    //указатель на неё
    result := nil;
    head := nil;

    if tpOrdinarySymbol(P) then begin
        i := stack.index_of((P as TVSymbol).uname);
        if stack.stack[i].V.V is TVChainPointer
        then result := stack.stack[i].V.V.Copy as TVChainPointer
        else result := TVChainPointer.Create(RefVariable(stack.stack[i].V));
        exit;
    end;

    if tpSelfEvaluating(P) then begin
        result := TVChainPointer.Create(NewVariable(P.Copy, true));
        exit;
    end;

    try
        if tpListNotEmpty(P) then begin
            head := eval((P as TVList)[0]);
            if tpOperator(head) then begin
                case (head as TVOperator).op_enum of
                    oeELT: result := opl_elt(P as TVList);
                    oeKEY: result := opl_key(P as TVList);
                    else result := TVChainPointer.Create(NewVariable(eval(P.Copy), true));
                end;
                exit;
            end;
            result := TVChainPointer.Create(NewVariable(eval(P.Copy), true));
            exit;
        end;
    finally
        head.Free;
    end;

    raise ELE.Create('eval_link не обработанный случай');
except
    on E:ELE do begin
        E.EStack := 'eval link '+P.AsString+new_line+'=> '+E.EStack;
        raise ELE.Create(E.Message, E.EClass, E.EStack);
    end;
end;
end;


function TEvaluationFlow.op_and                     (PL: TVList): TValue;
var pc: integer;
begin
    result := TVT.Create;
    for pc := 1 to PL.count-1 do begin
        FreeAndNil(result);
        result := eval(PL[pc]);
        if tpNIL(result) then exit;
    end;
end;

function TEvaluationFlow.op_block                   (PL: TVList): TValue;
begin
    result := oph_block(PL, 1, true);
end;

function TEvaluationFlow.op_case                    (PL: TVList): TValue;
var i: integer; expr: TValue;
begin
    if Pl.Count<2 then raise ELE.Malformed('CASE');

    for i := 2 to PL.High do
        if tpNIL(PL.look[i]) then raise ELE.Malformed('CASE');
    //TODO: CASE не проверяет наличие OTHERWISE в середине списка альтернатив
try
    expr := nil;
    result := nil;
    expr := eval(PL[1]);

    for i := 2 to PL.High do
        if equal(expr, PL.L[i].look[0])
            or (tpListNotEmpty(PL.L[i].look[0]) and
                    ifh_member(PL.L[i].L[0], expr))
            or tpT(PL.L[i].look[0])
            or vpSymbol_OTHERWISE(PL.L[i].look[0])
        then begin
            result := oph_block(PL.L[i], 1, false);
            exit;
        end;
    raise ELE.Create('CASE without result', 'invalid parameters');
finally
    expr.Free;
end;
end;

function TEvaluationFlow.op_pop                     (PL: TVList): TValue;
var CP: TVChainPointer;
begin
    oph_eval_link_for_modification(CP, PL.look[1]);
try
    if not tpList(CP.look) then raise ELE.Create('target is not list');

    result := (cp.look as TVList).POP;
finally
    CP.Free;
end;
end;

function TEvaluationFlow.op_push                    (PL: TVList): TValue;
var i: integer; CP: TVChainPointer; target: TValue;
begin
    oph_eval_link_for_modification(CP, PL.look[1]);
try
    target := CP.look;
    if not tpList(target) then raise ELE.Create('target is not list');

    for i := 2 to PL.High do (CP.look as TVList).Add(eval(PL[i]));

    result := TVT.Create;
finally
    CP.Free;
end;
end;

function TEvaluationFlow.op_const                   (PL: TVList): TValue;
var tmp: TValue;
begin
  //WriteLn(PL.AsString());
    if (PL.Count<>3) or not (tpOrdinarySymbol(PL.look[1]) or tpList(PL.look[1]))
    then raise ELE.malformed('CONST');

    try
        tmp := nil;
        tmp := eval(PL[2]);
        oph_bind(PL.look[1], tmp, true);
    finally
        tmp.Free;
    end;

    result := TVT.Create;
end;


procedure oph_debug_print_graph(V: PVariable);
type TVarRec = record P: PVariable; ref_count: integer; end;
var vars: array of TVarRec; links: array of TVarRec;
    indent, i, j, c: integer;
    clear: boolean;
    function registered_node(P: PVariable): boolean;
    var i: integer;
    begin
        result := true;
        for i := 0 to high(vars) do if vars[i].P=pointer(P) then Exit;
        result := false;
    end;

    procedure add_link(P: PVariable);
    begin
        SetLength(links, Length(links)+1);
        links[high(links)].P:=P;
        links[high(links)].ref_count:=P.references;
    end;

    procedure add_node(p: PVariable);
    var proc: TVProcedure; i,j: integer;
    begin
        if not registered_node(P) then begin
            SetLength(vars, length(vars)+1);
            vars[high(vars)].P:=P;
            vars[high(vars)].ref_count:=P.references;

            proc := P.V as TVProcedure;
            for j := 1 to indent do Write('   ');
            WriteLn(PointerToStr(P), '  ', P.references,' ', proc.AsString);
            Inc(indent);
            for i:=0 to high(proc.stack.stack) do begin
                if (proc.stack.stack[i].V<>nil) and tpProcedure(proc.stack.stack[i].V.V)
                then begin
                    add_node(proc.stack.stack[i].V);
                    add_link(proc.stack.stack[i].V);
                end;
            end;
            dec(indent);
        end;

    end;

begin
    add_link(V);
    indent := 0;
    if tpProcedure(V.V) then add_node(V);
    WriteLn();

    for i := 0 to high(links) do
        WriteLn(PointerToStr(links[i].P), '  ', (links[i].P.V as TVProcedure).AsString);

    clear := true;
    for i := 0 to high(vars) do begin
        c := 0;
        for j := 0 to high(links) do begin
            if vars[i].P = links[j].P then Inc(c);
        end;
        if c<vars[i].ref_count then begin
            clear := false;
            Break;
        end;
        if c>vars[i].ref_count then WriteLn('WARNING: нарушение ссылочной целостности');
    end;

    WriteLn(clear);
end;

function TEvaluationFlow.op_debug(PL: TVList): TValue;
var tmp: TValue;
begin
    result := TVT.Create;

    if (PL.Count=2) and vpKeyword_RESET_STACK(PL.look[1]) then begin
        main_stack.Free;
        main_stack := base_stack.Copy as TVSymbolStack;
        stack := main_stack;
        exit;
    end;

     if (PL.Count=2) and vpKeyword_RESET_PACKAGES(PL.look[1]) then begin
        FreePackages;
        exit;
    end;

    if (PL.Count>=2) and vpKeyword_PRINT_STACK(PL.look[1]) then begin
        if (PL.Count=3) and vpIntegerNotNegative(PL.look[2])
        then stack.Print(PL.I[2])
        else stack.Print;
        exit;
    end;

    if (PL.Count=3) and vpKeyword_PRINT_HASH_TABLE(PL.look[1]) then begin
        tmp := eval(PL[2]);
        (tmp as TVHashTable).print;
        tmp.Free;
        exit;
    end;

    //if (PL.Count>=2) and vpKeyword_THREADS_COUNT(PL.look[1]) then begin
    //    if (PL.Count=3) and vpIntegerNotNegative(PL.look[2])
    //    then set_threads_count(PL.I[2]);
    //    exit;
    //end;

    if (PL.Count=3) and vpKeyword_SHOW_GRAPH(PL.look[1]) then begin
        oph_debug_print_graph(stack.look_var(PL.SYM[2].N));
    end;

end;

function TEvaluationFlow.op_default                 (PL: TVList): TValue;
var CP: TVChainPointer;
begin
    if (PL.Count<>3) or not tpOrdinarySymbol(PL.look[1])
    then raise ELE.malformed('DEFAULT');

    CP := eval_link(PL.look[1]);
try
    if tpNIL(CP.look)
    then
        if CP.constant
        then
            stack.new_var(PL.SYM[1], eval(PL[2]), true)
        else begin
            CP.CopyOnWrite;
            CP.set_target(eval(PL[2]));
        end;

    result := TVT.Create;
finally
    CP.Free;
end;
end;

function TEvaluationFlow.op_delete(PL: TVList): TValue;
var CP: TVChainPointer; marks: array of boolean;
    indices, target, ind: TVList; arg: TValue;
    i, j: integer;
begin
    if PL.Count<>3 then raise ELE.Malformed('DELETE');
    arg := nil;
    ind := nil;
    indices := nil;
try
    oph_eval_link_for_modification(CP, PL.look[1]);

    if not tpList(CP.look) then raise ELE.Create('target is not list');
    target := CP.look as TVList;

    arg := oph_eval_indices(eval(PL[2]), target);

    if tpList(arg)
    then indices:=arg as TVList
    else begin
        ind := TVList.Create([arg], false);
        indices := ind;
    end;

    SetLength(marks, target.Count);
    for i := 0 to high(marks) do marks[i] := false;
    for i := 0 to indices.high do
        if tpInteger(indices.look[i]) and (indices.I[i]>=0) and (indices.I[i]<=target.high)
        then marks[indices.I[i]]:=true
        else
            if tpRange(indices.look[i]) and ((indices.look[i] as TVRange).low>=0)
                and ((indices.look[i] as TVRange).high<=target.count)
            then for j := (indices.look[i] as TVRange).low to (indices.look[i] as TVRange).high-1
                    do marks[j] := true
            else
                if (vpSymbol_LAST(indices.look[i]) or vpKeyword_LAST(indices.look[i]))
                    and (target.Count>0)
                then marks[high(marks)] := true
                else
                    raise ELE.InvalidParameters;
    for i := target.high downto 0 do
        if marks[i] then target.delete(i);

    result := TVT.Create;
finally
    CP.Free;
    arg.Free;
    ind.Free;
end;
end;

function TEvaluationFlow.op_elt                     (PL: TVList): TValue;
var CP: TVChainPointer;
begin
    if PL.Count<2 then raise ELE.malformed('ELT');

    CP := opl_elt(PL);
    result := CP.value;
    CP.Free;
end;

function TEvaluationFlow.op_error(PL: TVList): TValue;
var emsg, eclass: TValue; smsg,sclass,sstack: unicodestring;
    P: PVariable;
    sym_eclass, sym_emsg, sym_estack: TVSymbol;
begin
    result:=nil;
    //если задан класс [и сообщение] то вызывается новое исключение
    //если параметры не заданы, то в текущем стеке ищется ранее возникшее исключение
    //стэк исключения всегда ищется в стеке и если не найден принимается пустым
    if PL.Count>3 then raise ELE.Malformed('ERROR');

    sclass := '';
    if PL.Count>1 then try
        eclass := eval(PL[1]);
        if tpString(eclass)
        then sclass := (eclass as TVString).S
        else raise ELE.InvalidParameters;
    finally
        eclass.Free;
    end;

    smsg := '';
    if PL.Count>2 then try
        emsg := eval(PL[2]);
        if tpString(emsg)
        then smsg := (emsg as TVString).S
        else raise ELE.InvalidParameters;
    finally
        emsg.Free;
    end;

    sstack := '';
    try
        sym_estack := TVSymbol.Create('EXCEPTION-STACK');
        P := nil;
        P := stack.find_ref_or_nil(sym_estack);
        if (p<>nil) and tpString(p.V)
        then sstack := (P.V as TVString).S
        else sstack := '';
    finally
        sym_estack.Free;
        ReleaseVariable(P);
    end;

    if PL.Count=1 then try
        sym_eclass := TVSymbol.Create('EXCEPTION-CLASS');
        P := nil;
        P := stack.find_ref_or_nil(sym_eclass);
        if (p<>nil) and tpString(p.V)
        then sclass := (P.V as TVString).S
        else raise ELE.Create('exception not found','syntax');
    finally
        sym_eclass.Free;
        ReleaseVariable(P);
    end;

    if PL.Count=1 then try
        sym_emsg := TVSymbol.Create('EXCEPTION-MESSAGE');
        P := nil;
        P := stack.find_ref_or_nil(sym_emsg);
        if (p<>nil) and tpString(p.V)
        then smsg := (P.V as TVString).S
        else raise ELE.Create('exception not found','syntax');
    finally
        sym_emsg.Free;
        ReleaseVariable(P);
    end;

    raise ELE.Create(smsg, sclass, sstack);
end;

function TEvaluationFlow.op_execute_file(PL: TVList): TValue;
var fn: TValue;
begin
    if PL.Count<>2 then raise ELE.Malformed('EXECUTE-FILE');

try
    result := nil;
    fn := nil;
    fn := eval(PL[1]);
    if not tpString(fn) then raise ELE.InvalidParameters;

    oph_execute_file((fn as TVString).S);
    result := TVT.Create;
finally
    fn.Free;
end;

end;


function TEvaluationFlow.op_set                     (PL: TVList): TValue;
var CP :TVChainPointer;
begin try
    //TODO: set не падает если устанавливает параметр функции переданный по значению
    if (PL.Count<3) or (PL.Count>3) then raise ELE.InvalidParameters;

    oph_eval_link_for_modification(CP, PL.look[1]);

    CP.set_target(eval(PL[2]));

    result := TVT.Create;
finally
    FreeAndNil(CP);
end end;



function TEvaluationFlow.op_return(PL: TVList): TValue;
begin
    if PL.Count>2 then raise ELE.Malformed('RETURN');

    result := nil;
    case PL.Count of
        2: result := TVReturn.Create(eval(PL[1]));
        1: result := TVReturn.Create(TVList.Create);
    end;
end;

function TEvaluationFlow.op_cond                    (PL: TVList): TValue;
var i: integer; tmp: TValue;
begin try
    case valid_sign(PL, result, [@tpOperator,
        0,-1, @tpListNotEmpty]) of
        1: begin
            tmp := TVList.Create;
            for i := 1 to PL.Count-1 do begin
                FreeAndNil(tmp);
                tmp := eval(PL.L[i][0]);
                if not tpNIL(tmp)
                then begin
                    FreeAndNil(tmp);
                    tmp := oph_block(PL.L[i], 1, false);
                    break;
                end;
            end;
        end;
        0: raise ELE.malformed('COND');
    end;
    result := tmp;
except
    tmp.Free; raise;
end; end;

{$DEFINE FORSUBSEQ}

function TEvaluationFlow.op_for                     (PL: TVList): TValue;
var i, frame_start, high_i, low_i: integer;
    V: TValue;
    CP: TVChainPointer;
    index: TVInteger;
    op_var : (sequence, range, times, subseq);
    {$IFDEF FORSUBSEQ}
    procedure ss_expr(E: TVList);
    var l,h: TValue;
    begin try
        l := nil;
        h := nil;
        if not E.Count in [3..4] then raise ELE.Malformed('FOR subseq');
        op_var := sequence;
        CP := eval_link(E.look[1]);
        if not (CP.look is TVSequence) then raise ELE.InvalidParameters;
        l := eval(E[2]);
        case E.Count of
            3: if vpIntegerNotNegative(l) then begin
                low_i := (l as TVInteger).fI;
                high_i := (CP.look as TVSequence).high;
            end
            else
                if tpRange(l) then begin
                    low_i := (l as TVRange).low;
                    high_i := (l as TVRange).high-1;
                end
                else raise ELE.InvalidParameters;
            4: begin
                h := eval(E[3]);
                if vpIntegerNotNegative(l) and vpIntegerNotNegative(h) then begin
                    low_i := (l as TVInteger).fI;
                    high_i := (h as TVInteger).fI-1;
                end;
            end;
        end;
    finally
        l.Free;
        h.Free;
    end;
    end;
    {$ENDIF}

begin
    //оператор цикла возвращает NIL в случае завершения и последнее значение
    //итерируемой переменной в случае досрочного прерывания оператором (BREAK)
    //это упрощает написание функций поиска

    if (PL.count<3) or not tpOrdinarySymbol(PL.look[1])
    then raise ELE.malformed('FOR');

    {$IFDEF FORSUBSEQ}
    if vpListHeaded_SUBSEQ(PL.look[2])
    then ss_expr(PL.L[2])
    else begin
        CP := eval_link(PL.look[2]);
        if tpSequence(CP.look) then begin
            op_var := sequence;
            low_i := 0;
            high_i := (CP.look as TVSequence).high;
        end
        else
            if tpRange(CP.look) then begin
                op_var := range;
                low_i := (CP.look as TVRange).low;
                high_i := (CP.look as TVRange).high-1;
            end
            else
                if vpIntegerNotNegative(CP.look) then begin
                    op_var := range;
                    low_i := 0;
                    high_i := (CP.look as TVInteger).fI-1;
                end
                else
                    raise ELE.InvalidParameters;
    end;
    {$ELSE}
    CP := eval_link(PL.look[2]);

    if tpCompoundIndexed(CP.look) then op_var := sequence
    else
        if tpRange(CP.look) then op_var := range
        else
            if vpIntegerNotNegative(CP.look) then op_var := times
            else
                raise ELE.InvalidParameters;
    {$ENDIF}

try
    V := nil;
    result := nil;
    frame_start := stack.Count;

    {$IFDEF FORSUBSEQ}
        case op_var of
        sequence: begin
            CP.add_index(0);
            stack.new_var(PL.SYM[1], CP, true);
            for i := low_i to high_i do begin
                CP.set_last_index(i);
                FreeAndNil(V);
                V := oph_block(PL, 3, true);
                if tpBreak(V) then begin
                    result := CP.value;
                    break;
                end;
                if tpReturn(V) then begin
                    result := V.Copy;
                    break;
                end;
            end;
        end;

        range: begin
            CP.Free;
            index := TVInteger.Create(0);
            stack.new_var(PL.SYM[1], index, true);
            for i := low_i to high_i do begin
                index.fI := i;
                FreeAndNil(V);
                V := oph_block(PL, 3, true);
                if tpBreak(V) then begin
                    result := TVInteger.Create(i);
                    break;
                end;
                if tpReturn(V) then begin
                    result := V.Copy;
                    break;
                end;
            end;
        end;
    end;
    {$ELSE}
    case op_var of
        sequence: begin
            high_i := (CP.look as TVCompoundIndexed).high;
            CP.add_index(0);
            stack.new_var(PL.SYM[1], CP, true);
            for i := 0 to high_i do begin
                CP.set_last_index(i);
                FreeAndNil(V);
                V := oph_block(PL, 3, true);
                if tpBreak(V) then begin
                    result := CP.value;
                    break;
                end;
                if tpReturn(V) then begin
                    result := V.Copy;
                    break;
                end;
            end;
        end;

        range, times: begin
            case op_var of
                range: begin
                    high_i := (CP.look as TVRange).high-1;
                    low_i := (CP.look as TVRange).low;
                end;
                times: begin
                    high_i := (CP.look as TVInteger).fI-1;
                    low_i := 0;
                end;
            end;
            CP.Free;
            index := TVInteger.Create(0);
            stack.new_var(PL.SYM[1], index, true);
            for i := low_i to high_i do begin
                index.fI := i;
                FreeAndNil(V);
                V := oph_block(PL, 3, true);
                if tpBreak(V) then begin
                    result := TVInteger.Create(i);
                    break;
                end;
                if tpReturn(V) then begin
                    result := V.Copy;
                    break;
                end;
            end;
        end;
    end;
    {$ENDIF}

    if result=nil then result:= TVList.create;
finally
    stack.clear_frame(frame_start);
    V.Free;
end;
end;

function TEvaluationFlow.op_goto                    (PL: TVList): TValue;
begin
    if (PL.Count<>2) or not tpSymbol(PL.look[1])
    then raise ELE.malformed('GOTO');

    result := TVGoto.Create(PL.SYM[1]);
end;

function TEvaluationFlow.op_if                      (PL: TVList): TValue;
var condition: TValue;
begin
    if (PL.Count<3) or (PL.Count>4)
        //TODO: эти проверки синтаксиса IF нужны только для более ясного сообщения об ошибке
        //их отсутствие приведёт к падению с сообщением THEN/ELSE is not subprogram
        //or vpListHeaded_ELSE(PL.look[2])
        //or ((PL.Count=4) and vpListHeaded_THEN(PL.look[3]))
    then raise ELE.malformed('IF');
try
    condition := nil;
    condition := eval(PL[1]);

    if not tpNIL(condition)
    then begin
        if vpListHeaded_THEN(PL.look[2])
        then result := oph_block(PL.L[2], 1, false)
        else result := eval(PL[2]);
    end
    else
        if PL.Count=4
        then begin
            if vpListHeaded_ELSE(PL.look[3])
            then result := oph_block(PL.L[3], 1, false)
            else result := eval(PL[3]);
        end
        else result := TVList.Create;
finally
    condition.Free;
end;
end;

function TEvaluationFlow.op_if_nil                  (PL: TVList): TValue;
begin
    if PL.Count<>3 then raise ELE.malformed('IF-NIL');

    result := eval(PL[1]);
    if tpNIL(result)
    then begin
        FreeAndNil(result);
        result := eval(PL[2])
    end;
end;

function TEvaluationFlow.op_insert(PL: TVList): TValue;
var CP: TVChainPointer; target: TVList; arg: TValue; i: integer;
begin
    if PL.Count<>4 then raise ELE.Malformed('INSERT');
    arg := nil;
try
    oph_eval_link_for_modification(CP, PL.look[1]);
    if not tpList(CP.look) then raise ELE.Create('target is not list');
    target := CP.look as TVList;

    arg := oph_eval_indices(eval(PL[2]), target);

    if tpInteger(arg)
    then i := (arg as TVInteger).fI
    else
        if vpKeyword_LAST(arg) or vpSymbol_LAST(arg)
        then i := target.Count
        else
            raise ELE.InvalidParameters;

    if (i<0) or (i>target.Count) then ELE.InvalidParameters;

    target.insert(i, eval(PL[3]));

    result := TVT.Create;
finally
    CP.Free;
    arg.Free;
end;
end;

function TEvaluationFlow.op_key(PL: TVList): TValue;
var CP: TVChainPointer;
begin
    if PL.Count<>3 then raise ELE.Malformed('KEY');

    CP := opl_key(PL);
    result := CP.value;
    CP.Free;
end;

function TEvaluationFlow.op_let(PL: TVList): TValue;
var old_v, VPL: TVList; i, j, count: integer;
begin
    //временно изменяет значение переменной, по завершении возвращает исходное
    //значение
    if (PL.Count<3) or not vpListOfSymbolValuePairs(PL.look[1])
    then raise ELE.malformed('LET');

    VPL := PL.L[1];
    try
        old_v := TVList.Create;
        for i := 0 to VPL.High do old_v.Add(eval(VPL.L[i][0]));

        try
            count := 0;
            for i := 0 to VPL.High do begin
                stack.set_var(VPL.L[i].SYM[0], eval(VPL.L[i][1]));
                Inc(count);
            end;

            result := oph_block(PL,2, false);
        finally
            for j := 0 to count-1 do
                stack.set_var(VPL.L[j].SYM[0], old_v[j]);
        end;
    finally
        FreeAndNil(old_v);
    end;
end;

function TEvaluationFlow.op_macro_symbol(PL: TVList): TValue;
var proc: TVProcedure; sl: TVList;
begin
    //TODO: макросимвол не поддерживает чистый лямбда режим
    result := nil;

    if (PL.Count<3) then raise ELE.Malformed('MACRO-SYMBOL');
    if not tpOrdinarySymbol(PL.look[1]) then raise ELE.InvalidParameters;


    proc := TVMacroSymbol.Create;
    proc.nN := PL.SYM[1].N;

    result := proc;
    proc.stack_pointer := -1;
    proc.home_stack := nil;
    proc.body := PL.Subseq(2, PL.Count) as TVList;
    proc.sign := TVList.Create;
    proc.rests := TVRecord.Create;

    proc.stack := TVSymbolStack.Create(nil);
    try
        sl := extract_body_symbols(proc.body);
        fill_subprogram_stack(proc, sl);
    finally
        FreeAndNil(sl);
    end;

    proc.stack.remove_unbound;
    proc.evaluated := true;

    stack.new_var(PL.SYM[1], result.Copy, true);
end;

function TEvaluationFlow.op_or                      (PL: TVList): TValue;
var pc: integer;
begin
    result := TVList.Create;
    for pc := 1 to PL.count-1 do begin
        FreeAndNil(result);
        result := eval(PL[pc]);
        if not tpNIL(result) then exit;
    end;
end;

function TEvaluationFlow.op_package(PL: TVList): TValue;
var external_stack, package_stack: TVSymbolStack;
    pack: TPackage;
begin
    if (PL.Count<4)
        or (not tpOrdinarySymbol(PL.look[1]))
        or (not tpListOfOrdinarySymbols(PL.look[2]))
    then raise ELE.Malformed('PACKAGE');

    if FindPackage(PL.uname[1])<>nil
    then raise ELE.Create('package redefinition: '+PL.name[1], 'syntax');

    package_stack := base_stack.Copy as TVSymbolStack;
    external_stack := stack;
    stack := package_stack;
    result := nil;
try
    result := oph_block(PL, 3, false);

    //  сохранение пакета
    pack := TPackage.Create;
    pack.name := PL.name[1];
    pack.uname := PL.uname[1];
    pack.export_list := PL[2] as TVList;
    pack.stack := package_stack.Copy as TVSymbolStack;
    AddPackage(pack);

    Result.Free;
    result := TVT.Create;
finally
    stack := external_stack;
    package_stack.Free;
end;
end;


function TEvaluationFlow.op_append(PL: TVList): TValue;
var CP: TVChainPointer; i: integer; PLI: TVList;
begin
    if PL.Count<3 then raise ELE.InvalidParameters;
try
    //TODO: слишком много копирований
    //TODO: нет предварительной проверки корректности параметров
    // в случае несоответствия параметров программа упадёт при
    // попытке приведения типов

    PLI := TVList.Create;
    for i := 2 to PL.high do PLI.Add(eval(PL[i]));

    oph_eval_link_for_modification(CP, PL.look[1]);

    if CP.look is TVString
    then
        for i := 0 to PLI.high do
            (CP.look as TVString).S := (CP.look as TVString).S + PLI.S[i]


    else if CP.look is TVList
    then
        for i := 0 to PLI.high do (CP.look as TVList).Append(PLI[i] as TVList)


    else if CP.look is TVBytes
    then
        for i := 0 to PLI.high do
            (CP.look as TVBytes).append(PLI[i] as TVBytes)


    else raise ELE.InvalidParameters;

    result := TVT.Create;
finally
    FreeAndNil(CP);
    PLI.Free;
end
end;


function TEvaluationFlow.op_assemble(PL: TVList): TValue;
    function asmbl(L: TVList; from: integer = 0): TVList;
    var i, j: integer; tmp: TValue;
    begin
        result := TVList.Create;
        for i := from to L.high do begin
            if vpListHeaded_VALUE(L.look[i])
            then result.Add(eval(L.L[i][1]))

            else if vpListHeaded_INSET(L.look[i])
            then try
                tmp := nil;
                tmp := eval(L.L[i][1]);
                if not tpList(tmp) then raise ELE.Malformed('INSET');
                for j := 0 to (tmp as TVList).high do
                    result.Add((tmp as TVList)[j]);
            finally
                tmp.Free;
            end

            else if tpList(L.look[i])
            then result.Add(asmbl(L.L[i]))

            else result.Add(L[i]);
        end
    end;
begin
    // этот оператор должен работать аналогично BACKQUOTE в Лиспе
    // использует вторичные операторы VALUE и INSET

    result := asmbl(PL, 1);
end;

function TEvaluationFlow.op_procedure               (PL: TVList): TValue;
var proc: TVProcedure; sl, rl: TVList;
    sign_pos, i: integer;
    function rest_names(L: TVList): TVList;
    var i: integer;
    begin
        result := TVList.Create;
        for i := 0 to L.high do begin
            if tpList(L.look[i]) then result.Append(rest_names(L.L[i]));
            if vpKeyword_REST(L.look[i]) then begin
                if L.high=(i+1)
                then result.Add(L[i+1])
                else raise ELE.Malformed(L.AsString);
            end;
        end;
    end;

begin
    result := nil;

    if (PL.count>=3) and tpOrdinarySymbol(PL.look[1]) and tpList(PL.look[2])
    then sign_pos := 2
    else
        if (PL.count>=2) and tpList(PL.look[1])
        then sign_pos := 1
        else
            raise ELE.InvalidParameters;

    if (PL.look[0] as TVOperator).op_enum=oeMACRO
    then proc := TVMacro.Create
    else proc := TVProcedure.Create;

    if sign_pos=2 then proc.nN := PL.SYM[1].N;

    result := proc;
    proc.stack_pointer := stack.count;
    proc.home_stack := stack;
    proc.body := PL.Subseq(sign_pos+1, PL.Count) as TVList;
    proc.evaluated:=false;

    proc.sign := PL[sign_pos] as TVList;

    proc.stack := TVSymbolStack.Create(nil);
    try
        rl := nil;
        sl := extract_body_symbols(proc.body);
        fill_subprogram_stack(proc, sl);
        rl := rest_names(proc.sign);
        proc.rests := TVRecord.Create;

        for i := 0 to rl.high do
            proc.rests.AddSlot(rl.SYM[i], TVList.Create);
    finally
        FreeAndNil(sl);
        FreeAndNil(rl);
    end;

    if sign_pos=2 then
        stack.new_var(PL.SYM[1],result.Copy, true);

end;

function TEvaluationFlow.op_var                     (PL: TVList): TValue;
var tmp: TValue;
begin
    if (PL.Count<2) or (PL.Count>3)
        or not (tpOrdinarySymbol(PL.look[1]) or tpList(PL.look[1]))
    then raise ELE.malformed('VAR');

    case PL.Count of
        2: if tpOrdinarySymbol(PL.look[1])
            then stack.new_var(PL.SYM[1], TVList.Create)
            else raise ELE.Malformed('VAR');
        3: try
                tmp := nil;
                tmp := eval(PL[2]);
                oph_bind(PL.look[1], tmp, false);
            finally
                FreeAndNil(tmp);
            end;
    end;
    result := TVT.Create;
end;

function TEvaluationFlow.op_when(PL: TVList): TValue;
begin
    if PL.Count<2 then raise ELE.malformed('WHEN');

    result := eval(PL[1]);

    if not tpNIL(result)
    then begin
        FreeAndNil(result);
        result := oph_block(PL, 2, false)
    end;
end;

function TEvaluationFlow.op_while                   (PL: TVList): TValue;
var cond, V: TValue;
begin
    if PL.Count<2 then raise ELE.malformed('WHILE');
try
    result := TVList.Create;
    V := nil;
    cond := nil;
    cond := eval(PL[1]);
    while not tpNIL(cond) do begin
        V := oph_block(PL, 2, true);

        if tpBreak(V) then break;
        if tpReturn(V) then begin
            result := V.Copy;
            break;
        end;
        FreeAndNil(V);
        FreeAndNil(cond);
        cond := eval(PL[1]);
    end;
finally
    FreeAndNil(cond);
    FreeAndNil(V);
end;

end;

function TEvaluationFlow.op_with(PL: TVList; export_symbols: boolean): TValue;
var i, p: integer;
begin
    if (PL.Count<2) then raise ELE.Malformed('WITH/USE');
    for i := 1 to PL.high do
        if not (tpOrdinarySymbol(PL.look[i]) or tpString(PL.look[i]))
        then raise ELE.InvalidParameters;

    result := nil;

    for i := 1 to PL.high do
        if tpString(PL.look[i])
        then oph_execute_file(DirSep(PL.S[i]))
        else

        if vpSymbolQualified(PL.look[i])
        then begin
            if export_symbols
            then stack.new_ref(
                    PL.uname[i][Pos(':',PL.name[i])+1..Length(PL.name[i])],
                    stack.find_ref(PL.SYM[i]))
            else raise ELE.Create('invalid package name '+PL.name[i], 'syntax')
        end
        else

        if tpOrdinarySymbol(PL.look[i])
        then oph_bind_package(PL.name[i], export_symbols);

    result := TVT.Create;
end;

function TEvaluationFlow.extract_body_symbols(body: TVList): TVList;
var i: integer; tmp: TVList;
    function is_op_name(name: unicodestring): boolean;
    var i: TOperatorEnum;
    begin
        result := true;
        for i := low(ops) to high(ops) do if ops[i].n = name then exit;
        result := false;
    end;

begin
    //эта функция должна извлекать из тела процедуры или функции список,
    //используемых символов
    //TODO: не извлекать символы объявленные в теле

    result := TVList.Create;
    if tpNIL(body) then exit;

    for i := 0 to body.High do begin
        if tpOrdinarySymbol(body.look[i]) and not is_op_name(body.uname[i])
            then result.Add(body[i]);
        if tpList(body.look[i])
        then result.Append(extract_body_symbols(body.L[i]));
    end;

    try
        //здесь ifh_union используется для удаления дубликатов
        tmp := TVList.Create([result]);
        result := ifh_union(tmp);
    finally
        FreeAndNil(tmp);
    end;

end;

procedure TEvaluationFlow.fill_subprogram_stack(sp: TVProcedure;
    symbols: TVList);
var i: integer;
begin
    //эта процедура заполняет стэк подпрограммы, символами из переданного списка
    //символы для которых не найдено значения оставляются несвязанными
    //на случай если они позднее будут использоваться для рекурсивного вызова
    //игнорирование несвязанных символов так же позволяет работать при
    //ошибочном включении лишних символов процедурой extract_body_symbols
    //реальные ошибки будут проявляться как ошибка symbol not bound  на этапе
    //исполнения подпрограммы
    //несвязанные символы должны быть удалены при довычислении подпрограммы
    //с целью избежания доступа по нулевым указателям

    for i := 0 to symbols.High do
        sp.stack.new_ref(
            symbols.SYM[i],
            stack.find_ref_or_nil(symbols.SYM[i]));
end;

function TEvaluationFlow.call(PL: TVList): TValue;
var head: TValue;
begin
   head := PL.look[0];
   if head.ClassType = TVProcedure then result := call_procedure(PL)
   else
   if head.ClassType = TVInternalFunction then result := call_internal(PL)
   else
   if head.ClassType = TVPredicate then result := call_predicate(PL)
   else
   if head.ClassType = TVOperator then result := call_operator(PL)
   else
   result := eval(PL.Copy);
end;

function TEvaluationFlow.call_procedure(PL: TVList): TValue;
var proc: TVProcedure; params: TVList; tmp_stack, proc_stack: TVSymbolStack;
    tmp: TValue;
begin try

    params := nil;
    proc_stack := nil;
    proc := PL.look[0] as TVProcedure;
    proc.Complement;

    proc_stack := proc.stack.Copy as TVSymbolStack;
    params := PL.CDR;
    oph_bind(proc.sign, params, true, proc_stack, proc.rests);

    tmp_stack := stack;
    stack := proc_stack;
    result := oph_block(proc.body,0,false);
    if tpReturn(result) then begin
            tmp := (result as TVReturn).value.Copy;
            result.Free;
            result := tmp;
        end;
finally
    stack := tmp_stack;
    params.Free;
    proc_stack.Free;
end;
    //DONE: при очистке стэка, рекурсивные процедуры не освобождаются
end;

function TEvaluationFlow.call_macro(PL: TVList): TValue;
begin
    result := nil;
end;

function TEvaluationFlow.call_internal(PL: TVList): TValue;
var binded_PL, params: TVList;
begin try
    binded_PL := nil;
    params := PL.CDR;
    binded_PL := bind_parameters_list(params,
                        (PL.look[0] as TVInternalFunction).signature);
    result := nil;
    result := (PL.look[0] as TVInternalFunction).body(binded_PL, call);
finally
    FreeAndNil(binded_PL);
    params.Free;
end;
end;

function TEvaluationFlow.call_operator(PL: TVList): TValue;
begin
    case (PL.look[0] as TVOperator).op_enum of
        oeAND_THEN  : result := op_and(PL);
        oeAPPEND    : result := op_append(PL);
        oeASSEMBLE  : result := op_assemble(PL);
        oeBLOCK     : result := op_block(PL);
        oeBREAK     : result := TVBreak.Create;
        oeCASE      : result := op_case(PL);
        oeCOND      : result := op_cond(PL);
        oeCONST     : result := op_const(PL);
        oeCONTINUE  : result := TVContinue.Create;
        oeDEBUG     : result := op_debug(PL);
        oeDEFAULT   : result := op_default(PL);
        oeDELETE    : result := op_delete(PL);
        oeELT       : result := op_elt(PL);
        oeERROR     : result := op_error(PL);
        oeEXECUTE_FILE: result := op_execute_file(PL);
        oeFOR       : result := op_for(PL);
        oeGOTO      : result := op_goto(PL);
        oeIF        : result := op_if(PL);
        oeIF_NIL    : result := op_if_nil(PL);
        oeINSERT    : result := op_insert(PL);
        oeKEY       : result := op_key(PL);
        oeLET       : result := op_let(PL);
        oeMACRO     : result := op_procedure(PL);
        oeMACRO_SYMBOL: result := op_macro_symbol(PL);
        oeOR_THEN   : result := op_or(PL);
        oePACKAGE   : result := op_package(PL);
        oePOP       : result := op_pop(PL);
        oePROCEDURE : result := op_procedure(PL);
        oePUSH      : result := op_push(PL);
        oeQUOTE     : result := PL[1];
        //TODO: QUOTE не проверяет количество аргументов
        oeRETURN    : result := op_return(PL);
        oeSET       : result := op_set(PL);
        oeUSE       : result := op_with(PL, true);
        oeVAR       : result := op_var(PL);
        oeWHEN      : result := op_when(PL);
        oeWHILE     : result := op_while(PL);
        oeWITH      : result := op_with(PL, false);
        else raise ELE.Create('неизвестный оператор');
    end;
end;

function TEvaluationFlow.call_predicate(PL: TVList): TValue;
var res: boolean; P: TVPredicate;
begin
    result := nil;
    if PL.Count<>2 then ELE.Malformed(PL.AsString);
    P :=  PL.look[0] as TVPredicate;

    res := P.body(PL.look[1]);
    if P.fAssert
    then begin
        if res
        then result := PL[1]
        else raise ELE.Create(PL.look[1].AsString+' is not '+TVSymbol.symbol_uname(P.nN), 'assertion');
    end
    else bool_to_TV(res, result);
end;


function TEvaluationFlow.procedure_call(PL: TVList): TValue;
var proc: TVProcedure; params: TVList;
    i: integer;
    linkable: boolean;
begin
    //TODO: при вызове процедуры с несуществующими переменными не возникает ошибка

    proc := PL.look[0] as TVProcedure;

    params := TVList.Create([PL[0]]);
    params.SetCapacity(PL.Count);
    result := nil;
try
    //proc.Complement;
    linkable := true;
    for i := 1 to PL.high do
        if proc is TVMacro
        then params.Add(PL[i])
        else begin
            if linkable and not tpOrdinarySymbol(proc.sign.look[i-1])
            then linkable := false;
            if linkable
            then params.Add(eval_link(PL.look[i]))
            else params.Add(eval(PL[i]));

    //есть трудности с деструктурирующей привязкой параметров по ссылке,
    //по этому по ссылке
    //привязываются только обязательные параметры нулевого уровня
        end;

    result := call_procedure(params);

finally
    params.Free;
end;

end;


function TEvaluationFlow.eval_parameters(call: TCallProc; PL: TVList): TValue;
var PLI: TVList; i: integer;
begin try
    //TODO: много лишних копирований при вызове внутренних функций
    PLI := TVList.Create([PL[0]]);
    PLI.SetCapacity(PL.Count);
    for i := 1 to PL.high do PLI.Add(eval(PL[i]));
    result := call(PLI);
finally
    PLI.Free;
end;end;



//{$DEFINE EVAL_DEBUG_PRINT}

{$IFDEF EVAL_DEBUG_PRINT}
var eval_indent: integer=0;
procedure indent;
var i: integer;
begin
    for i := 1 to eval_indent do Write('¦   ');
end;
{$ENDIF}

function TEvaluationFlow.eval(V: TValue): TValue;
var PL: TVList;
    PV: PVariable;
    o: TOperatorEnum;
    type_v: (selfEval, symbol, list, unexpected);
    estack: unicodestring;

label return;
begin try
    {$IFDEF EVAL_DEBUG_PRINT}
    indent; WriteLn('eval>> ',V.AsString);
    //stack.print(78);
    inc(eval_indent);
    {$ENDIF}

    result := nil;

    if tpSelfEvaluating(V)
    then type_v := selfEval
    else
        if tpSymbol(V)
        then type_v := symbol
        else
            if tpList(V)
            then type_v := list
            else
                type_v := unexpected;

    case type_v of

        selfEval: result := V.Copy;

        symbol: begin
            for o := low(ops) to high(ops) do
                if ops[o].nN = (V as TVSymbol).N then begin
                    result := TVOperator.Create(ops[o].nN, o);
                    goto return;
                end;
            try
                PV := nil;
                PV := stack.find_ref(V as TVSymbol);

                if PV.V is TVChainPointer
                then result := (PV.V as TVChainPointer).value
                else result := PV.V.Copy;

                if result is TVMacroSymbol
                then try
                    PL := TVList.Create([result]);
                    result := eval(procedure_call(PL));
                finally
                    FreeAndNil(PL);
                end;
            finally
                ReleaseVariable(PV);
            end;

        end;

        list: begin
            (V as TVList)[0] := eval((V as TVList)[0]);

            if tpInternalFunction((V as TVList).look[0])
            then result := eval_parameters(call_internal, V as TVList)
            else

            if tpPredicate((V as TVList).look[0])
            then result := eval_parameters(call_predicate, V as TVList)
            else

            if tpOperator((V as TVList).look[0])
            then result := call_operator(V as TVList)

            else
                if (V as TVList).look[0] is TVProcedure
                then begin
                    if (V as TVList).look[0] is TVMacro
                    then result := eval(procedure_call(V as TVList))
                    else result := procedure_call(V as TVList);
                end
                else raise ELE.Create((V as TVList).look[0].AsString+' is not subprogram');
        end;

        unexpected: raise ELE.Create('невычисляемый параметр');
    end;


return:
   if result=nil then raise ELE.Create('eval без результата');
   {$IFDEF EVAL_DEBUG_PRINT}
   Dec(eval_indent);
   indent; writeLn('=  ',result.asString);
   {$ENDIF}
   //WriteLn(V.AsString);
   FreeAndNil(V);

except
    on E:ELisyaError do begin
       // WriteLn('eval ELE>> ', E.Eclass,'  ', e.Message);
       //E.Message := V.AsString+new_line+'=> '+E.Message;
       //WriteLn('1>> ',E.EStack);
       E.EStack := V.AsString+new_line+'=> '+E.EStack;
       //WriteLn('2>> ',E.EStack);
        V.Free;
        raise ELE.Create(E.Message, E.EClass, E.EStack);
    end;
    on E:Exception do begin
        //WriteLn('eval E>> ');
        try
            EStack := V.AsString+new_line+'=> ';
        except
            EStack := 'XXXX'+new_line+'=> ';
        end;
  //      stack.Print;
  //      WriteLn('>>>> ',E.Message);
        raise ELE.Create(E.Message, '!'+E.ClassName+'!', EStack);
    end;
end;
end;



initialization
    system.Randomize;
    fill_int_fun_signs;
    fill_base_stack;
    fill_ops_array;
    T := TVT.Create;
    NULL := TVList.Create;

finalization
    NULL.Free;
    T.Free;
    base_stack.Free;
    free_int_fun_signs;
end.
//4576 4431 4488 4643 4499 4701 5166 5096
