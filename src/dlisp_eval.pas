unit dlisp_eval;

{$mode delphi}

{$ASSERTIONS ON}

{$DEFINE mysql55}


interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    process, Classes, SysUtils, math, crc, ucomplex
    , dlisp_values, dlisp_read, lisya_xml, mar,
    lisya_packages
    ,lisya_predicates
    ,lisya_ifh
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
        procedure oph_bind(s, P: TValue; constant: boolean; st: TVSymbolStack=nil);
        function oph_bind_to_list(s, P: TVList): TVList;
        procedure oph_execute_file(fn: unicodestring);

        function opl_elt(PL: TVList): TVChainPointer;
        function opl_last(PL: TVList): TVChainPointer;

        function op_and(PL: TVList): TValue;
        function op_append(PL: TVList): TValue;
        function op_apply(PL: TVList): TValue;
        function op_block(PL: TVList): TValue;
        function op_case(PL: TVList): TValue;
        function op_cond(PL: TVList): TValue;
        function op_const(PL: TVList): TValue;
        function op_debug(PL: TVList): TValue;
        function op_default(PL: TVList): TValue;
        function op_elt(PL: TVList): TValue;
        function op_error(PL: TVList): TValue;
        function op_execute_file(PL: TVList): TValue;
        function op_filter(PL: TVList): TValue;
        function op_for(PL: TVList): TValue;
        function op_goto(PL: TVList): TValue;
        function op_if(PL: TVList): TValue;
        function op_if_nil(PL: TVList): TValue;
        function op_last(PL: TVList): TValue;
        function op_let(PL: TVList): TValue;
        function op_map(PL: TVList): TValue;
        function op_macro_symbol(PL: TVList): TValue;
        function op_or(PL: TVList): TValue;
        function op_package(PL: TVList): TValue;
        function op_pop(PL: TVList): TValue;
        function op_procedure(PL: TVList): TValue;
        function op_push(PL: TVList): TValue;
        function op_secondary_error(PL: TVList): TValue;
        function op_set(PL: TVList): TValue;
        function op_record(PL: TVList): TValue;
        function op_record_as(PL: TVList): TValue;
        function op_return(PL: TVList): TValue;
        function op_val(PL: TVList): TValue;
        function op_var(PL: TVList): TValue;
        function op_when(PL: TVList): TValue;
        function op_while(PL: TVList): TValue;
        function op_with(PL: TVList): TValue;


        function extract_body_symbols(body: TVList): TVList;
        procedure fill_subprogram_stack(sp: TVProcedure; symbols: TVList);

        //function bind_procedure_parameters_to_stack(
        //                    PL: TVList; sign: TSubprogramSignature;
        //                    ts: TVSymbolStack): boolean;
        //function bind_macro_parameters_to_stack(
        //                    PL: TVList; sign: TSubprogramSignature;
        //                    ts: TVSymbolStack): boolean;
//        procedure procedure_complement(V: TValue);

        function call(PL: TVList): TValue;
        function call_procedure(PL: TVList): TValue; inline;
        function call_macro(PL: TVList): TValue;
        function call_internal(PL: TVList): TValue; inline;
        function call_operator(PL: TVList): TValue;
        function call_predicate(PL: TVList): TValue;

        function procedure_call(PL: TVList): TValue;
        function internal_function_call(PL: TVList): TValue;
        function internal_predicate_call(PL: TVList): TValue;
        //procedure expand_ins(PL: TVList);

        function eval_link(P: TValue): TVChainPointer;

        function eval(V: TValue): TValue;
    end;


function execute_file(filename: unicodestring): boolean;

implementation

var root_evaluation_flow: TEvaluationFlow = nil;
    base_stack: TVSymbolStack = nil;
    quote_operator: TVOperator = nil;
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
   // WriteLn('>> ',L.AsString());
    result := MaxInt;
    if tpNIL(L)
    then result := 0
    else
        for i := from to L.High do
            if L.L[i].Count < result then result := L.L[i].Count;
end;

function bind_parameters_list(PL: TVList; sign: TVList): TVList;
var i, opt_start, p: integer;
    mode: TSubprogramParmeterMode;
const offset = 0;  //передаваемый список параметров первым пунктом будет
                    //содержать имя функции и его надо проигнорировать
                    //уже не надо

begin
    //TODO: bind_parameters_list не проверяет переданный список на наличие лишнего
    //print_stdout_ln(PL);
    //print_stdout_ln(sign);
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
                    if (i+offset)<PL.Count
                    then result.Add_phantom(PL.look[i+offset])
                    else raise ELE.Create('insufficient parameters count', 'invalid parameters');
                spmOpt:
                    if (i-1+offset)<PL.Count
                    then result.Add_phantom(PL.look[i-1+offset])
                    else result.Add(TVList.Create);
                spmKey: if key_pos(PL, ':'+sign.uname[i], opt_start, p)
                    then result.Add_phantom(PL.look[p+1])
                    else result.Add(TVList.Create);
                spmRest:
                    result.Add(PL.phantom_subseq(i-1+offset, PL.Count));
                spmFlag: if flag_exists(PL, ':'+sign.uname[i], opt_start)
                    then result.add(TVT.Create)
                    else result.add(TVList.Create);
            end;
end;

type TExpressionType = (
    etInvalid, etProcedure, etInternal, etPredicate, etMacro, etOperator);

function expression_type(expr: TVList): TExpressionType;
var head: TValue;
begin
    Assert(tpListNotEmpty(expr), 'не выражение');
    head := expr.look[0];
    if head.ClassType = TVProcedure then result := etProcedure
    else
        if head.ClassType = TVMacro then result := etMacro
        else
            if head is TVInternalFunction then result := etInternal
            else
                if head is TVPredicate then result := etPredicate
                else
                    if head is TVOperator then result := etOperator
                    else
                        result := etInvalid;
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
//TODO: накопилось очень много функций тестирующих параметры вызовов подпрограмм

//function parse_subprogram_signature(PL: TVList): TSubprogramSignature;
//var
//    mode: TSubprogramParmeterMode;
//    uname: unicodestring;
//    i: integer;
//begin
//    mode := spmNec;
//    SetLength(result, 0);
//    for i:=0 to PL.Count-1 do
//        if tpSymbol(PL.look[i])
//        then
//            if not (PL.name[i][1]='&')
//            then begin
//                SetLength(result, Length(result)+1);
//                result[high(result)].n := PL.uname[i];
//                result[high(result)].m := mode;
//            end
//            else
//                if mode=spmNec
//                then begin
//                    uname := UpperCaseU(PL.name[i]);
//                    if uname='&OPTIONAL' then mode := spmOpt;
//                    if uname='&KEY' then mode := spmKey;
//                    if (uname='&REST') or (uname='&BODY') then mode := spmRest;
//                    if (uname='&CAPTURED') then mode := spmCaptured;
//                end
//                else
//                    if UpperCaseU(PL.name[i])='&CAPTURED'
//                    then mode := spmCaptured
//                    else raise ELE.malformed('parameters list')
//        else raise ELE.malformed('parameters list');
//end;


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

function DirSep(s: unicodestring): unicodestring;
var buf: string;
begin
    buf := s;
    DoDirSeparators(buf);
    result := buf;
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
    for i := 1 to Length(s) do result := stream.stream.write_char(s[i]);
end;

function ifh_quote(const V: TValue): TVList;
begin
    result := TVList.Create([quote_operator.Copy, V.Copy]);
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
    {8} tpNumber,   tpNumber]) of
        1: result := TVInteger.Create(-PL.I[0]);
        2: result := TVFloat.Create(-PL.F[0]);
        3: result := TVInteger.Create(PL.I[0] - PL.I[1]);
        4: result := TVFloat.Create(PL.F[0] - PL.F[1]);
        5,7: result := TVTimeInterval.Create(
            (PL.look[0] as TVTime).fDT - (PL.look[1] as TVTime).fDT);
        6: result := TVDateTime.Create(
            (PL.look[0] as TVTime).fDT - (PL.look[1] as TVTime).fDT);
        8: result := TVComplex.Create(PL.C[0] - PL.C[1]);
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
        vpIntegerAbsOne,    tpNIL,
        vpRealNotZero,      tpNIL,
        vpComplexNotZero,   tpNIL,
        tpInteger,          vpIntegerNotZero,
        tpReal,             vpRealNotZero,
        tpNumber,           vpNumberNotZero]) of
        1: result := TVInteger.Create(1 div PL.I[0]);
        2: result := TVFloat.Create(1 / PL.F[0]);
        3: result := TVComplex.Create(cinv(PL.C[0]));
        4: if ((PL.I[0] mod PL.I[1]) = 0)
            then result := TVInteger.Create(PL.I[0] div PL.I[1])
            else result := TVFloat.Create(PL.I[0] / PL.I[1]);
        5: result := TVFloat.Create(PL.F[0] / PL.F[1]);
        6: result := TVComplex.Create(PL.C[0] / PL.C[1]);
    end;
end;

function if_div_int             (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, vpIntegerNotZero]) of
        1: result := TVInteger.Create(PL.I[0] div PL.I[1]);
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
        tpInteger,  tpInteger,
        tpCompoundIndexed, tpNIL,
        vpIntegerNotNegative, tpNIL]) of
        1: result := TVRange.Create(PL.I[0], PL.I[1]);
        2: result := TVRange.Create(0, (PL.Look[0] as TVCompound).Count);
        3: result := TVRange.Create(0, PL.I[0]);
    end;
end;

function if_symbol              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: begin
            result := read_from_string(PL.S[0]);
            if not tpSymbol(result) then begin
                FreeAndNil(result);
                raise ELE.Create('invalid symbol name', 'syntax');
            end;
        end;
    end;
end;

function if_random              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative,
        tpNIL]) of
        1: result := TVInteger.Create(system.Random(PL.I[0]));
        2: result := TVFloat.Create(system.Random);
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


function if_equal               (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [tpAny, tpAny]) of
        1: if ifh_equal(PL.look[0], PL.look[1])
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
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]>=PL.I[1] , result);
        2: bool_to_TV( PL.F[0]>=PL.F[1] , result);
        3: bool_to_TV( PL.S[0]>=PL.S[1] , result);
    end;
end;

function if_less_or_equal       (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]<=PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<=PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<=PL.S[1] , result);
    end;
end;

function if_not_equal           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpReal,    tpReal,
        tpString,  tpString,
        tpByteVector, tpByteVector]) of
        1: bool_to_TV( PL.I[0]<>PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<>PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<>PL.S[1] , result);
        4: bool_to_TV(not
            (PL.look[0] as TVByteVector).equal_to(PL.look[1] as TVByteVector),
            result);
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
        1: if UpperCaseU(PL.S[0])=UpperCaseU(PL.S[1])
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


function if_test_dyn            (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    ifh_bind(PL.look[0] as TVList, PL.look[1] as TVList);

    result := TVT.Create;
end;

//function if_error               (const PL: TVList; {%H-}call: TCallProc): TValue;
//begin
//    case params_is (PL, result, [
//        tpString, tpList]) of
//        1: raise ELE.Create(ifh_format(PL.L[1]), PL.S[0]);
//    end;
//end;

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
    //print_stdout_ln(pl);
    case params_is(PL, result, [
    {1} tpNIL,
    {2} tpList,
    {3} tpAny]) of
        1: result := TVList.Create;
        2: result := PL.L[0][0];
        3: result := PL[0];
    end;
end;

function if_subseq              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    //print_stdout_ln(pl);
    case params_is(PL, result, [
        tpCompoundIndexed, vpIntegerNotNegative, tpNIL,
        tpCompoundIndexed, vpIntegerNotNegative, vpIntegerNotNegative]) of
        1: result := (PL.look[0] as TVCompoundIndexed).subseq(PL.I[1]);
        2: result := (PL.look[0] as TVCompoundIndexed).subseq(PL.I[1], PL.I[2]);
    end;
end;

function if_sort                (const PL: TVList; call: TCallProc): TValue; //HIGH
type Tvalues = array of TValue;
var list: TValues; expr: TVlist; i: integer;
    function compare(a, b: TValue): integer;
    var tmp: TValue;
    begin
        expr[1] := a.copy;
        expr[2] := b.copy;
        //TODO: тут нужно переделать на использование фантомного списка

        try
            tmp := nil;
            tmp := call(expr);
            //Write(a.AsString(),'  -  ',b.AsString(),' = ', tmp.AsString());

            if tpNIL(tmp) or vpRealNegative(tmp) or vpKeyword_LESS(tmp)
            then result := -1
            else
                if vpRealZero(tmp) or vpKeyword_EQUAL(tmp)
                then result := 0
                else result := 1;

            //WriteLn('    ',result)
        finally
            tmp.Free;
        end;
    end;

    procedure sort(var V: TValues);
    var p: TValue; i : integer;
    less, equal, more: TValues;
    begin
        p := V[random(Length(V))];
       // WriteLn('p>> ',p.AsString);
        for i := 0 to high(V) do case compare(p, v[i]) of
            1: begin SetLength(more, Length({%H-}more)+1); more[high(more)] := v[i]; end;
            0: begin SetLength(equal, Length({%H-}equal)+1); equal[high(equal)] := v[i]; end;
            -1: begin SetLength(less, Length({%H-}less)+1); less[high(less)] := v[i]; end;
        end;
        if Length(less)>1 then sort(less);
        if Length(more)>1 then sort(more);
        for i := 0 to high(more) do V[i] := more[i];
        for i := 0 to high(equal) do V[Length(more)+i] := equal[i];
        for i := 0 to high(less) do V[Length(more)+Length(equal)+i] := less[i];
    end;

begin try
    expr := nil;
    case params_is(PL, result, [
        tpList, tpSubprogram,
        tpList, tpNIL]) of
        1: expr := TVList.Create([PL[1], TVT.Create, TVT.Create]);
        2: expr := TVList.Create([TVSymbol.Create('>'), TVT.Create, TVT.Create]);
    end;
    //WriteLn('sort>> ', expr.AsString);
    SetLength(list, PL.L[0].Count);
    for i := 0 to PL.L[0].high do list[i] := PL.L[0].look[i];

    sort(list);

    result := TVList.Create;
    for i := 0 to high(list) do (result as TVList).Add(list[i].Copy);


finally
    expr.Free;
end;

end;

function if_slots               (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        tpRecord]) of
        1: begin
            result := TVList.Create;
            for i := 0 to (PL.look[0] as TVRecord).count-1 do
                (result as TVList).Add(
                    TVSymbol.Create((PL.look[0] as TVRecord).name_n(i)));
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
var i, j: integer; m: boolean; A: TVList;
begin
    case params_is(PL, result, [tpNIL, tpListOfLists]) of
        1: result := TVList.Create;
        2: begin
            A := PL.L[0];
            result := TVList.Create;
            for i := 0 to A.L[0].high do begin
                for j := 1 to A.high do begin
                    m := ifh_member(A.L[j], A.L[0].look[i]);
                    if not m then break;
                end;
                if m and not ifh_member(result as TVList, A.L[0].look[i])
                then (result as TVList).Add(A.L[0][i]);
            end;
        end;
    end;
end;

function if_difference          (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; A, B: TVList;
begin
    case params_is(PL, result, [tpList, tpList]) of
        1: begin
            A := PL.L[0];
            B := PL.L[1];
            result := TVList.Create;
            for i := 0 to A.high do
                if not ifh_member(B, A.look[i])
                then (result as TVList).add(A[i]);
        end;
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
    //print_stdout_ln(PL);
    p := -1;
    case params_is(PL, result, [
    {1} tpList, tpAny,
    {2} tpString, tpString]) of
        1: for i := 0 to PL.L[0].high do
            if ifh_equal(PL.L[0].look[i], PL.look[1])
            then begin p := i; break; end;
        2: p := PosU(PL.S[1], PL.S[0]) - 1;
    end;
    if p>=0 then result := TVInteger.Create(p) else result := TVList.Create;
end;

function if_length              (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpCompoundIndexed]) of
        1: result := TVInteger.Create((PL.look[0] as TVCompound).Count);
    end;
end;

function if_list                (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpList]) of
        1: result := PL.L[0].Copy;
    end;
end;

function if_concatenate         (const PL: TVList; {%H-}call: TCallProc): TValue;
var sres: unicodestring; i,j: integer;
begin
    case params_is(PL, result, [
        tpListOfLists,
        tpListOfStrings,
        tpListOfByteVectors]) of
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
            result := TVByteVector.Create;
            //TODO: неэффективная конкатенация TVByteVector
            for i := 0 to PL.L[0].high do begin
                for j := 0 to (PL.L[0].look[i] as TVByteVector).High do
                    (result as TVByteVector).Add(
                        (PL.L[0].look[i] as TVByteVector).bytes[j]);
            end;
        end;
    end;
end;

function if_key                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpList, tpSymbol]) of
        1: result := value_by_key(PL.L[0], PL.uname[1]);
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
            source := PL.L[0].Phantom_Copy;
            predicates := PL.L[1];
            expr := TVList.CreatePhantom; expr.Add(nil); expr.Add(nil);
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
            groups.Add(source.Copy);
        finally
            source.Free;
            cnd.Free;
            expr.Free;
        end;
    end;
end;

function if_byte_vector         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        vpListOfByte]) of
        1: begin
            result := TVByteVector.Create;
            (result as TVByteVector).SetCount(PL.L[0].Count);
            (result as TVByteVector).SetCount(0);
            for i:= 0 to PL.L[0].High do
                (result as TVByteVector).Add(PL.L[0].I[i]);
        end;
    end;
end;

function if_bitwise_or          (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a, b: TVByteVector;
begin
    //TODO: побитовые операторы имеют много общего кода нужно разделить
    case params_is(PL, result, [
        tpByteVector, tpByteVector,
        tpInteger,      tpInteger]) of
        1: begin
            a := PL.look[0] as TVByteVector;
            b := PL.look[1] as TVByteVector;
            if a.Count<>b.Count then raise ELE.Create('inequal length', 'invalid parameters');
            result := TVByteVector.Create;
            (result as TVByteVector).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVByteVector).fBytes[i] := a.fBytes[i] or b.fBytes[i];
        end;
        2: result := TVInteger.Create(PL.I[0] or PL.I[1]);
    end;
end;

function if_bitwise_not         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a: TVByteVector;
begin
    case params_is(PL, result, [
        tpByteVector,
        tpInteger]) of
        1: begin
            a := PL.look[0] as TVByteVector;
            result := TVByteVector.Create;
            (result as TVByteVector).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVByteVector).fBytes[i] := not a.fBytes[i];
        end;
        2: result := TVInteger.Create(not PL.I[0]);
    end;
end;

function if_bitwise_and         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a, b: TVByteVector;
begin
    case params_is(PL, result, [
        tpByteVector, tpByteVector,
        tpInteger,      tpInteger]) of
        1: begin
            a := PL.look[0] as TVByteVector;
            b := PL.look[1] as TVByteVector;
            if a.Count<>b.Count then raise ELE.Create('inequal length', 'invalid parameters');
            result := TVByteVector.Create;
            (result as TVByteVector).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVByteVector).fBytes[i] := a.fBytes[i] and b.fBytes[i];
        end;
        2: result := TVInteger.Create(PL.I[0] and PL.I[1]);
    end;
end;

function if_bitwise_xor         (const PL: TVList; {%H-}call: TCallProc): TValue;
var i: integer; a, b: TVByteVector;
begin
    case params_is(PL, result, [
        tpByteVector, tpByteVector,
        tpInteger,      tpInteger]) of
        1: begin
            a := PL.look[0] as TVByteVector;
            b := PL.look[1] as TVByteVector;
            if a.Count<>b.Count then raise ELE.Create('inequal length', 'invalid parameters');
            result := TVByteVector.Create;
            (result as TVByteVector).SetCount(a.Count);
            for i := 0 to a.High do
                (result as TVByteVector).fBytes[i] := a.fBytes[i] xor b.fBytes[i];
        end;
        2: result := TVInteger.Create(PL.I[0] xor PL.I[1]);
    end;
end;

function if_crc32               (const PL: TVList; {%H-}call: TCallProc): TValue;
var b: TVByteVector;
begin
    case params_is(PL, result, [
        tpByteVector]) of
        1: begin
            b := PL.look[0] as TVByteVector;
            result := TVInteger.Create(
                crc32(0,
                    @b.fBytes[0],
                    Length((PL.look[0] as TVByteVector).fBytes)));
        end;

    end;
end;

function if_character           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative]) of
        1: result := TVString.Create(unicodechar(PL.I[0]));
    end;
end;

function if_assertion           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpTrue, tpString,
        tpNIL,  tpString]) of
        1: result := TVT.Create;
        2: raise ELE.Create(PL.S[1], 'assertion');
    end;
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

//function if_execute_file        (const PL: TVList; ep: TCallProc): TValue;
//var prog_file: TVStreamPointer; expr, res: TValue;
//begin
//    case params_is(PL, result, [
//        tpString]) of
//        1: try
//            prog_file := nil;
//            res := nil;
//            prog_file := TVStreamPointer.Create(
//                NewVariable(
//                    TVFileStream.Create(
//                        DirSep(PL.S[0]), fmRead, seBOM)));
//            while true do begin
//                expr := nil;
//                expr := dlisp_read.read(prog_file);
//
//                if ((expr IS TVSymbol) and ((expr as TVSymbol).name='exit'))
//                    or (expr is TVEndOfStream)
//                then begin expr.Free; break; end;
//
//                //writeln('expr>> ', expr.AsString());
//                res := ep(expr);
//                if res is TVReturn then break;
//                FreeAndNil(res);
//            end;
//        finally
//            FreeAndNil(prog_file);
//            FreeAndNil(res);
//            result := TVT.Create;
//        end;
//    end;
//end;

function execute_file(filename: unicodestring): boolean;
var expr: TVList; res: TValue;
begin try
    result := false;
//    expr := TVList.Create([TVString.Create(filename)]);
    expr := TVList.Create([TVSymbol.Create('EXECUTE-FILE'), TVString.Create(filename)]);
    res := nil;
    try
//        res := if_execute_file(expr, root_evaluation_flow.eval);
        res := root_evaluation_flow.op_execute_file(expr);
        result := true;
    except
        on E:ELE do begin
            WriteLn('ERROR during execution ',filename);
            Write(E.EStack);
            WriteLn(E.Message,' (',E.EClass,')');
        end;
    end;
finally
    expr.Free;
    res.Free;
end; end;

function if_run_command         (const PL: TVList; {%H-}call: TCallProc): TValue;
var output: string;
begin
    result := nil;
    case params_is(PL, result, [
        tpString, tpString,
        tpString, tpNIL]) of
        1: {%H-}process.RunCommandInDir(PL.S[1], PL.S[0], output{%H-});
        2: {%H-}process.RunCommand(PL.S[0], output);
    end;
    result := TVString.Create(output);
end;

function if_now                 (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    Assert(PL.Count=0, 'NOW не нуждается в параметрах');
    result := TVDateTime.Create(now);
end;

function if_open_file           (const PL: TVList; {%H-}call: TCallProc): TValue;
var mode: TFileMode; enc: TStreamEncoding;
begin
    case params_is(PL, result, [
        tpString, vpKeywordFileModeOrNIL, vpKeywordEncodingOrNIL]) of
        1: begin
            if tpNIL(PL.look[1]) then mode := fmRead else
            if vpKeyword_READ(PL.look[1]) then mode := fmRead else
            if vpKeyword_WRITE(PL.look[1]) then mode := fmWrite else
            if vpKeyword_APPEND(PL.look[1]) then mode := fmAppend else
                raise ELE.InvalidParameters;

            enc := ifh_keyword_to_encoding(PL.look[2]);

            if fileExists(DirSep(PL.S[0])) or (mode <> fmRead)
            then result := TVStreamPointer.Create(
                NewVariable(TVFileStream.Create(DirSep(PL.S[0]), mode, enc)))
            else raise ELE.Create(PL.S[0], 'file not found');
        end;
    end;
end;

function if_set_encoding        (const PL: TVList; {%H-}call: TCallProc): TValue;
var enc: TStreamEncoding;
begin
    case params_is(PL, result, [
    {1} vpStreamPointerActive, tpKeywordOrNIL]) of
        1: begin
            enc := ifh_keyword_to_encoding(PL.look[1]);
            if enc = seBOM
            then (PL.look[0] as TVStreamPointer).stream.read_BOM
            else (PL.look[0] as TVStreamPointer).stream.encoding := enc;
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

function if_inflate             (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpKeywordOrNIL, tpAny,
        tpStreamPointer, tpKeywordOrNIL, tpAny]) of
        1:  begin
            result := TVStreamPointer.Create(
                NewVariable(
                    TVInflateStream.Create(
                        RefVariable((PL.look[0] as TVStreamPointer).body),
                        ifh_keyword_to_encoding(PL.look[1]),
                        tpTrue(PL.look[2]))));
        end;
        2: raise ELE.Create('inactive stream', 'invalid parameters');
    end;
end;

function if_stream_position     (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpStreamPointer, vpIntegerNotNegative,
        tpStreamPointer, tpNIL]) of
        1: begin
            (PL.look[0] as TVStreamPointer).stream.fstream.Position := PL.I[1];
            result := TVT.Create;
        end;
        2: result := TVInteger.Create(
            (PL.look[0] as TVStreamPointer).stream.fstream.Position);
    end;
end;

function if_stream_length       (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: result := TVInteger.Create(
            (PL.look[0] as TVStreamPointer).stream.fstream.Size);
    end;
end;

function if_read_byte           (const PL: TVList; {%H-}call: TCallProc): TValue;
var b: byte;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: if (PL.look[0] as TVStreamPointer).stream.read_byte(b)
            then result := TVInteger.Create(b)
            else result := TVList.Create;
    end;
end;

function if_read_bytes          (const PL: TVList; {%H-}call: TCallProc): TValue;
var res: TVByteVector;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, vpIntegerNotNegative,
        vpStreamPointerActive, vpKeyword_ALL]) of
        1: try
            res := TVByteVector.Create;
            (PL.look[0] as TVStreamPointer).stream.read_bytes(
                (res as TVByteVector).fBytes, PL.I[1]);

        finally
            result := res;
        end;
        2: try
            res := TVByteVector.Create;
            (PL.look[0] as TVStreamPointer).stream.read_bytes(
                (res as TVByteVector).fBytes, -1);
        finally
            result := res;
        end;
    end;
end;

function if_write_byte          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, vpIntegerByte,
        vpStreamPointerActive, tpByteVector]) of
        1: begin
            (PL.look[0] as TVStreamPointer).stream.write_byte(PL.I[1]);
            result := TVT.Create
        end;
        2: begin
            (PL.look[0] as TVStreamPointer).stream.write_bytes(
                (PL.look[1] as TVByteVector).fBytes);
            result := TVT.Create;
        end;
    end;
end;

function if_read_character      (const PL: TVList; {%H-}call: TCallProc): TValue;
var ch: unicodechar;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,
        tpNIL]) of
        1: if (PL.look[0] as TVStreamPointer).stream.read_char(ch)
            then result := TVString.Create(ch)
            else result := TVList.Create;
        3: begin
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
            while (PL.look[0] as TVStreamPointer).stream.read_char(ch) do
                case ch of
                    #13,#10: if s<>'' then break;
                    else s := s + ch;
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
        1:  (PL.look[0] as TVStreamPointer).stream.read_BOM;
    end;
    result := TVT.Create;
end;

function if_write_bom           (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: (PL.look[0] as TVStreamPointer).stream.write_BOM;
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
            //WriteLn('read>>',result.Asstring);
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
var i: integer;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpAny,
        tpNIL,           tpAny]) of
        1: begin
                dlisp_read.print(
                    PL.look[1],
                    PL.look[0] as TVStreamPointer);
                for i := 1 to Length(new_line) do
                    (PL.look[0] as TVStreamPointer).stream.write_char(new_line[i]);
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
var i: integer;
begin
        case params_is(PL, result, [
            vpStreamPointerActive, tpAny,
            tpNIL,                 tpAny,
            vpKeyword_RESULT,      tpAny]) of
            1: begin
                print(PL.look[1], PL.look[0] as TVStreamPointer);
                for i := 1 to Length(new_line) do
                    (PL.look[0] as TVStreamPointer).stream.write_char(new_line[i]);
                result := TVT.Create;
            end;
            2: begin
                print(PL.look[1], nil);
                System.WriteLn('');
                result := TVT.Create;
            end;
            3: result := TVString.Create(PL[1].AsString);
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
        1: result := TVString.Create(IntToHex(PL.I[0],
                        round(math.logn(16, PL.I[0]))));
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
        1: result := TVString.Create(UpperCaseU(PL.S[0]));
    end;
end;

function if_lower_case          (const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(LowerCaseU(PL.S[0]));
    end;
end;





function if_xml_read_from_string(const PL: TVList; {%H-}call: TCallProc): TValue;
begin
    case params_is(PL, result, [
        tpString, tpNIL,
        tpString, tpTrue]) of
        1: result := xml_read_from_string_l(PL.S[0]);
        2: result := xml_read_from_string_sl(PL.S[0]);
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
            ucommand := UpperCaseU(SQL.Text);
            if (Pos('SELECT', ucommand)=1)
                or (Pos('SHOW', ucommand)=1)
                or (Pos('DESCRIBE', ucommand)=1)
            then Active := true
            else begin
                result := TVT.Create;
                ExecSQL;
                //(PL.look[0] as TVSQLPointer).Commit;
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
                    if VarIsNull(fields[j].Value)
                    then rec[j] := TVList.Create
                    else
                    case Fields[j].DataType of
                        ftUnknown, ftString, ftWideString,
                        ftFmtMemo, ftMemo, ftFixedWideChar,
                        ftWideMemo,
                        ftFixedChar: rec[j] :=
                            TVString.Create(Fields[j].AsString);
                        ftSmallint, ftInteger,
                        ftWord: rec[j] :=
                            TVInteger.Create(Fields[j].AsInteger);
                        ftBoolean: if Fields[j].AsBoolean
                            then rec[j] := TVT.Create
                            else rec[j] := TVList.Create;
                        ftFloat: rec[j] :=
                            TVFloat.Create(Fields[j].AsFloat);
                        ftDateTime, ftDate, ftTimeStamp: rec[j] :=
                            TVDateTime.Create(Fields[j].AsDateTime);
                        ftTime: rec[j] :=
                            TVTimeInterval.Create(Fields[j].AsDateTime);


                        //ftCurrency, ftBCD,
                        //ftBytes, ftVarBytes, ftAutoInc, ftBlob, , ftGraphic, ,
                        //ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
                        //, ftLargeint, ftADT, ftArray, ftReference,
                        //ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
                        //ftIDispatch, ftGuid, ftFMTBcd, );
                    end;
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
        1: with (PL.look[0] as TVSQLPointer).query do try

            Active := false;
            SQL.Text := ifh_format(PL.L[1]);
            ucommand := UpperCaseU(SQL.Text);
            if (Pos('SELECT', ucommand)=1)
                or (Pos('SHOW', ucommand)=1)
            then Active := true
            else begin
                raise ELE.Create('invalid query for list', 'sql');
            end;
            Last;

            if FieldCount<>1 then raise ELE.Create('запрос вернул не одну колонку', 'sql');

            result := TVList.Create;
            First;
            for i := 0 to RecordCount-1 do begin
                if VarIsNull(fields[0].Value)
                then (result as TVList).Add(TVList.Create)
                else
                case Fields[0].DataType of
                    ftUnknown, ftString, ftWideString, ftFmtMemo, ftMemo,
                    ftFixedWideChar, ftWideMemo,ftFixedChar:
                        (result as TVList).Add(
                            TVString.Create(Fields[0].AsString));
                    ftSmallint, ftInteger,ftWord:
                        (result as TVList).Add(
                            TVInteger.Create(Fields[0].AsInteger));
                    ftBoolean: if Fields[0].AsBoolean
                            then (result as TVList).Add(TVT.Create)
                            else (result as TVList).Add(TVList.Create);
                    ftFloat:
                        (result as TVList).Add(
                            TVFloat.Create(Fields[0].AsFloat));
                    ftDateTime, ftDate, ftTimeStamp:
                        (result as TVList).Add(
                            TVDateTime.Create(Fields[0].AsDateTime));
                    ftTime:
                        (result as TVList).Add(
                            TVTimeInterval.Create(Fields[0].AsDateTime));

                        //ftCurrency, ftBCD,
                        //ftBytes, ftVarBytes, ftAutoInc, ftBlob, , ftGraphic, ,
                        //ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
                        //, ftLargeint, ftADT, ftArray, ftReference,
                        //ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
                        //ftIDispatch, ftGuid, ftFMTBcd, );
                end;
                Next;
            end;
        finally

        end;
    end;
end;

const int_fun_count = 97;
var int_fun_sign: array[1..int_fun_count] of TVList;
const int_fun: array[1..int_fun_count] of TInternalFunctionRec = (
(n:'RECORD?';               f:if_structure_p;           s:'(s :optional type)'),

(n:'+';                     f:if_add;                   s:'(:rest n)'),
(n:'-';                     f:if_sub;                   s:'(a :optional b)'),
(n:'*';                     f:if_mul;                   s:'(:rest n)'),
(n:'/';                     f:if_div;                   s:'(a :optional b)'),
(n:'DIV';                   f:if_div_int;               s:'(a b)'),
(n:'MOD';                   f:if_mod;                   s:'(a b)'),
(n:'ABS';                   f:if_abs;                   s:'(a)'),
(n:'**';                    f:if_power;                 s:'(a b)'),
(n:'SQRT';                  f:if_sqrt;                  s:'(a)'),
(n:'ROUND';                 f:if_round;                 s:'(a)'),
(n:'RANGE';                 f:if_range;                 s:'(l :optional h)'),
(n:'SYMBOL';                f:if_symbol;                s:'(n)'),
(n:'RANDOM';                f:if_random;                s:'(:optional r)'),
(n:'RE';                    f:if_re;                    s:'(a)'),
(n:'IM';                    f:if_im;                    s:'(a)'),

(n:'=';                     f:if_equal;                 s:'(a b)'),
(n:'>';                     f:if_more;                  s:'(a b)'),
(n:'<';                     f:if_less;                  s:'(a b)'),
(n:'>=';                    f:if_more_or_equal;         s:'(a b)'),
(n:'<=';                    f:if_less_or_equal;         s:'(a b)'),
(n:'<>';                    f:if_not_equal;             s:'(a b)'),
(n:'NOT';                   f:if_not;                   s:'(a)'),
(n:'EQUAL-CASE-INSENSITIVE';f:if_equal_case_insensitive;s:'(s s)'),
(n:'EQUAL-SETS';            f:if_equal_sets;            s:'(a b)'),

(n:'TEST-DYN';              f:if_test_dyn;              s:'(a b)'),
//(n:'ERROR';                 f:if_error;                 s:'(c :rest m)'),


(n:'EXTRACT-FILE-EXT';      f:if_extract_file_ext;      s:'(s)'),
(n:'EXTRACT-FILE-NAME';     f:if_extract_file_name;     s:'(s)'),
(n:'EXTRACT-FILE-PATH';     f:if_extract_file_path;     s:'(s)'),
(n:'FILE-EXISTS';           f:if_file_exists;           s:'(n)'),
(n:'DIRECTORY-EXISTS';      f:if_directory_exists;      s:'(d)'),
(n:'COMMAND-LINE';          f:if_command_line;          s:'()'),
(n:'CHANGE-DIRECTORY';      f:if_change_directory;      s:'(d)'),
(n:'DELETE-FILE';           f:if_delete_file;           s:'(f)'),
(n:'RENAME-FILE';           f:if_rename_file;           s:'(o n)'),
(n:'CREATE-DIRECTORY';      f:if_create_directory;      s:'(d)'),
(n:'REMOVE-DIRECTORY';      f:if_remove_directory;      s:'(d)'),
(n:'GUID';                  f:if_guid;                  s:'()'),

(n:'EVERY';                 f:if_every;                 s:'(p :rest l)'),
(n:'SOME';                  f:if_some;                  s:'(p :rest l)'),
(n:'CAR';                   f:if_car;                   s:'(l)'),
(n:'SUBSEQ';                f:if_subseq;                s:'(s b :optional e)'),
(n:'SORT';                  f:if_sort;                  s:'(s :optional p)'),
(n:'SLOTS';                 f:if_slots;                 s:'(r)'),

(n:'UNION';                 f:if_union;                 s:'(:rest a)'),
(n:'INTERSECTION';          f:if_intersection;          s:'(:rest a)'),
(n:'DIFFERENCE';            f:if_difference;            s:'(a b)'),
(n:'REVERSE';               f:if_reverse;               s:'(a)'),
(n:'MEMBER';                f:if_member;                s:'(l e)'),
(n:'POSITION';              f:if_position;              s:'(l e)'),
(n:'LENGTH';                f:if_length;                s:'(l)'),
(n:'LIST';                  f:if_list;                  s:'(:rest e)'),
(n:'CONCATENATE';           f:if_concatenate;           s:'(:rest a)'),
(n:'KEY';                   f:if_key;                   s:'(l k)'),
(n:'GROUP';                 f:if_group;                 s:'(s :rest p)'),

(n:'BYTE-VECTOR';           f:if_byte_vector;           s:'(:rest b)'),
(n:'BITWISE-AND';           f:if_bitwise_and;           s:'(a b)'),
(n:'BITWISE-NOT';           f:if_bitwise_not;           s:'(a)'),
(n:'BITWISE-OR';            f:if_bitwise_or;            s:'(a b)'),
(n:'BITWISE-XOR';           f:if_bitwise_xor;           s:'(a b)'),
(n:'CRC32';                 f:if_crc32;                 s:'(b)'),
(n:'CHARACTER';             f:if_character;             s:'(n)'),

(n:'ASSERTION';             f:if_assertion;             s:'(c m)'),
(n:'DIRECTORY';             f:if_directory;             s:'(d)'),
(n:'SLEEP';                 f:if_sleep;                 s:'(m)'),
//(n:'EXECUTE-FILE';          f:if_execute_file;          s:'(n)'),
(n:'RUN-COMMAND';           f:if_run_command;           s:'(c :optional d)'),
(n:'NOW';                   f:if_now;                   s:'()'),

(n:'OPEN-FILE';             f:if_open_file;             s:'(n :key mode encoding)'),
(n:'CLOSE-FILE';            f:if_close_stream;          s:'(s)'),
(n:'INFLATE';               f:if_inflate;               s:'(s :key encoding header)'),
(n:'SET-ENCODING';          f:if_set_encoding;          s:'(s e)'),
//(n:'SET-COMPRESSION-METHOD';f:if_set_compression_method;s:'(s c)'),
(n:'STREAM-POSITION';       f:if_stream_position;       s:'(s :optional p)'),
(n:'STREAM-LENGTH';         f:if_stream_length;         s:'(s)'),
(n:'READ-BYTE';             f:if_read_byte;             s:'(s)'),
(n:'READ-BYTES';            f:if_read_bytes;            s:'(s c)'),
(n:'WRITE-BYTE';            f:if_write_byte;            s:'(s i)'),
(n:'READ-CHARACTER';        f:if_read_character;        s:'(:optional s)'),
(n:'WRITE-STRING';          f:if_write_string;          s:'(s s)'),
(n:'READ-LINE';             f:if_read_line;             s:'(:optional s)'),
(n:'WRITE-LINE';            f:if_write_line;            s:'(s l)'),
(n:'READ-BOM';              f:if_read_bom;              s:'(s)'),
(n:'WRITE-BOM';             f:if_write_bom;             s:'(s)'),

(n:'READ';                  f:if_read;                  s:'(:optional s)'),
(n:'WRITE';                 f:if_write;                 s:'(s a)'),
(n:'PRINT';                 f:if_print;                 s:'(s a)'),

(n:'FMT';                   f:if_fmt;                   s:'(stream :rest s)'),
(n:'LOG';                   f:if_log;                   s:'(:rest s)'),
(n:'HEX';                   f:if_hex;                   s:'(i :optional d)'),
(n:'FIXED';                 f:if_fixed;                 s:'(f :optional d)'),
(n:'COL';                   f:if_col;                   s:'(w v :optional a)'),
(n:'LST';                   f:if_fmt_list;              s:'(l :optional s b e)'),
(n:'UPPER-CASE';            f:if_upper_case;            s:'(s)'),
(n:'LOWER-CASE';            f:if_lower_case;            s:'(s)'),

(n:'XML:READ-FROM-STRING';  f:if_xml_read_from_string;  s:'(s :flag separate)'),

(n:'SQL:MYSQL-CONNECTION';  f:if_sql_mysql_connection;  s:'(database :key host port username password)'),
(n:'SQL:QUERY';             f:if_sql_query;             s:'(db :rest q)'),
(n:'SQL:QUERY-LIST';        f:if_sql_query_list;        s:'(db :rest q)')
);


const predicates: array[1..14] of record n: unicodestring; f: TTypePredicate; end = (
(n:'T?';                    f:tpT),
(n:'NIL?';                  f:tpNIL),
(n:'TRUE?';                 f:tpTRUE),
//(n:'NUMBER?';               f:tpNumber),
(n:'REAL?';                 f:tpReal),
(n:'INTEGER?';              f:tpInteger),
(n:'FLOAT?';                f:tpFloat),
(n:'ATOM?';                 f:tpAtom),
(n:'SUBPROGRAM?';           f:tpSubprogram),
(n:'LIST?';                 f:tpList),
(n:'SYMBOL?';               f:tpSymbol),
(n:'KEYWORD?';              f:tpKeyword),
(n:'STRING?';               f:tpString),
(n:'POSITIVE?';             f:vpRealPositive),
(n:'NEGATIVE?';             f:vpRealNegative)

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
            oeAND       : op('AND');
            oeAPPEND    : op('APPEND');
            oeAPPLY     : op('APPLY');
            oeBLOCK     : op('BLOCK');
            oeBREAK     : op('BREAK');
            oeCASE      : op('CASE');
            oeCOND      : op('COND');
            oeCONST     : op('CONST');
            oeCONTINUE  : op('CONTINUE');
            oeDEBUG     : op('DEBUG');
            oeDEFAULT   : op('DEFAULT');
            oeELT       : op('ELT');
            oeERROR     : op('ERROR');
            //oeELSE      : op('ELSE');
            //oeEXCEPTION : op('EXCEPTION');
            oeEXECUTE_FILE: op('EXECUTE-FILE');
            oeFILTER    : op('FILTER');
            oeFOR       : op('FOR');
            oeGOTO      : op('GOTO');
            oeIF        : op('IF');
            oeIF_NIL    : op('IF-NIL');
            oeLAST      : op('LAST');
            oeLET       : op('LET');
            oeMACRO     : op('MACRO');
            oeMACRO_SYMBOL: op('MACRO-SYMBOL');
            oeMAP       : op('MAP');
            oeOR        : op('OR');
            oePACKAGE   : op('PACKAGE');
            oePOP       : op('POP');
            oePROCEDURE : op('PROCEDURE');
            oePUSH      : op('PUSH');
            oeQUOTE     : op('QUOTE');
            oeRECORD    : op('RECORD');
            oeRECORD_AS : op('RECORD-AS');
            oeRETURN    : op('RETURN');
            oeSET       : op('SET');
            //oeTHEN      : op('THEN');
            oeVAL       : op('VAL');
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
var i: integer;
begin
    base_stack := TVSymbolStack.Create(nil);
    for i := low(int_fun) to high(int_fun) do
        base_stack.new_var(
            int_fun[i].n,
            TVInternalFunction.Create(
                    int_fun_sign[i],
                    int_fun[i].f,
                    int_fun[i].n),
            true);

    //загрузка предикатов
    for i := low(predicates) to high(predicates) do
        base_stack.new_var(
            predicates[i].n,
            TVPredicate.Create(predicates[i].n, predicates[i].f),
            true);

    //загрузка констант
    base_stack.new_var('NL', TVString.Create(new_line), true);
    base_stack.new_var('CR', TVString.Create(#10), true);
    base_stack.new_var('LF', TVString.Create(#13), true);
    base_stack.new_var('TAB', TVString.Create(#09), true);
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
        ec := UpperCaseU(exception_class);
        if vpListHeaded_EXCEPTION(PL.look[pc]) then
             if (PL.L[pc].Count>=2) and tpString(PL.L[pc].look[1])
             then begin
                eh := UpperCaseU(PL.L[pc].S[1]);
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
                    if tpKeyword(PL.look[i]) and (PL.uname[i]=(V as TVGoto).uname)
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
    st: TVSymbolStack);
var bind: TBindings; i: integer; _: integer;
begin
    if st=nil then st := stack;
    bind := ifh_bind(s, p);
    _ := TVSymbol.symbol_n('_');
    for i := 0 to high(bind) do begin
        if not (_ = bind[i].nN)
        then st.new_var(bind[i].nN, bind[i].V, constant)//bind[i].c)
        else bind[i].V.Free;
    end;
end;


function TEvaluationFlow.oph_bind_to_list(s, P: TVList): TVList;
var bind: TBindings; i: integer;
    params: TVList;
begin
    params := P.Phantom_CDR;
    //WriteLn('params>> ', params.asString);
    result := TVList.Create;
    bind := ifh_bind(s, params);
    for i := 0 to high(bind) do result.Add(bind[i].V);
    params.Free;
end;

procedure TEvaluationFlow.oph_execute_file(fn: unicodestring);
var prog_file: TVStreamPointer; expr, res: TValue;
begin
     try
            prog_file := nil;
            res := nil;
            prog_file := TVStreamPointer.Create(
                NewVariable(
                    TVFileStream.Create(DirSep(fn), fmRead, seBOM)));
            while true do begin
                expr := nil;
                expr := dlisp_read.read(prog_file);

                if ((expr IS TVSymbol) and ((expr as TVSymbol).name='exit'))
                    or (expr is TVEndOfStream)
                then begin expr.Free; break; end;

                //writeln('expr>> ', expr.AsString());
                res := eval(expr);
                if res is TVReturn then break;
                FreeAndNil(res);
            end;
        finally
            FreeAndNil(prog_file);
            FreeAndNil(res);
        end;
end;

function TEvaluationFlow.opl_elt(PL: TVList): TVChainPointer;
var i: integer; tmp: TValue; ind: TValue;
begin
    //  Эта процедура должна вернуть указатель на компонент составного типа
    //для дальнейшего извлечения или перезаписи значения.

    result := nil;
    tmp := nil;
    ind := nil;

    if PL.Count<2 then raise ELE.Create('недостаточно параметров');

    result := eval_link(PL.look[1]);
    //TODO: result не освобождается при возникновении исключения
try

    for i := 2 to PL.high do begin
        tmp := result.look;
        ind := eval(PL[i]);
        if tpCompoundIndexed(tmp) and vpIntegerNotNegative(ind)
        then result.add_index((ind as TVInteger).fI)
        else
            if tpCompoundIndexed(tmp) and vpSymbol_LAST(ind)
            then result.add_index((tmp as TVCompound).Count-1)
            else
                if tpRecord(tmp) and tpSymbol(ind)
                then result.add_index((tmp as TVRecord).get_n_of((ind as TVSymbol).uname))
                else
                    if tpNIL(ind)
                    then begin
                        result.Free;
                        result := TVChainPointer.Create(
                            NewVariable(TVList.Create, true));
                        exit;
                    end
                    else
                        raise ELE.InvalidParameters;
        FreeAndNil(ind);
    end;
finally
    ind.Free;
end;
end;

function TEvaluationFlow.opl_last(PL: TVList): TVChainPointer;
begin
    result := nil;
    result := eval_link(PL.look[1]);

    if not tpCompoundIndexed(result.look)
    then raise ELE.Create(result.look.AsString + ' is not indexed compound');

    result.add_index((result.look as TVCompoundIndexed).Count-1);
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
        //TODO: константы не защищены от изменения элементов
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
               // expand_ins(P as TVList);
                case (head as TVOperator).op_enum of
                    oeELT: result := opl_elt(P as TVList);
                    oeLAST: result := opl_last(P as TVList);
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
        if ifh_equal(expr, PL.L[i].look[0])
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

function TEvaluationFlow.op_push                    (PL: TVList): TValue;
var i: integer; CP: TVChainPointer;
begin
    CP := eval_link(PL.look[1]);
try
    if CP.constant then raise ELE.Create('target is not variable');
    if not tpList(CP.look) then raise ELE.Create('target is not list');
    for i := 2 to PL.High do (CP.look as TVList).Add(eval(PL[i]));
    result := TVT.Create;
finally
    CP.Free;
end;
end;

function TEvaluationFlow.op_secondary_error(PL: TVList): TValue;
begin
   // case (PL.look[0] as TVOperator).op_enum of
        //oeTHEN : raise ELE.Create('THEN not inside IF', 'syntax');
        //oeELSE : raise ELE.Create('ELSE not inside IF', 'syntax');
        //oeEXCEPTION : raise ELE.Create('EXCEPTION not inside block', 'syntax');
   //     else raise ELE.Create('secondary op error');
  //  end;
    result := TVT.Create;
end;

function TEvaluationFlow.op_pop                     (PL: TVList): TValue;
var CP: TVChainPointer;
begin
    CP := nil;
    CP := eval_link(PL.look[1]);
try
    if CP.constant then raise ELE.Create('target is not variable');
    if not tpList(CP.look) then raise ELE.Create('target is not list');
    result := (cp.look as TVList).POP;
finally
    CP.Free;
end;
end;

function TEvaluationFlow.op_const                   (PL: TVList): TValue;
var tmp: TValue;
begin
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

function TEvaluationFlow.op_debug(PL: TVList): TValue;
begin
    result := TVT.Create;

    if (PL.Count=2) and vpKeyword_RESET_STACK(PL.look[1]) then begin
        main_stack.Free;
        main_stack := base_stack.Copy as TVSymbolStack;
        stack := main_stack;
        exit;
    end;

    if (PL.Count>=2) and vpKeyword_PRINT_STACK(PL.look[1]) then begin
        if (PL.Count=3) and vpIntegerNotNegative(PL.look[2])
        then stack.Print(PL.I[2])
        else stack.Print;
        exit;
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
        then //stack.new_var(PL.uname[1], eval(PL[2]), true)
            stack.new_var(PL.SYM[1], eval(PL[2]), true)
        else CP.set_target(eval(PL[2]));

    result := TVT.Create;
finally
    CP.Free;
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

function TEvaluationFlow.op_filter                  (PL: TVList): TValue;
var i: integer; res: TVList; PLI: TVList; tmp: TVList;
begin
    //TODO: FILTER должен быть переделан во внутреннюю функцию
    if PL.Count<>3 then raise ELE.Malformed('FILTER');
try
    PLI := nil;
    PLI := TVList.Create([eval(PL[1]), eval(PL[2])]);

    case params_is(PLI, result, [
        tpSubprogram, tpList]) of
        1: begin
            res := TVList.Create();
            for i := 0 to PLI.L[1].high do begin
                tmp := TVList.Create([PLI[0], PLI.L[1][i]]);
                result := call(tmp);
                tmp.Free;
                if not tpNIL(result)
                then res.Add(PLI.L[1][i]);
                FreeAndNil(result);
            end;
        end;
    end;
    result := res;
finally
    PLI.Free;
end;
end;

function TEvaluationFlow.op_val                     (PL: TVList): TValue;
begin
    if PL.Count<>2 then raise ELE.malformed('VAL');

    result := eval(PL[1]);
end;

function TEvaluationFlow.op_set                     (PL: TVList): TValue;
var CP :TVChainPointer;
begin try
    //TODO: set не падает если устанавливает параметр функции переданный по значению
    if (PL.Count<3) or (PL.Count>3) then raise ELE.InvalidParameters;

    CP := nil;
    CP := eval_link(PL.look[1]);
    if CP.constant then raise ELE.Create('target is not variable');
    CP.set_target(eval(PL[2]));
    result := TVT.Create;
finally
    FreeAndNil(CP);
end end;


function TEvaluationFlow.op_record               (PL: TVList): TValue;
var i: integer; tmp: TVRecord;
begin
    if (PL.Count mod 2)<>1 then raise ELE.Malformed('RECORD');
try
    tmp := TVRecord.Create;
    for i := 0 to (PL.Count div 2)-1 do begin
        if tpSymbol(PL.look[2*i+1])
        then tmp.AddSlot(PL.uname[2*i+1], eval(PL[2*i+2]))
        else raise ELE.Malformed('record: '+PL.look[2*i+1].AsString+' is not symbol');
    end;
    result := tmp;
except
    tmp.Free;
    raise;
end;
end;

function TEvaluationFlow.op_record_as            (PL: TVList): TValue;
var i: integer; rec: TVRecord; tmp: TValue;
begin
    //TODO: очень запутанный алгоритм проверки параметров
    if PL.Count<2 then raise ELE.malformed('RECORD-AS');
    tmp := nil;
    rec := nil;
    result := nil;
try
    tmp := eval(PL[1]);

    if not tpRecord(tmp)
    then raise ELE.Create(tmp.AsString+' не структура', 'invalid parameters');
    if (PL.Count mod 2)<>0
    then raise ELE.Malformed('RECORD-AS: не чётное число параметров');

    rec := tmp as TVRecord;
    for i := 1 to (PL.Count div 2)-1 do begin
        if tpSymbol(PL.look[2*i])
        then rec.slot[PL.uname[2*i]] := eval(PL[2*i+1])
        else raise ELE.Create(PL.look[2*i].AsString+' is not symbol');
    end;
    result := rec;
except
    tmp.Free;
    raise;
end;
end;

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

function TEvaluationFlow.op_for                     (PL: TVList): TValue;
var i, frame_start, high_i, low_i: integer;
    V: TValue;
    CP: TVChainPointer;
    index: TVInteger;
    op_var : (sequence, range, times);
begin
    //оператор цикла возвращает NIL в случае завершения и последнее значение
    //итерируемой переменной в случае досрочного прерывания оператором (BREAK)
    //это упрощает написание функций поиска

    if (PL.count<3) or not tpOrdinarySymbol(PL.look[1])
    then raise ELE.malformed('FOR');

    CP := eval_link(PL.look[2]);

    if tpCompoundIndexed(CP.look) then op_var := sequence
    else
        if tpRange(CP.look) then op_var := range
        else
            if vpIntegerNotNegative(CP.look) then op_var := times
            else
                raise ELE.InvalidParameters;

try
    V := nil;
    result := nil;
    frame_start := stack.Count;
    //stack.new_var(' <for-list2>', TVT.Create);

    case op_var of
        sequence: begin
            high_i := (CP.look as TVCompoundIndexed).high;
            CP.add_index(0);
            //stack.new_var(PL.uname[1], CP, true);
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

function TEvaluationFlow.op_last                    (PL: TVList): TValue;
var CP: TVChainPointer;
begin
    if PL.Count<>2 then raise ELE.malformed('LAST');

    CP := opl_last(PL);
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
                //stack.set_var(VPL.L[i].uname[0], eval(VPL.L[i][1]));
                stack.set_var(VPL.L[i].SYM[0], eval(VPL.L[i][1]));
                Inc(count);
            end;

            result := oph_block(PL,2, false);
        finally
            for j := 0 to count-1 do //stack.set_var(VPL.L[j].uname[0], old_v[j]);
                stack.set_var(VPL.L[j].SYM[0], old_v[j]);
        end;
    finally
        FreeAndNil(old_v);
    end;
end;

function TEvaluationFlow.op_map                     (PL: TVList): TValue;
var expr: TVList; i,j: integer;
    min_count: integer; PLI: TVList;
begin
    if PL.Count<3 then raise ELE.Malformed('MAP');
    result := nil;
    expr := nil;
    PLI := TVList.Create;
    PLI.SetCapacity(PL.Count-1);
try
    for i := 1 to PL.High do PLI.Add(eval(PL[i]));

    if not tpSubprogram(PLI.look[0]) then raise ELE.InvalidParameters;
    for i := 1 to PLI.high do
        if not tpList(PLI.look[i]) then raise ELE.InvalidParameters;

    min_count := min_list_length(PLI, 1);
try
    expr := PLI.Phantom_Copy;
    result := TVList.Create;
    for i := 0 to min_count - 1 do begin
        for j := 1 to PLI.High do expr[j] := PLI.L[j].look[i];
        (result as TVList).Add(call(expr));
    end;
except
    //удалить результаты предыдущих итераций если текущая завершилась
    // с ошибкой
    FreeAndNil(result);
    raise;
end;
finally
    PLI.Free;
    expr.Free;
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
    //proc.is_macro_symbol := true;

    proc.nN := PL.SYM[1].N;

    result := proc;
    proc.stack_pointer := -1;// := stack.count;
    proc.home_stack := nil;
    proc.body := PL.Subseq(2, PL.Count) as TVList;
//    proc.fsignature := nil;
    //proc.evaluated:=true;
    proc.sign := TVList.Create;

    proc.stack := TVSymbolStack.Create(nil);
    try
        sl := extract_body_symbols(proc.body);
        fill_subprogram_stack(proc, sl);
    finally
        FreeAndNil(sl);
    end;

    proc.stack.remove_unbound;
    proc.evaluated := true;

    //stack.new_var(PL.uname[1],result.Copy, true);
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
    P: PVariable;
    i: integer;
    pack: TPackage;
begin
    if (PL.Count<4)
        or (not tpOrdinarySymbol(PL.look[1]))
        or (not tpListOfOrdinarySymbols(PL.look[2]))
    then raise ELE.Malformed('PACKAGE');

    package_stack := base_stack.Copy as TVSymbolStack;
    external_stack := stack;
    stack := package_stack;
    result := nil;
try
    result := oph_block(PL, 3, false);

    for i := 0 to PL.L[2].high do begin
        //P := package_stack.find_ref_or_nil(PL.L[2].uname[i]);
        P := package_stack.find_ref_or_nil(PL.L[2].SYM[i]);
        if P=nil then raise ELE.Create(PL.L[2].uname[i]+
            ' not bound in package '+PL.uname[1], 'symbol not bound');
        external_stack.new_ref(PL.uname[1]+':'+PL.L[2].uname[i], P);
    end;

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
var CP: TVChainPointer; i: integer;
begin try

    //TODO: слишком много копирований
    //TODO: нет предварительной проверки корректности параметров
    // в случае несоответствия параметров программа упадёт при
    // попытке приведения типов
    if PL.Count<3 then raise ELE.InvalidParameters;
    for i := 2 to PL.high do PL[i] := eval(PL[i]);

    CP := eval_link(PL.look[1]);
    if CP.constant then raise ELE.Create('target is not variable');
    if CP.look is TVString
    then
        for i := 2 to PL.high do
            (CP.look as TVString).S := (CP.look as TVString).S + PL.S[i]
    else
        if CP.look is TVList
        then
            for i := 2 to PL.high do
                (CP.look as TVList).Append(PL[i] as TVList)
        else
            if CP.look is TVByteVector
            then
                for i := 2 to PL.high do
                    (CP.look as TVByteVector).append(PL[i] as TVByteVector)
            else raise ELE.InvalidParameters;


    result := TVT.Create;
finally
    FreeAndNil(CP);
end end;

function TEvaluationFlow.op_apply(PL: TVList): TValue;
var expr: TVList; i: integer; tmp: TValue; list: TVList;
begin
    result := nil;
    if PL.Count<2 then raise ELE.Malformed('APPLY');
try
    tmp := nil;
    expr := TVList.Create;
    expr.Add(eval(PL[1]));
    for i := 2 to PL.high-1 do expr.Add(eval(PL[i]));
    if (PL.Count>=3) then begin
        tmp := eval(PL[PL.high]);
        if tpList(tmp) then begin
            list := tmp as TVList;
            expr.Append(list);
            //for i := 0 to list.high do expr.Add(ifh_quote(list.look[i]));
            result := call(expr);
        end
        else raise ELE.InvalidParameters;

    end;
finally
    expr.Free;
    //tmp.Free;
end;
end;

function TEvaluationFlow.op_procedure               (PL: TVList): TValue;
var proc: TVProcedure; sl: TVList;
    sign_pos: integer;
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
    //proc.is_macro := (PL.look[0] as TVOperator).op_enum=oeMACRO;

    if sign_pos=2 then proc.nN := PL.SYM[1].N;

    result := proc;
    proc.stack_pointer := stack.count;
    proc.home_stack := stack;
    proc.body := PL.Subseq(sign_pos+1, PL.Count) as TVList;
//    proc.fsignature := parse_subprogram_signature(PL.look[sign_pos] as TVList);
    proc.evaluated:=false;

    proc.sign := PL[sign_pos] as TVList;

    proc.stack := TVSymbolStack.Create(nil);
    try
        sl := extract_body_symbols(proc.body);
        fill_subprogram_stack(proc, sl);
    finally
        FreeAndNil(sl);
    end;

    if sign_pos=2 then //stack.new_var(PL.uname[1],result.Copy, true);
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
        3: //stack.new_var(PL.SYM[1], eval(PL[2]));
            try
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
    //WriteLn('when cond>> ', result.asstring);
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

function TEvaluationFlow.op_with(PL: TVList): TValue;
var i, j: integer;
    pack: TPackage;
    fn: unicodestring;
begin
    if (PL.Count<2) then raise ELE.Malformed('WITH');
    for i := 1 to PL.high do
        if not (tpOrdinarySymbol(PL.look[i]) or tpString(PL.look[i]))
        then raise ELE.InvalidParameters;

    result := nil;

    for i := 0 to PL.high do
    begin
        if tpSymbol(PL.look[i])
        then begin
            pack := FindPackage(PL.uname[i]);
            if pack<>nil
            then begin
                for j := 0 to pack.export_list.high do
                    stack.new_ref(pack.uname+':'+pack.export_list.uname[j],
                        //pack.stack.find_ref(pack.export_list.uname[j])
                        pack.stack.find_ref(pack.export_list.SYM[j])
                        );
            end
            else begin
                fn := PL.name[i]+'.lisya';
                if FileExists(fn)
                then begin
                    oph_execute_file(fn);
                end
                else begin
                    raise ELE.Create('package '+PL.name[i]+' not found');
                end;
            end;
        end;
        if tpString(PL.look[i])
        then begin
            fn := DirSep(PL.S[i]);
            if FileExists(fn)
            then begin
                oph_execute_file(fn);
            end
            else begin
                raise ELE.Create('package "'+fn+'" not found');
            end;
        end;
    end;

    result.Free;
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
        //здесь if_union используется для удаления дубликатов
        tmp := TVList.Create([TVList.Create([result])]);
        //TODO: переделать if_union в ifh_union, поскольку функция используется для собственных нужд
        result := if_union(tmp, call) as TVList;
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

    for i := 0 to symbols.High do begin
        sp.stack.new_ref(
            symbols.SYM[i],
            stack.find_ref_or_nil(symbols.SYM[i]));
    end;
end;

function TEvaluationFlow.call(PL: TVList): TValue;
var head: TValue;
begin
   // WriteLn('CALL>> ', PL.AsString);
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
   //WriteLn('= ', result.AsString);
end;

function TEvaluationFlow.call_procedure(PL: TVList): TValue;
var proc: TVProcedure; params: TVList; tmp_stack, proc_stack: TVSymbolStack;
    tmp: TValue;
begin try
    //WriteLn('call\-proc');
    params := nil;
    proc_stack := nil;
    proc := PL.look[0] as TVProcedure;
    proc.Complement;
    proc_stack := proc.stack.Copy as TVSymbolStack;
    params := PL.Phantom_CDR;
    oph_bind(proc.sign, params, true, proc_stack);

    tmp_stack := stack;
    stack := proc_stack;
    result := oph_block(proc.body,0,false);
    if tpReturn(result) then begin
            tmp := (result as TVReturn).value.Copy;
            result.Free;
            result := tmp;
        end;
//    if PL.look[0] is
finally
    stack := tmp_stack;
    params.Free;
    proc_stack.Free;
end;
    //TODO: при очистке стэка, рекурсивные процедуры не освобождаются
end;

function TEvaluationFlow.call_macro(PL: TVList): TValue;
begin

end;

function TEvaluationFlow.call_internal(PL: TVList): TValue;
var binded_PL, params: TVList;
begin try
    binded_PL := nil;
    params := PL.Phantom_CDR;
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
        oeAND       : result := op_and(PL);
        oeAPPEND    : result := op_append(PL);
        oeAPPLY     : result := op_apply(PL);
        oeBLOCK     : result := op_block(PL);
        oeBREAK     : result := TVBreak.Create;
        oeCASE      : result := op_case(PL);
        oeCOND      : result := op_cond(PL);
        oeCONST     : result := op_const(PL);
        oeCONTINUE  : result := TVContinue.Create;
        oeDEBUG     : result := op_debug(PL);
        oeDEFAULT   : result := op_default(PL);
        oeELT       : result := op_elt(PL);
        oeERROR     : result := op_error(PL);
        //oeELSE      : result := op_secondary_error(PL);
        //oeEXCEPTION : result := op_secondary_error(PL);
        oeEXECUTE_FILE: result := op_execute_file(PL);
        oeFILTER    : result := op_filter(PL);
        oeFOR       : result := op_for(PL);
        oeGOTO      : result := op_goto(PL);
        oeIF        : result := op_if(PL);
        oeIF_NIL    : result := op_if_nil(PL);
        oeLAST      : result := op_last(PL);
        oeLET       : result := op_let(PL);
        oeMACRO     : result := op_procedure(PL);
        oeMACRO_SYMBOL: result := op_macro_symbol(PL);
        oeMAP       : result := op_map(PL);
        oeOR        : result := op_OR(PL);
        oePACKAGE   : result := op_package(PL);
        oePOP       : result := op_pop(PL);
        oePROCEDURE : result := op_procedure(PL);
        oePUSH      : result := op_push(PL);
        oeQUOTE     : result := PL[1];
        //TODO: QUOTE не проверяет количество аргументов
        oeRECORD    : result := op_record(PL);
        oeRECORD_AS : result := op_record_as(PL);
        oeRETURN    : result := op_return(PL);
        oeSET       : result := op_set(PL);
        //oeTHEN      : result := op_secondary_error(PL);
        oeVAL       : result := op_val(PL);
        oeVAR       : result := op_var(PL);
        oeWHEN      : result := op_when(PL);
        oeWHILE     : result := op_while(PL);
        oeWITH      : result := op_with(PL);
        else raise ELE.Create('неизвестный оператор');
    end;
end;

function TEvaluationFlow.call_predicate(PL: TVList): TValue;
begin
    if PL.Count<>2 then ELE.Malformed(PL.AsString);
    if (PL.look[0] as TVPredicate).body(PL.look[1])
    then result := TVT.Create
    else result := TVList.Create;
end;

//function TEvaluationFlow.bind_procedure_parameters_to_stack(PL: TVList;
//    sign: TSubprogramSignature; ts: TVSymbolStack): boolean;
//var i, j, key_start:integer;
//    rest: TVList;
//    key_found: boolean;
//    procedure bind(n: unicodestring; FP: TValue);
//    var CP: TVChainPointer;
//    begin
//        CP := eval_link(FP);
//        ts.new_var(n, CP, false);
//    end;
//
//begin
//    assert((PL.look[0] as TVProcedure).evaluated,
//                'выполнение недовычисленной процедуры ' + PL.look[0].AsString);
//
//    //Write('bind >>'); print_stdout_ln(PL);
//    key_start := -1;
//    for i:=0 to Length(sign)-1 do begin
//        case sign[i].m of
//            spmNec:
//                if PL.Count> i+1
//                then begin
//                    bind(sign[i].n, PL.look[i+1]);
//                end
//                else raise ELE.InvalidParameters;
//            spmOpt:
//                if PL.Count> i+1
//                then ts.new_var(sign[i].n, eval(PL[i+1]), true)
//                else ts.new_var(sign[i].n, TVList.Create, true);
//            spmRest: begin
//                //TODO: излишнее копирование
//                rest := (PL as TVList).subseq(i+1, PL.Count) as TVList;
//                for j := 0 to Rest.Count-1 do rest[j] := eval(rest[j]);
//                ts.new_var(sign[i].n, rest, true);
//            end;
//            spmKey: begin
//                if key_start<0 then key_start := i+1;
//                key_found := false;
//                for j := 0 to ((PL.Count-key_start) div 2) -1 do
//                    if (PL.look[key_start+j*2] is TVSymbol)
//                        and (UpperCaseU(PL.name[key_start+j*2])=
//                            ':'+UpperCaseU(sign[i].n))
//                    then begin
//                    //TODO: ключевае параметры процедур не вычисляются !
//                        ts.new_var(sign[i].n, eval(PL[key_start+j*2+1]), true);
//                        key_found := true;
//                    end;
//                if not key_found then ts.new_var(sign[i].n, TVList.Create, true);
//            end;
//        end;
//    end;
//   // stack.Print(78);
//    result := true;
//
//end;

//function TEvaluationFlow.bind_macro_parameters_to_stack(PL: TVList;
//    sign: TSubprogramSignature; ts: TVSymbolStack): boolean;
//var i, j, key_start:integer;
//    rest: TVList;
//    key_found: boolean;
//    procedure bind(n: unicodestring; FP: TValue);
//    begin
//        ts.new_var(n, FP.Copy(), true)
//    end;
//
//begin
//    assert((PL.look[0] as TVProcedure).evaluated, 'выполнение недовычисленного макроса '+
//        PL.look[0].AsString);
//
//
//    key_start := -1;
//    for i:=0 to Length(sign)-1 do begin
//        case sign[i].m of
//            spmNec:
//                if PL.Count>= i+1
//                then begin
//                    bind(sign[i].n, PL.look[i+1]);
//                end
//                else raise ELE.InvalidParameters;
//            spmOpt:
//                if PL.Count>= i+1+1
//                then ts.new_var(sign[i].n, PL[i+1], true)
//                else ts.new_var(sign[i].n, TVList.Create, true);
//            spmRest: begin
//                //TODO: излишнее копирование
//                rest := (PL as TVList).subseq(i+1, PL.Count) as TVList;
//                ts.new_var(sign[i].n, rest, true);
//            end;
//            spmKey: begin
//                if key_start<0 then key_start := i+1;
//                key_found := false;
//                for j := 0 to ((PL.Count-key_start) div 2) -1 do
//                    if (PL.look[key_start+j*2] is TVSymbol)
//                        and (UpperCaseU(PL.name[key_start+j*2])=
//                            ':'+UpperCaseU(sign[i].n))
//                    then begin
//                    //TODO: ключевае параметры процедур не вычисляются !
//                        ts.new_var(sign[i].n, PL[key_start+j*2+1], true);
//                        key_found := true;
//                    end;
//                if not key_found then ts.new_var(sign[i].n, TVList.Create, true);
//            end;
//        end;
//    end;
//    result := true;
//
//end;

//procedure TEvaluationFlow.procedure_complement(V: TValue);
//var
//    proc: TVProcedure;
//    i: integer;
//begin
//    //эта процедура вызывается из мест:
//    // 1. bind_procedure_parameters_to_stack - на случай передачи выражения
//    //  (PROCEDURE ...) как параметра процедуры
//    // 2. internal_function_call
//    // 3. при вычислении символа в процедуру - на всякий случай
//    // 4. при вычислении результата блока с фреймом
//    // --5. при экспорте процедуры из модуля
//    // --6. из функции оператора MAP (только первый аргумент, нужно доделать остальные)
//    //TODO: очень сложный механизм определения момента довычисления процедуры
//    //для определения провалов в этом механизме в stack.index_of добавлен
//    //assert падающий при наличии нулевых указателей в стеке.
//    //альтернативный варинант - вообще избавиться от довычислений. А требовать
//    //предварительного описания переменных для рекурсивных функций
//    proc := V as TVProcedure;
//    if not proc.evaluated
//    then try
//        for i := 0 to proc.stack.Count-1 do
//            if proc.stack.stack[i].V=nil
//            then proc.stack.stack[i].V :=
//                    //stack.find_ref_in_frame_or_nil(proc.stack.stack[i].name,
//                    //                                proc.stack_pointer);
//                    stack.find_ref_in_frame_or_nil(proc.stack.stack[i].name,
//                                                    proc.stack_pointer);
//
//        proc.stack.remove_unbound;
//        proc.evaluated:=true;
//    finally
//    end;
//end;


function TEvaluationFlow.procedure_call(PL: TVList): TValue;
var first: TValue; proc: TVProcedure; params: TVList;
    error_message: unicodestring;
    i: integer;
    linkable: boolean;
begin
    //TODO: при вызове процедуры с несуществующими переменными не возникает ошибка

    first := PL.look[0];

    if not tpProcedure(first)
    then begin
        error_message := first.AsString + ' is not procedure';
        first.Free;
        raise ELE.Create(error_message, 'syntax');
    end;

    proc := first as TVProcedure;

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

function TEvaluationFlow.internal_function_call(PL: TVList): TValue;
var i: integer; PLI: TVList;
begin try
    PLI := TVList.Create([PL[0]]);
    PLI.SetCapacity(PL.Count);
    for i := 1 to PL.high do PLI.Add(eval(PL[i]));
    result := call_internal(PLI);
finally
    PLI.Free;
end; end;

function TEvaluationFlow.internal_predicate_call(PL: TVList): TValue;
var CP: TVChainPointer;
begin
    if PL.Count<>2 then ELE.InvalidParameters;
try
    CP := nil;
    CP := eval_link(PL.look[1]);
    if (PL.look[0] as TVPredicate).body(CP.look)
    then result := TVT.Create
    else result := TVList.Create;
finally
    CP.Free;
end;
end;


//procedure TEvaluationFlow.expand_ins(PL: TVList);
//var expand: boolean; i: integer; tmp: TVList;
//begin
//    //анализирует список на предмет наличия элементов-списков помеченных
//    //оператором INS и вклеивает их
//    expand := false;
//    for i := 0 to PL.High do
//        if vpListHeaded_INS(PL.look[i]) then begin expand := true; break; end;
//    if not expand then exit;
//    //WriteLn('INS>> ',PL.AsString);
//    tmp := TVList.Create;
//    tmp.SetCapacity(PL.Count);
//    for i:= 0 to PL.High do
//        if vpListHeaded_INS(PL.look[i])
//        then begin
//            if (PL.L[i].Count<>2)// or not tpList(PL.L[i].look[1])
//            then raise ELE.Malformed('INS')
//            else tmp.Append(eval(PL.L[i][1]) as TVList)
//        end
//        else tmp.Add(PL[i]);
//    PL.Clear;
//    PL.Append(tmp);
//    //TODO: много копировании при разворачивании INS
//    //TODO: обработку INS нужно перенести в вычислитель параметров функций
//    //это сделает его более предсказуемым по поведению
//end;



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
    //uname: unicodestring;
    estack: unicodestring;
  //  function op(oe: TOperatorEnum): TValue;
  //  begin result := TVOperator.Create(uname, oe, TVList.Create); end;

label return;
begin try
    {$IFDEF EVAL_DEBUG_PRINT}
    indent; Write('eval>> ');print_stdout_ln(V);
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
            //uname := (V as TVSymbol).uname;
            //WriteLn(uname);
            for o := low(ops) to high(ops) do
                //if ops[o].n=uname then begin
                if ops[o].nN = (V as TVSymbol).N then begin
                    result := TVOperator.Create(ops[o].nN, o);
                    goto return;
                end;
            try
                //WriteLn('eval symbol>> ',uname);
                PV := nil;
                PV := stack.find_ref(V as TVSymbol);

                if PV.V is TVChainPointer
                then result := (PV.V as TVChainPointer).value
                else result := PV.V.Copy;

                //if tpProcedure(result) and (result as TVProcedure).is_macro_symbol
                if result is TVMacroSymbol
                then try
                    PL := TVList.Create([result]);
                    result := eval(procedure_call(PL));
                    //goto return;
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
            then result := internal_function_call(V as TVList)
            else

            if tpPredicate((V as TVList).look[0])
            then result := internal_predicate_call(V as TVList)
            else

            if tpOperator((V as TVList).look[0])
            then   result := call_operator(V as TVList)

            else
                if (V as TVList).look[0] is TVProcedure
                then begin
                    //if ((V as TVList).look[0] as TVProcedure).is_macro
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
   indent; write('=  '); print_stdout_ln(result);
   {$ENDIF}
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
    root_evaluation_flow := TEvaluationFlow.Create(base_stack.Copy as TVSymbolStack);
    fill_ops_array;
    quote_operator := root_evaluation_flow.eval(TVSymbol.Create('QUOTE')) as TVOperator;
    T := TVT.Create;
    NULL := TVList.Create;

finalization
    NULL.Free;
    T.Free;
    quote_operator.Free;
    root_evaluation_flow.Free;
    base_stack.Free;
    free_int_fun_signs;
end.

