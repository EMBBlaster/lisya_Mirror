unit dlisp_eval;

{$mode delphi}

{$ASSERTIONS ON}

{$DEFINE mysql55}


interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    process, Classes, SysUtils, dlisp_values, dlisp_read, math, lisya_xml, mar,
    lisya_packages
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
        function op_structure(PL: TVList): TValue;
        function op_structure_as(PL: TVList): TValue;
        function op_val(PL: TVList): TValue;
        function op_var(PL: TVList): TValue;
        function op_when(PL: TVList): TValue;
        function op_while(PL: TVList): TValue;
        function op_with(PL: TVList): TValue;


        function extract_body_symbols(body: TVList): TVList;
        procedure fill_subprogram_stack(sp: TVProcedure; symbols: TVList);

        function bind_procedure_parameters_to_stack(
                            PL: TVList; sign: TSubprogramSignature;
                            ts: TVSymbolStack): boolean;
        function bind_macro_parameters_to_stack(
                            PL: TVList; sign: TSubprogramSignature;
                            ts: TVSymbolStack): boolean;
        procedure procedure_complement(V: TValue);
        function procedure_call(PL: TVList): TValue;
        function internal_function_call(PL: TVList): TValue;
        procedure expand_ins(PL: TVList);

        function eval_link(P: TValue): TVChainPointer;

        function eval(V: TValue): TValue;
    end;

function tpNIL(V: TValue): boolean;

function execute_file(filename: unicodestring): boolean;

implementation

var root_evaluation_flow: TEvaluationFlow;
    base_stack: TVSymbolStack;


type TTypePredicate = function (V: TValue): boolean;

function tpAny(V: TValue): boolean;
begin
    result := (V is TValue);
end;

function tpNumber(V: TValue): boolean;
begin
    result := (V is TVNumber);
end;

function tpDateTime(V: TValue): boolean;
begin
    result := (V is TVDateTime);
end;

function tpTimeInterval(V: TValue): boolean;
begin
    result := (V is TVDateTime);
end;

function tpInteger(V: tValue): boolean;
begin
    result := V is TVInteger;
end;

function tpRange(V: TValue): boolean;
begin
    result := V is TVRange;
end;

function tpFloat(V: TValue): boolean;
begin
    result := V is TVFloat;
end;

function tpString(V: TValue): boolean;
begin
    result := V is TVString;
end;

function tpStringOrNIL(V: TValue): boolean;
begin
    result := (V is TVString) or tpNIL(V);
end;

function tpList(V: TValue): boolean;
begin
    result := V is TVList;
end;

function tpNIL(V:  TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count=0);
end;

function tpTrue(V:  TValue): boolean;
begin
    result := not tpNIL(V);
end;

function tpBoolean(V:  TValue): boolean;
begin
    result := (V is TVT) or ((V is TVList) and ((V as TVList).Count=0));
end;

function tpListNotEmpty(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count>0);
end;

function tpT(V: TValue): boolean;
begin
    result := V is TVT;
end;

function tpAtom(V: TValue): boolean;
begin
    result := (not (V is TVList)) or ((V as TVList).Count=0);
end;

function tpSymbol(V: TValue): boolean;
begin
    result := (V is TVSymbol);
end;

function tpSymbol_in(V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = 'IN');
end;

function tpSymbol_range(V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = 'RANGE');
end;

function tpSymbol_list(V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = 'LIST');
end;

function tpListOfSymbols(V: TValue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result
    then for i := 0 to (V as TVList).Count-1 do
        if not ((V as TVList).look[i] is TVSymbol)
        then begin
            result := false;
            break;
        end;
end;

function tpListOfStrings(V: TValue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result
    then for i := 0 to (V as TVList).Count-1 do
        if not ((V as TVList).look[i] is TVString)
        then begin
            result := false;
            break;
        end;
end;

function tpListOfLists(V: TValue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result
    then for i := 0 to (V as TVList).Count-1 do
        if not ((V as TVList).look[i] is TVList)
        then begin
            result := false;
            break;
        end;
end;

function tpListOfIntegers(V: TValue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result
    then for i := 0 to (V as TVList).high do
        if not ((V as TVList).look[i] is TVInteger)
        then begin
            result := false;
            break;
        end;
end;

function tpListOfNumbers(V: TValue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result
    then for i := 0 to (V as TVList).high do
        if not ((V as TVList).look[i] is TVNumber)
        then begin
            result := false;
            break;
        end;
end;

function tpListOfByteVectors(V: TValue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result
    then for i := 0 to (V as TVList).high do
        if not ((V as TVList).look[i] is TVByteVector)
        then begin
            result := false;
            break;
        end;
end;

function tpOrdinarySymbol(V :TValue): boolean;
begin
    result := ((V is TVSymbol) and ((V as TVSymbol).name[1]<>':'));
end;

function tpListOfOrdinarySymbols(V: TValue): boolean;
var i: integer;
begin
    result := true;
    if V is TVList
    then
        begin
        for i := 0 to (V as TVList).Count-1 do
            if not tpOrdinarySymbol((V as TVList).look[i]) then result := false;
        end
    else
        result := false;
end;

function tpKeyword(V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).name[1]=':');
end;

function tpGoto(V: TValue): boolean;
begin
    result := V is TVGoto;
end;

function tpBreak(V: TValue): boolean;
begin
    result := V is TVBreak;
end;

function tpContinue(V: TValue): boolean;
begin
    result := V is TVContinue;
end;

function tpSelfEvaluating(V: TValue): boolean;
begin
    result := (V is TVNumber)
        or (V is TVString)
        or (V is TVT)
        or tpNil(V)
        or tpKeyword(V)
        or (V is TVProcedure)
        or (V is TVInternalFunction)
        or (V is TVOperator)
        or (V is TVRecord)
        or (V is TVRange)
        or (V is TVDateTime)
        or (V is TVSQLPointer)
        or (V is TVTimeInterval);
end;

function tpReferenceOnly(V: TValue): boolean;
begin
    result := (V is TVProcedure);
end;

function tpProcedure(V: TValue): boolean;
begin
    result := V is TVProcedure;
end;

function tpInternalFunction(V: TValue): boolean;
begin
    result := V is TVInternalFunction;
end;

function tpOperator(V: TValue): boolean;
begin
    result := V is TVOperator;
end;

function tpSubprogram(V: TValue): boolean;
begin
    result := V is TVSubprogram;
end;

function tpStreamPointer(V: TValue): boolean;
begin
    result := V is TVStreamPointer;
end;

function tpRecord(V: TValue): boolean;
begin
    result := V is TVRecord;
end;

function tpCompoundIndexed(V: TValue): boolean;
begin
    result := (V is TVCompoundIndexed);
end;

function tpByteVector(V: TValue): boolean;
begin
    result := V is TVByteVector;
end;

/////////////////////////////////////
/// Value Predicates ////////////////
/////////////////////////////////////

function vphKeywordName(V: TValue; const n: unicodestring): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = n);
end;

function vphSymbolName(V: TValue; const n: unicodestring): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = n);
end;

function vphListHeaded(V: TValue; const n: unicodestring): boolean;
begin
    result := (V is TVList)
        and ((V as TVList).Count>0)
        and tpSymbol((V as TVList).look[0])
        and ((V as TVList).uname[0]=n);
end;

function vpIntegerAbsOne                            (V: TValue): boolean;
begin
    result := (V is TVInteger) and (abs((V as TVInteger).fI) = 1);
end;

function vpIntegerNotZero                           (V: TValue): boolean;
begin
    result := (V is TVInteger) and ((V as TVInteger).fI <> 0);
end;

function vpNumberNotZero                            (V: TValue): boolean;
begin
    result := (V is TVNumber) and ((V as TVNumber).F <> 0);
end;

function vpNumberAbsOneOrMore                       (V: TValue): boolean;
begin
    result := (V is TVNumber) and (abs((V as TVNumber).F) >= 1);
end;

function vpNumberNotNegative                        (V: TValue): boolean;
begin
    result := (V is TVNumber) and ((V as TVNumber).F >= 0);
end;

function vpIntegerNotNegative                       (V: TValue): boolean;
begin
    result := (V is TVInteger) and ((V as TVInteger).fI >= 0);
end;

function vpIntegerNotNegativeORNIL                  (V: TValue): boolean;
begin
    result := tpNIL(V) or ((V is TVInteger) and ((V as TVInteger).fI >= 0));
end;

function vpIntegerRoundToRange                      (V: TValue): boolean;
begin
    result := (V is TVInteger)
        and Math.InRange((V as TVInteger).fI,
            low(math.TRoundToRange),
            high(math.TRoundToRange));
end;

function vpIntegerByte                              (V: TValue): boolean;
begin
    result := (V is TVInteger)
        and ((V as TVInteger).fI>=0)
        and ((V as TVInteger).fI<256);
end;

function vpKeywordKey                               (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':KEY');
end;

function vpKeywordRest                              (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':REST');
end;

function vpKeywordOptional                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':OPTIONAL');
end;

function vpKeywordCaptured                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':CAPTURED');
end;

function vpKeywordCapture                           (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':CAPTURE');
end;

function vpKeywordLast                              (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':LAST');
end;

function vpKeywordFirst                             (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':FIRST');
end;

function vpKeywordSecond                            (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':SECOND');
end;

function vpKeyword_ALL                              (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':ALL');
end;

function vpKeyword_RESET_STACK                      (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':RESET-STACK');
end;

function vpKeyword_PRINT_STACK                      (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':PRINT-STACK');
end;

function vpKeyword_READ                             (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':READ');
end;

function vpKeyword_WRITE                            (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':WRITE');
end;

function vpKeyword_APPEND                           (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':APPEND');
end;

function vpKeyword_BOM                              (V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).uname = ':BOM');
end;

function vpKeyword_UTF8                             (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':UTF8')
        or ((V as TVSymbol).uname = ':UTF-8'));
end;

function vpKeyword_UTF16BE                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':UTF16BE')
        or ((V as TVSymbol).uname = ':UTF-16BE'));
end;

function vpKeyword_UTF16LE                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':UTF16LE')
        or ((V as TVSymbol).uname = ':UTF-16LE'));
end;

function vpKeyword_UTF32BE                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':UTF32BE')
        or ((V as TVSymbol).uname = ':UTF-32BE'));
end;

function vpKeyword_UTF32LE                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':UTF32LE')
        or ((V as TVSymbol).uname = ':UTF-32LE'));
end;

function vpKeyword_CP1251                           (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':CP1251')
        or ((V as TVSymbol).uname = ':WINDOWS-1251'));
end;

function vpKeyword_CP1252                           (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':CP1252')
        or ((V as TVSymbol).uname = ':WINDOWS-1252'));
end;

function vpKeyword_DEFLATE                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':DEFLATE'));
end;

function vpKeyword_RESULT                           (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':RESULT'));
end;

function vpKeyword_FLAG                             (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
           ((V as TVSymbol).uname = ':FLAG'));
end;

function vpKeyword_LEFT                             (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':LEFT');
end;

function vpKeyword_RIGHT                            (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':RIGHT');
end;

function vpKeyword_CENTER                           (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':CENTER');
end;

function vpKeywordFileMode                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
        ((V as TVSymbol).uname = ':READ')
        or ((V as TVSymbol).uname = ':WRITE')
        or ((V as TVSymbol).uname = ':APPEND'));
end;

function vpKeywordFileModeOrNIL                     (V: TValue): boolean;
begin
    result := tpNil(V) or vpKeywordFileMode(V);
end;

function vpKeywordEncoding                          (V: TValue): boolean;
begin
    result := (V is TVSymbol) and (
            ((V as TVSymbol).uname = ':BOM')
        or vpKeyword_UTF8(V)
        or vpKeyword_CP1251(V));
end;

function vpKeywordEncodingOrNIL                     (V: TValue): boolean;
begin
    result := tpNIL(V) or vpKeywordEncoding(V);
end;

function vpKeywordAlign                             (V: TValue): boolean;
begin
    result := vpKeyword_LEFT(V) or vpKeyword_RIGHT(V) or vpKeyword_CENTER(V);
end;

function vpKeywordAlignOrNil                        (V: TValue): boolean;
begin
    result := tpNIL(V) or vpKeywordAlign(V);
end;

function vpSymbol_OTHERWISE                         (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'OTHERWISE');
end;

function vpSymbol_LAST                              (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'LAST');
end;

function vpStringEmpty                              (V: TValue): boolean;
begin
    result := (V is TVString) and ((V as TVString).S = '');
end;

function vpStreamPointerActive                      (V: TValue): boolean;
begin
    result := (V is TVStreamPointer) and
        ((V as TVStreamPointer).body.V <> nil);
end;

function vpSQLPointerActive                         (V: TValue): boolean;
begin
    result := (V is TVSQLPointer) and
        ((V as TVSQLPointer).body.V <> nil);
end;

function vpListHeaded_INS                           (V: TValue): boolean;
begin
    result := (V is TVList)
        and ((V as TVList).Count>0)
        and tpSymbol((V as TVList).look[0])
        and ((V as TVList).uname[0]='INS');
end;

function vpListHeaded_ELT                           (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'ELT');
end;

function vpListHeaded_THEN                          (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'THEN');
end;

function vpListHeaded_ELSE                          (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'ELSE');
end;

function vpListHeaded_EXCEPTION                     (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'EXCEPTION');
end;

function vpListEvenLength                           (V: TValue): boolean;
begin
    result := (V is TVList) and (((V as TVList).count mod 2) = 0);
end;

function vpListKeywordValue                         (V: TValue): boolean;
var i: integer;
begin
    result := vpListEvenLength(V);
    if result then
        for i :=  0 to (V as TVList).Count div 2 - 1 do
            if not tpKeyword((V as TVList).look[i*2]) then begin
                result := false;
                exit;
            end;
end;

function vpListSymbolValue                          (V: TValue): boolean;
var i: integer;
begin
    result := vpListEvenLength(V);
    if result then
        for i :=  0 to (V as TVList).Count div 2 - 1 do
            if not tpSymbol((V as TVList).look[i*2]) then begin
                result := false;
                exit;
            end;
end;

function vpListOfSymbolValuePairs                   (V: TValue): boolean;
var i: integer; L: TVList;
begin
    result := false;
    if V is TVList then begin
        L := V as TVList;
        for i :=  0 to L.high do
            if not (tpList(L.look[i])
                and (L.L[i].count=2)
                and tpOrdinarySymbol(L.L[i].look[0]))
            then exit;
    end
    else exit;
    result := true;
end;

function vpListOfByte                               (V: Tvalue): boolean;
var i: integer;
begin
    result := V is TVList;
    if result then
        for i := 0 to (V as TVList).High do
            if ((V as TVList).I[i]<0) or ((V as TVList).I[i]>255) then begin
                result := false;
                exit;
            end;
end;

function vpRangeNotNegative                         (V: TValue): boolean;
begin
    result := (V is TVRange) and ((V as TVRange).high>=(V as TVRange).low);
end;



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

function key_exists(L :TVList; key: unicodestring): TValue;
var i: integer;
begin
    for i := 0 to L.high do
        if tpKeyword(L.look[i]) and (key=L.uname[i])
        then begin
            result := TVT.Create;
            exit;
        end;
    result := TVList.Create;
end;

function min_list_length(L: TVList): integer;
var i, count: integer;
begin
    result := MaxInt;
    if tpNIL(L)
    then result := 0
    else
        for i := 0 to L.High do
            if L.L[i].Count < result then result := L.L[i].Count;
end;

function bind_parameters_list(PL: TVList; sign: TVList): TVList;
var i: integer;
    mode: TSubprogramParmeterMode;
const offset = 1;  //передаваемый список параметров первым пунктом будет
                    //содержать имя функции и его надо проигнорировать
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
            if vpKeywordKey(sign.look[i]) then mode := spmKey;
            if vpKeywordOptional(sign.look[i]) then mode := spmOpt;
            if vpKeywordrest(sign.look[i]) then mode := spmRest;
            if vpKeyword_FLAG(sign.look[i]) then mode := spmFlag;
        end
        else
            case mode of
                spmNec:
                    if (i+offset)<PL.Count
                    then result.Add(PL[i+offset])
                    else raise ELE.Create('insufficient parameters count', 'invalid parameters');
                spmOpt:
                    if (i-1+offset)<PL.Count
                    then result.Add(PL[i-1+offset])
                    else result.Add(TVList.Create);
                spmKey:
                    result.Add(value_by_key(PL, ':'+sign.uname[i]));
                spmRest:
                    result.Add(PL.Subseq(i-1+offset, PL.Count));
                spmFlag:
                    result.add(key_exists(PL, ':'+sign.uname[i]));
            end;
end;


function valid_sign(const PL: TVList; var E: TValue; tp: array of const): integer;
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
                    var E: TValue;
                    const tp: array of TTypePredicate): integer;
    function pr_confirm(case_n: integer): boolean; inline;
    var i: integer;
    begin
        result := false;
        for i := (case_n-1)*PL.Count to (case_n)*PL.Count-1 do
            if not tp[i](PL.look[i mod PL.Count]) then exit;
        result := true;
    end;
var i, case_count, case_n :integer;
begin
    //  Эта функция проверяет переданный список параметров на предмет
    //соответствия параметров шаблону.
    //  Если среди параметров есть ошибки первая из них возвращается через
    //вариантный параметр E в вызвавшую функция где должна использоваться в
    //качестве результата. Наличие ошибки сигнализируется возвратом значения -1
    //через результат функции. Таким образом если каждая функция будет проверять
    //свои параметры посредством этой, то ошибки будут распространяться
    //назад по стеку вызовов.
    //  Если ошибок в параметрах нет, то производится поиск подходящего шаблона
    //номер первого подошедшего возвращается как результат. Если ни один не
    //подошёл - возвращается 0.
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

function parse_subprogram_signature(PL: TVList): TSubprogramSignature;
var
    mode: TSubprogramParmeterMode;
    uname: unicodestring;
    i: integer;
label error;
begin
    mode := spmNec;
    SetLength(result, 0);
    for i:=0 to PL.Count-1 do
        if tpSymbol(PL.look[i])
        then
            if not (PL.name[i][1]='&')
            then begin
                SetLength(result, Length(result)+1);
                result[high(result)].n := PL.uname[i];
                result[high(result)].m := mode;
            end
            else
                if mode=spmNec
                then begin
                    uname := UpperCaseU(PL.name[i]);
                    if uname='&OPTIONAL' then mode := spmOpt;
                    if uname='&KEY' then mode := spmKey;
                    if (uname='&REST') or (uname='&BODY') then mode := spmRest;
                    if (uname='&CAPTURED') then mode := spmCaptured;
                end
                else
                    if UpperCaseU(PL.name[i])='&CAPTURED'
                    then mode := spmCaptured
                    else raise ELE.malformed('parameters list')
        else raise ELE.malformed('parameters list');
end;


//////////////////////////////////////////////
//// Internal Functions //////////////////////
//////////////////////////////////////////////

type TInternalFunction = function (const PL: TVList): TValue;

type TInternalFunctionRec = record
        n: unicodestring;
        f: TInternalFunctionBody;
        s: unicodestring;
    end;

procedure bool_to_TV(b: boolean; var E: TValue);
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

function ifh_equal(const A,B: TValue): boolean;
var type_v: (int, num, str, sym, t, lst, struct, any, bytevec); i: integer;
begin
    if tpInteger(A) and tpInteger(B)
    then type_v := int
    else
        if tpNumber(A) and tpNumber(B)
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
        num: result := (A as TVNumber).F=(B as TVNumber).F;
        str: result := (A as TVString).S=(B as TVString).S;
        sym: result := (A as TVSymbol).uname=(B as TVSymbol).uname;
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
    for i := 1 to Length(s) do begin
        result := stream.write_char(s[i]);
        if not result then break
    end;
end;

function ifh_predicate_template(PL: TVList; p: TTypePredicate): TValue; inline;
begin
    if PL.count<>1 then raise ELE.Malformed('predicate');

    if p(PL.look[0]) then result := TVT.Create else result := TVList.Create;
end;


function if_t_p                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,    tpT        );
end;

function if_true_p              (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,    tpTrue     );
end;

function if_nil_p               (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,     tpNIL     );
end;

function if_number_p            (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,   tpNumber    );
end;

function if_integer_p           (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,    tpInteger  );
end;

function if_float_p             (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,  tpFloat      );
end;

function if_atom_p              (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,  tpAtom       );
end;

function if_subprogram_p        (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL, tpSubprogram  );
end;

function if_list_p              (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,  tpList       );
end;

function if_symbol_p            (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,  tpSymbol     );
end;

function if_keyword_p           (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL,  tpKeyword   );
end;

function if_string_p            (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := ifh_predicate_template(PL, tpString     );
end;

function if_structure_p         (const PL: TVList; ep: TEvalProc): TValue;
var s: unicodestring; w,i: integer; lr: boolean; names: TStringList;
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

function if_add                 (const PL: TVList; ep: TEvalProc): TValue;
var i: integer; fres: double; ires: Int64;
begin
        ires := 0;
        fres := 0.0;
        case params_is(PL, result, [
            tpListOfIntegers,
            tpListOfNumbers]) of
            1: begin
                for i := 0 to PL.L[0].high do
                    ires := ires + PL.L[0].I[i];
                result := TVInteger.Create(ires);
            end;
            2: begin
                for i := 0 to PL.L[0].high do
                    fres := fres + PL.L[0].F[i];
                result := TVFloat.Create(fres);
            end;
        end;
end;

function if_sub                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
    {1} tpInteger,  tpNIL,
    {2} tpNumber,   tpNIL,
    {3} tpInteger,  tpInteger,
    {4} tpNumber,   tpNumber,
    {5} tpDateTime, tpDatetime,
    {6} tpDateTime, tpTimeInterval,
    {7} tpTimeInterval, tpTimeInterval]) of
        1: result := TVInteger.Create(-PL.I[0]);
        2: result := TVFloat.Create(-PL.F[0]);
        3: result := TVInteger.Create(PL.I[0] - PL.I[1]);
        4: result := TVFloat.Create(PL.F[0] - PL.F[1]);
        5,7: result := TVTimeInterval.Create(
            (PL.look[0] as TVTime).fDT - (PL.look[1] as TVTime).fDT);
        6: result := TVDateTime.Create(
            (PL.look[0] as TVTime).fDT - (PL.look[1] as TVTime).fDT);
    end;
end;

function if_mul                 (const PL: TVList; ep: TEvalProc): TValue;
var i: integer; fres: double; ires: Int64; A: TVList;
begin
    ires := 1;
    fres := 1.0;
    A := PL.L[0];
    case params_is(PL, result, [
        tpListOfIntegers,
        tpListOfNumbers]) of
        1: begin
            for i := 0 to A.high do ires := ires * A.I[i];
            result := TVInteger.Create(ires);
        end;
        2: begin
            for i := 0 to A.high do fres := fres * A.F[i];
            result := TVFloat.Create(fres);
        end;
    end;
end;

function if_div                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerAbsOne,    tpNIL,
        vpNumberNotZero,    tpNIL,
        tpInteger,          vpIntegerNotZero,
        tpNumber,           vpNumberNotZero]) of
        1: result := TVInteger.Create(1 div PL.I[0]);
        2: result := TVFloat.Create(1 / PL.F[0]);
        3: if ((PL.I[0] mod PL.I[1]) = 0)
            then result := TVInteger.Create(PL.I[0] div PL.I[1])
            else result := TVFloat.Create(PL.I[0] / PL.I[1]);
        4: result := TVFloat.Create(PL.F[0] / PL.F[1]);
    end;
end;

function if_div_int             (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, vpIntegerNotZero]) of
        1: result := TVInteger.Create(PL.I[0] div PL.I[1]);
    end;
end;

function if_mod                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, vpIntegerNotZero]) of
        1: result := TVInteger.Create(PL.I[0] mod PL.I[1]);
    end;
end;

function if_abs                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger,
        tpNumber]) of
        1: result := TVInteger.Create(abs(PL.I[0]));
        2: result := TVFloat.Create(abs(PL.F[0]));
    end;
end;

function if_power               (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger,            vpIntegerNotNegative,
        vpNumberNotNegative,  tpNumber,
        tpNumber,             vpNumberAbsOneOrMore]) of
        1: result := TVInteger.Create(PL.I[0] ** PL.I[1]);
        2,3: result := TVFloat.Create(PL.F[0] ** PL.F[1]);
    end;
end;

function if_sqrt                (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpNumberNotNegative]) of
        1: result := TVFloat.Create(PL.F[0] ** 0.5);
    end;
end;

function if_round               (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpNumber,   tpNIL,
        tpNumber,   vpIntegerRoundToRange]) of
        1: result := TVInteger.Create(round(PL.F[0]));
        2: result := TVFloat.Create(roundto(PL.F[0], TRoundToRange(PL.I[0])));
    end;
end;

function if_range               (const PL: TVList; ep: TEvalProc): TValue;
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

function if_symbol              (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: if (Pos(' ',PL.S[0])>0)
                or (Pos(#9,PL.S[0])>0)
                or (Pos(#10,PL.S[0])>0)
                or (Pos(#13,PL.S[0])>0)
                or (Pos('\',PL.S[0])>0)
                or (Pos('/',PL.S[0])>0)
                or (Pos('(',PL.S[0])>0)
                or (Pos(')',PL.S[0])>0)
                or (Pos('#',PL.S[0])>0)
                or (Pos(';',PL.S[0])>0)
                or (Pos('"',PL.S[0])>0)
                or (Pos('''',PL.S[0])>0)
            then raise ELE.Create('invalid symbol name', 'syntax')
            else result := TVSymbol.Create(PL.S[0]);
    end;
end;

function if_equal               (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [tpAny, tpAny]) of
        1: if ifh_equal(PL.look[0], PL.look[1])
            then result := TVT.Create
            else result := TVList.Create;
    end;
end;

function if_more                (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpNumber,  tpNumber,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]>PL.I[1] , result);
        2: bool_to_TV( PL.F[0]>PL.F[1] , result);
        3: bool_to_TV( PL.S[0]>PL.S[1] , result);
    end;
end;

function if_less                (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpNumber,  tpNumber,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]<PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<PL.S[1] , result);
    end;
end;

function if_more_or_equal       (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpNumber,  tpNumber,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]>=PL.I[1] , result);
        2: bool_to_TV( PL.F[0]>=PL.F[1] , result);
        3: bool_to_TV( PL.S[0]>=PL.S[1] , result);
    end;
end;

function if_less_or_equal       (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpNumber,  tpNumber,
        tpString,  tpString]) of
        1: bool_to_TV( PL.I[0]<=PL.I[1] , result);
        2: bool_to_TV( PL.F[0]<=PL.F[1] , result);
        3: bool_to_TV( PL.S[0]<=PL.S[1] , result);
    end;
end;

function if_not_equal           (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpInteger, tpInteger,
        tpNumber,  tpNumber,
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

function if_not                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpNIL,
        tpAny]) of
        1: result := TVT.Create;
        2: result := TVList.Create;
    end;
end;

function if_equal_case_insensitive(const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString, tpString]) of
        1: if UpperCaseU(PL.S[0])=UpperCaseU(PL.S[1])
            then result := TVT.Create
            else result := TVList.Create;
    end;
end;


function if_test_dyn            (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpAny, tpList]) of
        1: begin
            WriteLn('test dyn ', PL.look[0].AsString(),'  ',  PL.look[1].AsString());
            result := TVT.Create;
        end;
    end;
end;

function if_error               (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString, tpString,
        tpString, tpNIL,
        tpNIL,    tpNIL]) of
        1: raise ELE.Create(PL.S[0], PL.S[1]);
        2: raise ELE.Create(PL.S[0]);
        3: raise ELE.Create('');
    end;
end;

function if_extract_file_ext    (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(ExtractFileExt(PL.S[0]));
    end;
end;

function if_extract_file_name   (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(ExtractFileName(PL.S[0]));
    end;
end;

function if_extract_file_path   (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(DirSep(ExtractFilePath(PL.S[0])));
    end;
end;

function if_file_exists         (const PL: TVList; ep: TEvalProc): TValue;
var attr: DWORD; fn: unicodestring;
begin
    case params_is (PL, result, [
        tpString]) of
        1: begin
            fn := DirSep(PL.S[0]);
            if FileExists(fn)
            then begin
                if (FileGetAttr(fn) and faDirectory)<>0
                then result := TVString.Create(
                    IncludeTrailingPathDelimiter(ExpandFileName(fn)))
                else result := TVString.Create(ExpandFileName(fn));
            end
            else result := TVList.create;
        end;
    end;
end;

function if_directory_exists    (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if DirectoryExists(DirSep(PL.S[0]))
            then result := TVString.Create(DirSep(PL.S[0]))
            else result := TVList.create;
    end;
end;

function if_command_line        (const PL: TVList; ep: TEvalProc): TValue;
var i: integer;
begin
    result := TVList.Create;
    for i := 2 to paramCount do
        (result as TVList).Add(TVString.Create(paramStr(i)));
end;

function if_change_directory    (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if SetCurrentDir(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('change-directory error: '+PL.S[0]);
    end;
end;

function if_delete_file         (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if DeleteFile(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('delete-file error: '+PL.S[0]);
    end;
end;

function if_rename_file         (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString, tpString]) of
        1: if RenameFile(DirSep(PL.S[0]), DirSep(PL.S[1]))
            then result := TVT.Create
            else raise ELE.Create('rename-file error: '+PL.S[0]);
    end;
end;

function if_create_directory    (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if CreateDir(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('create-directory error: '+PL.S[0]);
    end;
end;

function if_remove_directory    (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is (PL, result, [
        tpString]) of
        1: if RemoveDir(DirSep(PL.S[0]))
            then result := TVT.Create
            else raise ELE.Create('remove-directory error: '+PL.S[0]);
    end;
end;

function if_guid                (const PL: TVList; ep: TEvalProc): TValue;
var g: TGUID;
begin
    if CreateGUID(g)=0
    then result := TVSymbol.Create(GUIDToString(g))
    else raise ELE.Create('GUID error');
end;


function if_every               (const PL: TVList; ep: TEvalProc): TValue;
var i, j, count: integer;
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
                result := ep(result);
                if tpNil(result) then exit;
                result.Free;
            end;
            result := TVT.Create;
        end;
    end;
end;

function if_some                (const PL: TVList; ep: TEvalProc): TValue;
var i, j, count: integer;
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
                result := ep(result);
                if not tpNil(result) then exit;
                result.Free;
            end;
            result := TVList.Create;
        end;
    end;
end;

function if_last                (const PL: TVList; ep: TEvalProc): TValue;
begin
    //print_stdout_ln(pl);
    case params_is(PL, result, [
    {1} tpNIL,
    {2} tpList,
    {3} vpStringEmpty,
    {4} tpString,
    {5} tpRange]) of
        1: result := TVList.Create;
        2: result := PL.L[0][PL.L[0].high];
        3: result := TVString.Create('');
        4: result := TVString.Create(PL.S[0][Length(PL.S[0])]);
        5: result := TVInteger.Create((PL.look[0] as TVRange).high);
    end;
end;

function if_car                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    //print_stdout_ln(pl);
    case params_is(PL, result, [
    {1} tpNIL,
    {2} tpList,
    {3} vpStringEmpty,
    {4} tpString,
    {5} tpRange]) of
        1: result := TVList.Create;
        2: result := PL.L[0][0];
        3: result := TVString.Create('');
        4: result := TVString.Create(PL.S[0][1]);
        5: result := TVInteger.Create((PL.look[0] as TVRange).low);
    end;
end;

function if_subseq              (const PL: TVList; ep: TEvalProc): TValue;
begin
    //print_stdout_ln(pl);
    case params_is(PL, result, [
        tpCompoundIndexed, vpIntegerNotNegative, tpNIL,
        tpCompoundIndexed, vpIntegerNotNegative, vpIntegerNotNegative]) of
        1: result := (PL.look[0] as TVCompoundIndexed).subseq(PL.I[1]);
        2: result := (PL.look[0] as TVCompoundIndexed).subseq(PL.I[1], PL.I[2]);
    end;
end;

function if_union               (const PL: TVList; ep: TEvalProc): TValue;
var i, j: integer;
begin
    //WriteLn('union>>',PL.AsString);
    case params_is(PL, result, [tpListOfLists]) of
        1: begin
            result := TVList.Create;
            for i := 0 to PL.L[0].high do
                for j := 0 to PL.L[0].L[i].high do
                    if not ifh_member((result as TVList), PL.L[0].L[i].look[j])
                    then (result as TVList).Add(PL.L[0].L[i][j]);
        end;
    end;
end;

function if_intersection        (const PL: TVList; ep: TEvalProc): TValue;
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

function if_difference          (const PL: TVList; ep: TEvalProc): TValue;
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

function if_member              (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [tpList, tpAny]) of
        1: if ifh_member(PL.L[0], PL.look[1])
            then result := PL[1]
            else result := TVList.Create;
    end;
end;

function if_position            (const PL: TVList; ep: TEvalProc): TValue;
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

function if_length              (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpCompoundIndexed]) of
        1: result := TVInteger.Create((PL.look[0] as TVCompound).Count);
    end;
end;

function if_list                (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpList]) of
        1: result := PL.L[0].Copy;
    end;
end;

function if_concatenate         (const PL: TVList; ep: TEvalProc): TValue;
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

function if_key                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpList, tpSymbol]) of
        1: result := value_by_key(PL.L[0], PL.uname[1]);
    end;
end;


function if_byte_vector         (const PL: TVList; ep: TEvalProc): TValue;
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

function if_bitwise_or          (const PL: TVList; ep: TEvalProc): TValue;
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

function if_bitwise_not         (const PL: TVList; ep: TEvalProc): TValue;
var i: integer; a, b: TVByteVector;
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

function if_bitwise_and         (const PL: TVList; ep: TEvalProc): TValue;
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

function if_bitwise_xor         (const PL: TVList; ep: TEvalProc): TValue;
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

function if_character           (const PL: TVList; ep: TEvalProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative]) of
        1: result := TVString.Create(unicodechar(PL.I[0]));
    end;
end;

function if_assertion           (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpTrue, tpString,
        tpNIL,  tpString]) of
        1: result := TVT.Create;
        2: raise ELE.Create(PL.S[1], 'assertion');
    end;
end;

function if_directory           (const PL: TVList; ep: TEvalProc): TValue;
var sr: TSearchRec; found: boolean; dir, fn, wc: unicodestring;
    exclude_files: boolean;
    attr: DWORD;
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

function if_sleep               (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpIntegerNotNegative]) of
        1: begin
            Sleep(PL.I[0]);
            result := TVT.Create;
        end;
    end;
end;

function if_execute_file        (const PL: TVList; ep: TEvalProc): TValue;
var prog_file: TVStreamPointer; expr, res: TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: try
            prog_file := nil;
            res := nil;
            prog_file := TVStreamPointer.Create(DirSep(PL.S[0]), fmRead, seBOM);
            while true do begin
                expr := nil;
                expr := dlisp_read.read(prog_file);

                if ((expr IS TVSymbol) and ((expr as TVSymbol).name='exit'))
                    or (expr is TVEndOfStream)
                then begin expr.Free; break; end;

                res := ep(expr);
                FreeAndNil(res);
            end;
        finally
            FreeAndNil(prog_file);
            FreeAndNil(res);
            result := TVT.Create;
        end;
    end;
end;

function execute_file(filename: unicodestring): boolean;
var expr: TVList; res: TValue;
begin try
    result := false;
    expr := TVList.Create([TVString.Create(filename)]);
    res := nil;
    try
        res := if_execute_file(expr, root_evaluation_flow.eval);
        result := true;
    except
        on E:ELE do begin
            WriteLn('ERROR during execution ',filename);
            WriteLn(E.Message,' (',E.EClass,')');
        end;
    end;
finally
    expr.Free;
    res.Free;
end; end;

function if_run_command         (const PL: TVList; ep: TEvalProc): TValue;
var output: string;
begin
    result := nil;
    case params_is(PL, result, [
        tpString, tpString,
        tpString, tpNIL]) of
        1: process.RunCommandInDir(PL.S[1], PL.S[0], output);
        2: process.RunCommand(PL.S[0], output);
    end;
    result := TVString.Create(output);
end;

function if_now                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    result := TVDateTime.Create(now);
end;

function if_open_file           (const PL: TVList; ep: TEvalProc): TValue;
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

            if tpNIL(PL.look[2]) then enc := seUTF8 else
            if vpKeyword_UTF8(PL.look[2]) then enc := seUTF8 else
            if vpKeyword_CP1251(PL.look[2]) then enc := seCP1251 else
            if vpKeyword_BOM(PL.look[2]) then enc := seBOM else
                enc:= seBOM;

            if fileExists(DirSep(PL.S[0])) or (mode <> fmRead)
            then result := TVStreamPointer.Create(DirSep(PL.S[0]), mode, enc)
            else raise ELE.Create(PL.S[0], 'file not found');
        end;
    end;
end;

function if_set_encoding        (const PL: TVList; ep: TEvalProc): TValue;
    function enc(e: TStreamEncoding): TValue;
    begin
        ((PL.look[0] as TVStreamPointer).body.V as TVStreamBody).encoding:=e;
        result := TVT.Create;
    end;
begin
    case params_is(PL, result, [
    {1} vpStreamPointerActive, vpKeyword_UTF8,
    {2} vpStreamPointerActive, vpKeyword_UTF16BE,
    {3} vpStreamPointerActive, vpKeyword_UTF16LE,
    {4} vpStreamPointerActive, vpKeyword_UTF32BE,
    {5} vpStreamPointerActive, vpKeyword_UTF32LE,
    {6} vpStreamPointerActive, vpKeyword_CP1251,
    {7} vpStreamPointerActive, vpKeyword_CP1252]) of
        1: result := enc(seUTF8);
        2: result := enc(seUTF16BE);
        3: result := enc(seUTF16LE);
        4: result := enc(seUTF32BE);
        5: result := enc(seUTF32LE);
        6: result := enc(seCP1251);
        7: result := enc(seCP1252);
    end;
end;

function if_set_compression_method(const PL: TVList; ep: TEvalProc): TValue;
var mode: TFileMode; enc: TStreamEncoding;
begin
    case params_is(PL, result, [
        tpStreamPointer, vpKeyword_DEFLATE,
        tpStreamPointer, tpNIL]) of
        1: begin
            (PL.look[0] as TVStreamPointer).Set_compression_mode(cmDeflate);
            result := TVT.Create;
        end;
        2: begin
            (PL.look[0] as TVStreamPointer).Set_compression_mode(cmNone);
            result := TVT.Create;
        end;
    end;
end;

function if_close_stream        (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpStreamPointer]) of
        1: begin
            (PL.look[0] as TVStreamPointer).close_stream;
            result := TVT.Create;
        end;
    end;
end;

function if_stream_position     (const PL: TVList; ep: TEvalProc): TValue;
var p: Int64;
begin
    case params_is(PL, result, [
        tpStreamPointer, vpIntegerNotNegative,
        tpStreamPointer, tpNIL]) of
        1: if (PL.look[0] as TVStreamPointer).set_position(PL.I[1])
           then result := TVT.Create
           else raise ELE.Create('set position', 'stream');
        2: begin
            if (PL.look[0] as TVStreamPointer).get_position(p)
            then result := TVInteger.Create(p)
            else raise ELE.Create('get position', 'stream');
        end;
    end;
end;

function if_stream_length       (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: result := TVInteger.Create((PL.look[0] as TVStreamPointer).stream_length);
    end;
end;

function if_read_byte           (const PL: TVList; ep: TEvalProc): TValue;
var b: byte;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: if (PL.look[0] as TVStreamPointer).read_byte(b)
            then result := TVInteger.Create(b)
            else result := TVList.Create;
    end;
end;

function if_read_bytes          (const PL: TVList; ep: TEvalProc): TValue;
var b: byte; i: integer; res: TVByteVector;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, vpIntegerNotNegative,
        vpStreamPointerActive, vpKeyword_ALL]) of
        1: try
            res := TVByteVector.Create;
            res.SetCount(PL.I[1]);
            for i := 0 to PL.I[1]-1 do begin
                if (PL.look[0] as TVStreamPointer).read_byte(b)
                then res[i] := b
                else raise ELE.Create('insufficient stream capacity');
            end;
        finally
            result := res;
        end;
        2: try
            res := TVByteVector.Create;
            while (PL.look[0] as TVStreamPointer).read_byte(b) do
                res.Add(b);
        finally
            result := res;
        end;
    end;
end;

function if_write_byte          (const PL: TVList; ep: TEvalProc): TValue;
var i: integer;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, vpIntegerByte,
        vpStreamPointerActive, tpByteVector]) of
        1: if (PL.look[0] as TVStreamPointer).write_byte(PL.I[1])
            then result := TVT.Create
            else raise ELE.Stream('write byte');
        2: begin
            for i := 0 to (PL.look[1] as TVByteVector).High do
                if not (PL.look[0] as TVStreamPointer).write_byte(
                                            (PL.look[1] as TVByteVector)[i])
                then raise ELE.Create('insufficient stream capacity', 'stream');
            result := TVT.Create;
        end;

    end;
end;

function if_read_character      (const PL: TVList; ep: TEvalProc): TValue;
var ch: unicodechar;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,
        tpNIL]) of
        1: if (PL.look[0] as TVStreamPointer).read_char(ch)
            then result := TVString.Create(ch)
            else result := TVList.Create;
        2: begin
            System.Read(ch);
            result := TVString.Create(ch);
        end;
    end;
end;

function if_write_string        (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpString,
        tpNil,                 tpString]) of
        1: if ifh_write_string(PL.look[0] as TVStreamPointer, PL.S[1])
            then result := TVT.Create
            else raise ELE.Stream('write string');
        2: begin
            System.Write(PL.S[1]);
            result := TVT.Create;
        end;
    end;
end;

function if_read_line           (const PL: TVList; ep: TEvalProc): TValue;
var ch: unicodechar; s: unicodestring;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,
        tpNIL]) of
        1: begin
            s := '';
            while (PL.look[0] as TVStreamPointer).read_char(ch) do
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

function if_write_line          (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpString,
        tpNIL,                 tpString]) of
        1: if ifh_write_string(PL.look[0] as TVStreamPointer, PL.S[1]+new_line)
            then result := TVT.Create
            else raise ELE.Stream('write line');
        2: begin
            System.WriteLn(PL.S[1]);
            result := TVT.Create;
        end;
    end;
end;

function if_read_bom            (const PL: TVList; ep: TEvalProc): TValue;
var b1, b2, b3, b4: byte;
begin
    Assert(false, 'read-bom not implemented');
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1:  //if read_byte(b1) and read_byte(b2) and read_byte(b3) and read_byte(b4)
            //then begin
            //if (b1=$EF) and (b2=$BB) and (b3=$BF)
            //then result := TVSymbol.Create(':UTF-8');
            //if (b1=$FF) and (b2=$FE) and (b3=$00) and (b4=$00)
            //then result := TVSymbol.Create(':UTF-32LE');


        //end;
    end;
end;

function if_write_bom           (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        vpStreamPointerActive]) of
        1: if (PL.look[0] as TVStreamPointer).write_BOM
            then result := TVT.Create
            else raise ELE.Stream('write BOM');
    end;
end;


function if_read                (const PL: TVList; ep: TEvalProc): TValue;
var i: integer; res: boolean; s: unicodestring;
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

function if_write               (const PL: TVList; ep: TEvalProc): TValue;
var i: integer; res: boolean;
begin
    case params_is(PL, result, [
        vpStreamPointerActive, tpAny,
        tpNIL,           tpAny]) of
        1: begin
                dlisp_read.print(
                    PL.look[1],
                    PL.look[0] as TVStreamPointer);
                //TODO: не кросплатформенный перенос строки
                (PL.look[0] as TVStreamPointer).write_char(#13);
                (PL.look[0] as TVStreamPointer).write_char(#10);
                result := TVT.Create;
                //TODO: не возвращается ошибка при записи в файл
        end;
        2: begin
            result := TVT.Create;
            dlisp_read.print(PL.look[1], nil);
        end;
    end;
end;

function if_print               (const PL: TVList; ep: TEvalProc): TValue;
var i: integer;
begin
        case params_is(PL, result, [
            vpStreamPointerActive, tpAny,
            tpNIL,                 tpAny,
            vpKeyword_RESULT,      tpAny]) of
            1: begin
                print(PL.look[1], PL.look[0] as TVStreamPointer);
                for i := 1 to Length(new_line) do
                    (PL.look[0] as TVStreamPointer).write_char(new_line[i]);
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


function if_fmt                 (const PL: TVList; ep: TEvalProc): TValue;
var i: integer; res: boolean;
begin
    case params_is(PL, result, [
        vpStreamPointerActive,   tpList,
        tpNIL,                   tpList,
        vpKeyword_RESULT,        tpList,
        tpT,                     tpList]) of
        1: if ifh_write_string(PL.look[0] as TVStreamPointer, ifh_format(PL.L[1]))
            then result := TVT.Create
            else raise ELE.Stream('fmt');
        2: begin
            System.Write(ifh_format(PL.L[1]));
            result := TVT.Create;
        end;
        3,4: result := TVString.Create(ifh_format(PL.L[1]));
    end;
end;

function if_log                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result{%H-}, [
        tpList]) of
        1: begin
            System.WriteLn(ifh_format(PL.L[0]));
            result := TVT.Create;
        end;
    end;
end;

function if_hex                 (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result{%H-}, [
        vpIntegerNotNegative,  tpNIL,
        vpIntegerNotNegative,  vpIntegerNotNegative]) of
        1: result := TVString.Create(IntToHex(PL.I[0],
                        round(math.logn(16, PL.I[0]))));
        2: result := TVString.Create(IntToHex(PL.I[0], PL.I[1]));
    end;
end;

function if_fixed               (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result{%H-}, [
        tpNumber,  vpIntegerNotNegative]) of
        1: result := TVString.Create(FloatToStrF(PL.F[0], ffFixed, 0, PL.I[1]));
    end;
end;

function if_col                 (const PL: TVList; ep: TEvalProc): TValue;
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

function if_fmt_list            (const PL: TVList; ep: TEvalProc): TValue;
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

function if_upper_case          (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(UpperCaseU(PL.S[0]));
    end;
end;

function if_lower_case          (const PL: TVList; ep: TEvalProc): TValue;
begin
    case params_is(PL, result, [
        tpString]) of
        1: result := TVString.Create(LowerCaseU(PL.S[0]));
    end;
end;





function if_xml_read_from_string(const PL: TVList; ep: TEvalProc): TValue;
var s: unicodestring; w,i: integer; lr: boolean; names: TStringList;
begin
    case params_is(PL, result, [
        tpString]) of
        1: begin
            result := xml_read_from_string(PL.S[0]);
        end;
    end;
end;


function if_sql_mysql_connection(const PL: TVList; ep: TEvalProc): TValue;
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

function if_sql_query           (const PL: TVList; ep: TEvalProc): TValue;
var database: TVSQLPointer; rec: TVRecord; i,j: integer;
    bool_val: boolean; ucommand: unicodestring;
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

function if_sql_query_list      (const PL: TVList; ep: TEvalProc): TValue;
var database: TVSQLPointer; i: integer;
    bool_val: boolean; ucommand: unicodestring;
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


const int_dyn: array[1..102] of TInternalFunctionRec = (
(n:'T?';                    f:if_t_p;                   s:'(a)'),
(n:'TRUE?';                 f:if_true_p;                s:'(a)'),
(n:'NIL?';                  f:if_nil_p;                 s:'(a)'),
(n:'NUMBER?';               f:if_number_p;              s:'(a)'),
(n:'INTEGER?';              f:if_integer_p;             s:'(a)'),
(n:'FLOAT?';                f:if_float_p;               s:'(a)'),
(n:'ATOM?';                 f:if_atom_p;                s:'(a)'),
(n:'SUBPROGRAM?';           f:if_subprogram_p;          s:'(a)'),
(n:'LIST?';                 f:if_list_p;                s:'(a)'),
(n:'SYMBOL?';               f:if_symbol_p;              s:'(a)'),
(n:'KEYWORD?';              f:if_keyword_p;             s:'(a)'),
(n:'STRING?';               f:if_string_p;              s:'(a)'),
(n:'STRUCTURE?';            f:if_structure_p;           s:'(s :optional t)'),

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

(n:'=';                     f:if_equal;                 s:'(a b)'),
(n:'>';                     f:if_more;                  s:'(a b)'),
(n:'<';                     f:if_less;                  s:'(a b)'),
(n:'>=';                    f:if_more_or_equal;         s:'(a b)'),
(n:'<=';                    f:if_less_or_equal;         s:'(a b)'),
(n:'<>';                    f:if_not_equal;             s:'(a b)'),
(n:'NOT';                   f:if_not;                   s:'(a)'),
(n:'EQUAL-CASE-INSENSITIVE';f:if_equal_case_insensitive;s:'(s s)'),

(n:'TEST-DYN';              f:if_test_dyn;              s:'(a :rest r)'),
(n:'ERROR';                 f:if_error;                 s:'(:optional m c)'),


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

(n:'UNION';                 f:if_union;                 s:'(:rest a)'),
(n:'INTERSECTION';          f:if_intersection;          s:'(:rest a)'),
(n:'DIFFERENCE';            f:if_difference;            s:'(a b)'),
(n:'MEMBER';                f:if_member;                s:'(l e)'),
(n:'POSITION';              f:if_position;              s:'(l e)'),
(n:'LENGTH';                f:if_length;                s:'(l)'),
(n:'LIST';                  f:if_list;                  s:'(:rest e)'),
(n:'CONCATENATE';           f:if_concatenate;           s:'(:rest a)'),
(n:'KEY';                   f:if_key;                   s:'(l k)'),

(n:'BYTE-VECTOR';           f:if_byte_vector;           s:'(:rest b)'),
(n:'BITWISE-AND';           f:if_bitwise_and;           s:'(a b)'),
(n:'BITWISE-NOT';           f:if_bitwise_not;           s:'(a)'),
(n:'BITWISE-OR';            f:if_bitwise_or;            s:'(a b)'),
(n:'BITWISE-XOR';           f:if_bitwise_xor;           s:'(a b)'),
(n:'CHARACTER';             f:if_character;             s:'(n)'),

(n:'ASSERTION';             f:if_assertion;             s:'(c m)'),
(n:'DIRECTORY';             f:if_directory;             s:'(d)'),
(n:'SLEEP';                 f:if_sleep;                 s:'(m)'),
(n:'EXECUTE-FILE';          f:if_execute_file;          s:'(n)'),
(n:'RUN-COMMAND';           f:if_run_command;           s:'(c :optional d)'),
(n:'NOW';                   f:if_now;                   s:'()'),

(n:'OPEN-FILE';             f:if_open_file;             s:'(n :key mode encoding)'),
(n:'CLOSE-FILE';            f:if_close_stream;          s:'(s)'),
(n:'SET-ENCODING';          f:if_set_encoding;          s:'(s e)'),
(n:'SET-COMPRESSION-METHOD';f:if_set_compression_method;s:'(s c)'),
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

(n:'FMT';                   f:if_fmt;                   s:'(t :rest s)'),
(n:'LOG';                   f:if_log;                   s:'(:rest s)'),
(n:'HEX';                   f:if_hex;                   s:'(i :optional d)'),
(n:'FIXED';                 f:if_fixed;                 s:'(f :optional d)'),
(n:'COL';                   f:if_col;                   s:'(w v :optional a)'),
(n:'LST';                   f:if_fmt_list;              s:'(l :optional s b e)'),
(n:'UPPER-CASE';            f:if_upper_case;            s:'(s)'),
(n:'LOWER-CASE';            f:if_lower_case;            s:'(s)'),

(n:'XML:READ-FROM-STRING';  f:if_xml_read_from_string;  s:'(s)'),

(n:'SQL:MYSQL-CONNECTION';  f:if_sql_mysql_connection;  s:'(database :key host port username password)'),
(n:'SQL:QUERY';             f:if_sql_query;             s:'(db :rest q)'),
(n:'SQL:QUERY-LIST';        f:if_sql_query_list;        s:'(db :rest q)')


);


var ops: array[TOperatorEnum] of record n: unicodestring; s: TVList; end;

procedure fill_ops_array;
var o: TOperatorEnum;
    procedure op(s: unicodestring);
    var V: TVList;
    begin
        //WriteLn(s);
        V := read_from_string(s) as TVList;
        ops[o].n := V.uname[0];
        ops[o].s := V.Subseq(1, v.Count) as TVList;
        FreeAndNil(V);
    end;
begin
    for o := low(ops) to high(ops) do
        case o of
            oeAND       : op('(AND :rest a)');
            oeAPPEND    : op('(APPEND s s)');
            oeAPPLY     : op('(APPLY :rest p)');
            oeBLOCK     : op('(BLOCK :rest a)');
            oeBREAK     : op('(BREAK)');
            oeCASE      : op('(CASE o :rest c)');
            oeCOND      : op('(COND :rest a)');
            oeCONST     : op('(CONST n v)');
            oeCONTINUE  : op('(CONTINUE)');
            oeDEBUG     : op('(DEBUG key)');
            oeDEFAULT   : op('(DEFAULT n v)');
            oeELT       : op('(ELT o :rest s)');
            oeELSE      : op('(ELSE :rest body)');
            oeEXCEPTION : op('(EXCEPTION class :rest body)');
            oeFILTER    : op('(FILTER)');
            oeFOR       : op('(FOR i r :rest a)');
            oeGOTO      : op('(GOTO a)');
            oeIF        : op('(IF c t e)');
            oeIF_NIL    : op('(IF-NIL c d)');
            oeLAST      : op('(LAST l)');
            oeLET       : op('(LET ((s v)))');
            oeMACRO     : op('(MACRO m :rest b)');
            oeMACRO_SYMBOL: op('(MACRO-SYMBOL :rest body)');
            oeMAP       : op('(MAP f :rest l)');
            oeOR        : op('(OR :rest a)');
            oePACKAGE   : op('(PACKAGE name export :rest body)');
            oePOP       : op('(POP l)');
            oePROCEDURE : op('(PROCEDURE p :rest b)');
            oePUSH      : op('(PUSH l e)');
            oeQUOTE     : op('(QUOTE a)');
            oeRECORD    : op('(STRUCTURE :rest s)');
            oeRECORD_AS : op('(STRUCTURE-AS t :rest s)');
            oeSET       : op('(SET n v)');
            oeTHEN      : op('(THEN :rest body)');
            oeVAL       : op('(VAL a)');
            oeVAR       : op('(VAR n v)');
            oeWHEN      : op('(WHEN c :rest b)');
            oeWHILE     : op('(WHILE c :rest b)');
            oeWITH      : op('(WITH :rest m)');
            else raise Exception.Create('неизвестный оператор');
        end
end;

procedure clear_ops_array;
var o: TOperatorEnum;
begin
    for o := low(ops) to high(ops) do ops[o].s.Free;
end;

procedure fill_base_stack;
var i: integer;
begin
    base_stack := TVSymbolStack.Create(nil);
    for i := low(int_dyn) to high(int_dyn) do
        base_stack.new_var(
            int_dyn[i].n,
            TVInternalFunction.Create(
                    read_from_string(int_dyn[i].s) as TVList,
                    int_dyn[i].f,
                    int_dyn[i].n),
            true);
    //загрузка констант
    base_stack.new_var('NL', TVString.Create(new_line), true);
    base_stack.new_var('CR', TVString.Create(#10), true);
    base_stack.new_var('LF', TVString.Create(#13), true);
    base_stack.new_var('TAB', TVString.Create(#09), true);
    base_stack.new_var('SPACE', TVString.Create(' '), true);
end;


{$DEFINE CHAINPOINTER}

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
    exception_message, exception_class: unicodestring;
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
   //         WriteLn('val Ele>> ',E.EClass);
        end;
        on E:Exception do begin
  //          WriteLn('val>> ',E.className);
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
        if vpListHeaded_Exception(PL.look[pc]) then
             if (PL.L[pc].Count>=2) and tpString(PL.L[pc].look[1])
             then begin
                eh := UpperCaseU(PL.L[pc].S[1]);
                if (Length(eh)<=Length(ec)) and (eh=ec[1..length(eh)])
                then result := true;
             end
             else result := true;
    end;

begin
  //  if tpInteger(PL.look[start]) then
  //      WriteLn(PL.asString);

    frame_start := stack.Count;
    if with_frame then stack.new_var( ' <block>', TVT.Create);
    //result := oph_frameless_block(PL, start);

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
                if tpBreak(V) or tpContinue(V) then break;
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
                    result := oph_block(PL.L[pc], 1, false);
                    stack.clear_frame(exception_frame_start);
                    goto return;
                end;
                inc(pc);
            end;
            //если обработчик не найден
            if with_frame then stack.clear_frame(frame_start);
            raise ELE.Create(exception_message, exception_class);
        end;
    end;
    if (V is TVProcedure) and with_frame then procedure_complement(V);
    result := V;

return:
    if with_frame then stack.clear_frame(frame_start);
end;

function TEvaluationFlow.opl_elt(PL: TVList): TVChainPointer;
var i: integer; tmp: TValue;
begin
    //  Эта процедура должна вернуть указатель на компонент составного типа
    //для дальнейшего извлечения или перезаписи значения.

    result := nil;
    tmp := nil;

    if PL.Count<2 then raise ELE.Create('недостаточно параметров');

    result := eval_link(PL.look[1]);

    for i := 2 to PL.High do begin
        PL[i] := eval(PL[i]);
        tmp := result.look;
        if tpCompoundIndexed(tmp) and vpIntegerNotNegative(PL.look[i])
        then result.add_index(PL.I[i])
        else
            if tpCompoundIndexed(tmp) and vpSymbol_LAST(PL.look[i])
            then result.add_index((tmp as TVCompound).Count-1)
            else
                if tpRecord(tmp) and tpSymbol(PL.look[i])
                then result.add_index((tmp as TVRecord).get_n_of(PL.uname[i]))
                else
                    if tpNIL(PL.look[i])
                    then begin
                        result.Free;
                        result := TVChainPointer.Create(
                            NewVariable(TVList.Create, true));
                        exit;
                    end
                    else
                        raise ELE.InvalidParameters;
    end;
end;

function TEvaluationFlow.opl_last(PL: TVList): TVChainPointer;
begin
    result := nil;
    result := eval_link(PL[1]);

    if not tpCompoundIndexed(result.look)
    then raise ELE.Create(result.look.AsString + ' is not indexed compound');

    result.add_index((result.look as TVCompoundIndexed).Count-1);
end;


function TEvaluationFlow.eval_link(P: TValue): TVChainPointer;
var i: integer;
begin try
    //эта функция должна попытаться вычислить указатель на место
    //если выражение не является корректным указанием на место,
    //то создать новую константу, содержащую значение выражения, и вернуть
    //указатель на неё
    result := nil;

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

    if tpListNotEmpty(P) then begin
        (P as TVList)[0] := eval((P as TVList)[0]);
        if tpOperator((P as TVList).look[0]) then begin
            expand_ins(P as TVList);
            case ((P as TVList).look[0] as TVOperator).op_enum of
                oeELT: result := opl_elt(P as TVList);
                oeLAST: result := opl_last(P as TVList);
                else result := TVChainPointer.Create(NewVariable(eval(P.Copy), true));
            end;
            exit;
        end;
        result := TVChainPointer.Create(NewVariable(eval(P.Copy), true));
        exit;
    end;

    raise ELE.Create('eval_link не обработанный случай');
except
    on E:ELE do begin
        raise ELE.Create('eval link '+P.AsString+new_line+'=> '+E.Message, E.EClass);
    end;
end;
end;


function TEvaluationFlow.op_and                     (PL: TVList): TValue;
var pc: integer;
begin
    result := TVList.Create;
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
var i: integer;
begin
    if Pl.Count<2 then raise ELE.Malformed('CASE');

    for i := 2 to PL.High do
        if tpNIL(PL.look[i]) then raise ELE.Malformed('CASE');
    //TODO: CASE не проверяет наличие OTHERWISE в середине списка альтернатив
    result := eval(PL[1]);
    //TODO: CASE должен вычислять ключи, иначе не получается использовать константы
    for i := 2 to PL.High do
        if ifh_equal(result, PL.L[i].look[0])
            or (tpListNotEmpty(PL.L[i].look[0]) and
                    ifh_member(PL.L[i].L[0], result))
            or tpT(PL.L[i].look[0])
            or vpSymbol_OTHERWISE(PL.L[i].look[0])
        then begin
            result.Free;
            result := oph_block(PL.L[i], 1, false);
            exit;
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
    case (PL.look[0] as TVOperator).op_enum of
        oeTHEN : raise ELE.Create('THEN not inside IF', 'syntax');
        oeELSE : raise ELE.Create('ELSE not inside IF', 'syntax');
        oeEXCEPTION : raise ELE.Create('EXCEPTION not inside block', 'syntax');
        else raise ELE.Create('secondary op error');
    end;
    result := TVT.Create;
end;

function TEvaluationFlow.op_pop                     (PL: TVList): TValue;
var CP: TVChainPointer;
begin
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
begin
    if (PL.Count<>3) or not tpOrdinarySymbol(PL.look[1])
    then raise ELE.malformed('CONST');
    stack.new_var(PL.name[1], eval(PL[2]), true);
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
        then stack.new_var(PL.uname[1], eval(PL[2]), true)
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

function TEvaluationFlow.op_filter                  (PL: TVList): TValue;
var
    i: integer; res: TVList;
begin
    // Write('filter>> ');print_stdout_ln(PL);
    //stack.print_stack;
    if PL.Count>=2
    then begin
        PL[1] := eval(PL[1]);
        PL[2] := eval(PL[2]);
    end;

    case valid_sign(PL, result, [@tpOperator,
        2,2, @tpSubprogram, @tpList]) of
        1: begin
            res := TVList.Create();
            for i := 0 to (PL[2] as TVList).Count-1 do begin
                result := eval(TVList.Create([PL[1], (PL[2] as TVList)[i]]));
                if not tpNIL(result)
                then res.Add((PL[2] as TVList)[i]);
                FreeAndNil(result);
            end;
        end;
        0: raise ELE.malformed('FILTER');
    end;
    result := res;
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

function TEvaluationFlow.op_structure               (PL: TVList): TValue;
var i: integer; tmp: TVRecord;
begin
    if (PL.Count mod 2)<>1 then raise ELE.Malformed('record');
try
    tmp := TVRecord.Create;
    for i := 0 to (PL.Count div 2)-1 do begin
        PL[2*i+2] := eval(PL[2*i+2]);
        if tpSymbol(PL.look[2*i+1])
        then tmp.AddSlot(PL.uname[2*i+1], PL[2*i+2])
        else raise ELE.Malformed('record: '+PL.look[2*i+1].AsString+' is not symbol');
    end;
    result := tmp.Copy;
finally
    tmp.Free;
end;
end;

function TEvaluationFlow.op_structure_as            (PL: TVList): TValue;
var i: integer; rec: TVRecord;
begin
    //TODO: очень запутанный алгоритм проверки параметров
    if PL.Count<2 then raise ELE.malformed('RECORD-AS');
    PL[1] := eval(PL[1]);

    if not tpRecord(PL.look[1])
    then raise ELE.Create(PL.look[1].AsString+' не структура', 'invalid parameters');
    if (PL.Count mod 2)<>0
    then raise ELE.Malformed('RECORD-AS: не чётное число параметров');

    rec := PL.look[1] as TVRecord;
    for i := 1 to (PL.Count div 2)-1 do begin
        PL[2*i+1] := eval(PL[2*i+1]);
        if tpSymbol(PL.look[2*i])
        then rec.slot[PL.uname[2*i]] := PL[2*i+1]
        else raise ELE.Create(PL.look[2*i].AsString+' is not symbol');
    end;
    result := rec.Copy;
end;

function TEvaluationFlow.op_cond                    (PL: TVList): TValue;
var pc, i, sp_i: integer; tmp: TValue;
begin try
    case valid_sign(PL, result{%H-}, [@tpOperator,
        0,-1, @tpListNotEmpty]) of
        1: begin
            tmp := TVList.Create;
            for i := 1 to PL.Count-1 do begin
                FreeAndNil(tmp);
                tmp := eval(PL.L[i][0]);
                if not tpNIL(tmp)
                then begin
                    for pc := 1 to PL.L[i].Count-1 do begin
                        FreeAndNil(tmp);
                        tmp := eval(PL.L[i][pc]);
                    end;
                    break;
                end;
            end;
        end;
        0: raise ELE.malformed('COND');
    end;
    result := tmp.Copy;
finally
    tmp.Free;
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
    stack.new_var(' <for-list2>', TVT.Create);

    case op_var of
        sequence: begin
            high_i := (CP.look as TVCompoundIndexed).high;
            CP.add_index(0);
            stack.new_var(PL.uname[1], CP, true);
            for i := 0 to high_i do begin
                CP.set_last_index(i);
                FreeAndNil(V);
                V := oph_block(PL, 3, true);
                if tpBreak(V) then begin
                    result := CP.value;
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
            index := TVInteger.Create(0);
            stack.new_var(PL.uname[1], index, true);
            for i := low_i to high_i do begin
                index.fI := i;
                FreeAndNil(V);
                V := oph_block(PL, 3, true);
                if tpBreak(V) then begin
                    result := TVInteger.Create(i);
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

    result := TVGoto.Create(PL[1] as TVSymbol);
end;

function TEvaluationFlow.op_if                      (PL: TVList): TValue;
begin
    if (PL.Count<3) or (PL.Count>4)
        //TODO: эти проверки синтаксиса IF нужны только для более ясного сообщения об ошибке
        //их отсутствие приведёт к падению с сообщением symbol not bound THEN/ELSE
        //при условии, что переменные с такими именами не существуют
        //or vpListHeaded_ELSE(PL.look[2])
        //or ((PL.Count=4) and vpListHeaded_THEN(PL.look[3]))
    then raise ELE.malformed('IF');

    PL[1] := eval(PL[1]);

    if not tpNIL(PL.look[1])
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
end;

function TEvaluationFlow.op_if_nil                  (PL: TVList): TValue;
begin
    if PL.Count<>3 then raise ELE.malformed('IF-NIL');

    PL[1] := eval(PL[1]);
    if tpNIL(PL.look[1])
    then result := eval(PL[2])
    else result := PL[1];
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
                stack.set_var(VPL.L[i].uname[0], eval(VPL.L[i][1]));
                Inc(count);
            end;

            result := oph_block(PL,2, false);
        finally
            for j := 0 to count-1 do stack.set_var(VPL.L[j].uname[0], old_v[j]);
        end;
    finally
        FreeAndNil(old_v);
    end;
end;

function TEvaluationFlow.op_map                     (PL: TVList): TValue;
var binded, se: TVList; P: PVariable; i,j: integer;
begin
    for i := 1 to PL.High do PL[i] := eval(PL[i]);
    if PL.look[1] is TVProcedure then procedure_complement(PL.look[1]);
try
    binded := nil;
    binded := bind_parameters_list(PL, ops[oeMAP].s);
    case params_is(binded, result, [
        tpSubprogram, tpListOfLists]) of
        1: try
            result := TVList.Create;
            for i := 0 to min_list_length(binded.L[1])-1 do begin
                se := TVList.Create((binded[0]));
                se.SetCapacity(binded.L[1].Count+1);
                for j := 0 to binded.L[1].High do se.Add(binded.L[1].L[j][i]);
                (result as TVList).Add(eval(se));
            end;
        except
            //удалить результаты предыдущих итераций если текущая завершилась
            // с ошибкой
            FreeAndNil(result);
            raise;
        end;
    end;
finally
    FreeAndNil(binded);
end;
end;

function TEvaluationFlow.op_macro_symbol(PL: TVList): TValue;
var proc: TVProcedure; first_captured: integer; sl: TVList;
    sign_pos: integer;
begin
    result := nil;

    if (PL.Count<3) then raise ELE.Malformed('MACRO-SYMBOL');
    if not tpOrdinarySymbol(PL.look[1]) then raise ELE.InvalidParameters;


    proc := TVProcedure.Create;
    proc.is_macro_symbol := true;

    proc.name:=PL.name[1];

    result := proc;
    proc.stack_pointer := stack.count;
    proc.body.Append(PL.Subseq(2, PL.Count) as TVList);
    proc.fsignature := nil;
    //proc.evaluated:=true;

    try
        sl := extract_body_symbols(proc.body);
        fill_subprogram_stack(proc, sl);
    finally
        FreeAndNil(sl);
    end;

    procedure_complement(proc);

    stack.new_var(PL.uname[1],result.Copy, true);
end;

function TEvaluationFlow.op_or                      (PL: TVList): TValue;
var pc: integer;
begin
    result := TVT.Create;
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
        P := package_stack.find_ref_or_nil(PL.L[2].uname[i]);
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
var expr: TVList; i: integer;
begin
    expr := TVList.Create;
    if PL.Count<2 then raise ELE.Malformed('APPLY');
try
    expr.Add(eval(PL[1]));
    for i := 2 to PL.high-1 do expr.Add(eval(PL[i]));
    if (PL.Count>=3) then begin
        if tpList(PL.look[PL.high]) then begin
            expr.Append(PL[PL.high] as TVList);
            result := eval(expr.Copy);
        end
        else raise ELE.InvalidParameters;
    end;
finally
    expr.Free;
end;
end;

function TEvaluationFlow.op_procedure               (PL: TVList): TValue;
var proc: TVProcedure; first_captured: integer; sl: TVList;
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

    proc := TVProcedure.Create;
    proc.is_macro := (PL.look[0] as TVOperator).op_enum=oeMACRO;

    if sign_pos=2 then proc.name:=PL.name[1];

    result := proc;
    proc.stack_pointer := stack.count;
    proc.body.Append(PL.Subseq(sign_pos+1, PL.Count) as TVList);
    proc.fsignature := parse_subprogram_signature(PL.look[sign_pos] as TVList);
    proc.evaluated:=false;

    try
        sl := extract_body_symbols(proc.body);
        fill_subprogram_stack(proc, sl);
    finally
        FreeAndNil(sl);
    end;

    if sign_pos=2 then stack.new_var(PL.uname[1],result.Copy, true);


end;

function TEvaluationFlow.op_var                     (PL: TVList): TValue;
begin
    if (PL.Count<2) or (PL.Count>3)
        or not tpOrdinarySymbol(PL.look[1])
    then raise ELE.malformed('VAR');

    case PL.Count of
        2: stack.new_var(PL.name[1], TVList.Create);
        3: stack.new_var(PL.name[1], eval(PL[2]));
    end;
    result := TVT.Create;
end;

function TEvaluationFlow.op_when(PL: TVList): TValue;
begin
    if PL.Count<2 then raise ELE.malformed('WHEN');

    PL[1] := eval(PL[1]);
    if not tpNIL(PL.look[1])
    then result := oph_block(PL, 2, false)
    else result := TVList.Create;
end;

function TEvaluationFlow.op_while                   (PL: TVList): TValue;
var cond, V: TValue;
begin
    if PL.Count<2 then raise ELE.malformed('WHILE');
try
    V := nil;
    cond := nil;
    cond := eval(PL[1]);
    while not tpNIL(cond) do begin
        V := oph_block(PL, 2, true);

        if tpBreak(V) then break;
        FreeAndNil(V);
        FreeAndNil(cond);
        cond := eval(PL[1]);
    end;
finally
    FreeAndNil(cond);
    FreeAndNil(V);
end;
    result := TVList.Create;
end;

function TEvaluationFlow.op_with(PL: TVList): TValue;
var external_stack, package_stack: TVSymbolStack;
    P: PVariable;
    i, j: integer;
    pack: TPackage;
    fn: unicodestring;
    expr: TVList;
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
                        pack.stack.find_ref(pack.export_list.uname[j]));
            end
            else begin
                fn := PL.name[i]+'.lisya';
                if FileExists(fn)
                then begin
                    FreeAndNil(result);
                    result := if_execute_file(TVList.Create([TVString.Create(fn)]), eval);
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
                FreeAndNil(result);
                result := if_execute_file(TVList.Create([TVString.Create(fn)]), eval);
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
        result := if_union(tmp, eval) as TVList;
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
        sp.stack.new_ref(symbols.uname[i],
            stack.find_ref_or_nil(symbols.uname[i]));
    end;
end;

function TEvaluationFlow.bind_procedure_parameters_to_stack(PL: TVList;
    sign: TSubprogramSignature; ts: TVSymbolStack): boolean;
var i, j, key_start:integer;
    rest: TVList;
    key_found: boolean;
    procedure bind(n: unicodestring; FP: TValue);
    var CP: TVChainPointer;
    begin
        CP := eval_link(FP);

        if CP.look is TVProcedure and not (CP.look as TVProcedure).evaluated
        then procedure_complement(CP.look);

        ts.new_var(n, CP, false);
    end;

begin
    assert((PL.look[0] as TVProcedure).evaluated,
                'выполнение недовычисленной процедуры ' + PL.look[0].AsString);

    //Write('bind >>'); print_stdout_ln(PL);
    key_start := -1;
    for i:=0 to Length(sign)-1 do begin
        case sign[i].m of
            spmNec:
                if PL.Count>= i+1
                then begin
                    bind(sign[i].n, PL.look[i+1]);
                end
                else raise ELE.InvalidParameters;
            spmOpt:
                if PL.Count>= i+1+1
                then ts.new_var(sign[i].n, eval(PL[i+1]))
                else ts.new_var(sign[i].n, TVList.Create);
            spmRest: begin
                //TODO: излишнее копирование
                rest := (PL as TVList).subseq(i+1, PL.Count) as TVList;
                for j := 0 to Rest.Count-1 do rest[j] := eval(rest[j]);
                ts.new_var(sign[i].n, rest);
            end;
            spmKey: begin
                if key_start<0 then key_start := i+1;
                key_found := false;
                for j := 0 to ((PL.Count-key_start) div 2) -1 do
                    if (PL.look[key_start+j*2] is TVSymbol)
                        and (UpperCaseU(PL.name[key_start+j*2])=
                            ':'+UpperCaseU(sign[i].n))
                    then begin
                    //TODO: ключевае параметры процедур не вычисляются !
                        ts.new_var(sign[i].n, PL[key_start+j*2+1]);
                        key_found := true;
                    end;
                if not key_found then ts.new_var(sign[i].n, TVList.Create);
            end;
        end;
    end;
   // stack.Print(78);
    result := true;

end;

function TEvaluationFlow.bind_macro_parameters_to_stack(PL: TVList;
    sign: TSubprogramSignature; ts: TVSymbolStack): boolean;
var i, j, key_start:integer;
    rest: TVList;
    key_found: boolean;
    procedure bind(n: unicodestring; FP: TValue);
    begin
        ts.new_var(n, FP.Copy(), true)
    end;

begin
    assert((PL.look[0] as TVProcedure).evaluated, 'выполнение недовычисленного макроса '+
        PL.look[0].AsString);


    key_start := -1;
    for i:=0 to Length(sign)-1 do begin
        case sign[i].m of
            spmNec:
                if PL.Count>= i+1
                then begin
                    bind(sign[i].n, PL.look[i+1]);
                end
                else raise ELE.InvalidParameters;
            spmOpt:
                if PL.Count>= i+1+1
                then ts.new_var(sign[i].n, PL[i+1], true)
                else ts.new_var(sign[i].n, TVList.Create, true);
            spmRest: begin
                //TODO: излишнее копирование
                rest := (PL as TVList).subseq(i+1, PL.Count) as TVList;
                ts.new_var(sign[i].n, rest, true);
            end;
            spmKey: begin
                if key_start<0 then key_start := i+1;
                key_found := false;
                for j := 0 to ((PL.Count-key_start) div 2) -1 do
                    if (PL.look[key_start+j*2] is TVSymbol)
                        and (UpperCaseU(PL.name[key_start+j*2])=
                            ':'+UpperCaseU(sign[i].n))
                    then begin
                    //TODO: ключевае параметры процедур не вычисляются !
                        ts.new_var(sign[i].n, PL[key_start+j*2+1], true);
                        key_found := true;
                    end;
                if not key_found then ts.new_var(sign[i].n, TVList.Create, true);
            end;
        end;
    end;
    result := true;

end;

procedure TEvaluationFlow.procedure_complement(V: TValue);
var
    proc: TVProcedure;
    i: integer;
begin
    //эта процедура вызывается из мест:
    // 1. bind_procedure_parameters_to_stack - на случай передачи выражения
    //  (PROCEDURE ...) как параметра процедуры
    // 2. internal_function_call
    // 3. при вычислении символа в процедуру - на всякий случай
    // 4. при вычислении результата блока с фреймом
    // --5. при экспорте процедуры из модуля
    // --6. из функции оператора MAP (только первый аргумент, нужно доделать остальные)
    //TODO: очень сложный механизм определения момента довычисления процедуры
    //для определения провалов в этом механизме в stack.index_of добавлен
    //assert падающий при наличии нулевых указателей в стеке.
    //альтернативный варинант - вообще избавиться от довычислений. А требовать
    //предварительного описания переменных для рекурсивных функций
    proc := V as TVProcedure;
    if not proc.evaluated
    then try
        for i := 0 to proc.stack.Count-1 do
            if proc.stack.stack[i].V=nil
            then proc.stack.stack[i].V :=
                    stack.find_ref_in_frame_or_nil(proc.stack.stack[i].name,
                                                    proc.stack_pointer);

        proc.stack.remove_unbound;
        proc.evaluated:=true;
    finally
    end;
end;


function TEvaluationFlow.procedure_call(PL: TVList): TValue;
var first: TValue; proc: TVProcedure; i: integer; frame_start: integer;
    error_message: unicodestring;
    tmp_stack: TVSymbolStack; body: TVList;
begin
    //TODO: вызов процедуры с недостающими параметрами вызывает ошибку
    //TODO: при вызове процедуры с несуществующими переменными не возникает ошибка
    //TODO: лишний EVAL? голова должна быть вычислена до вызова?
    first := eval(PL[0]);

    if not tpProcedure(first)
    then begin
        error_message := first.AsString + ' is not procedure';
        first.Free;
        raise ELE.Create(error_message, 'syntax');
    end;


    proc := first as TVProcedure;
    if proc.is_macro
    then bind_macro_parameters_to_stack(PL, proc.fsignature, proc.stack)
    else bind_procedure_parameters_to_stack(PL, proc.fsignature, proc.stack);
    try
        tmp_stack := stack;
        stack := proc.stack;
        result := oph_block(proc.body, 0, false);
    finally
        stack := tmp_stack;
        proc.Free;
    end;

    //TODO: при очистке стэка, рекурсивные процедуры не освобождаются
end;

function TEvaluationFlow.internal_function_call(PL: TVList): TValue;
var i: integer; binded_PL: TVList;
begin
    //print_stdout_ln(PL);
    for i := 1 to PL.High do begin
        PL[i] := eval(PL[i]);
        if (PL.look[i] is TVProcedure) then procedure_complement(PL.look[i]);
    end;
try
    binded_PL := bind_parameters_list(PL,
                        (PL.look[0] as TVInternalFunction).signature);
    result := nil;
    result := (PL.look[0] as TVInternalFunction).body(binded_PL, eval);
finally
    FreeAndNil(binded_PL);
end; end;


procedure TEvaluationFlow.expand_ins(PL: TVList);
var expand: boolean; i,j: integer; tmp: TVList;
begin
    //анализирует список на предмет наличия элементов-списков помеченных
    //оператором INS и вклеивает их
    expand := false;
    for i := 0 to PL.High do
        if vpListHeaded_INS(PL.look[i]) then begin expand := true; break; end;
    if not expand then exit;
    //WriteLn('INS>> ',PL.AsString);
    tmp := TVList.Create;
    tmp.SetCapacity(PL.Count);
    for i:= 0 to PL.High do
        if vpListHeaded_INS(PL.look[i])
        then begin
            if (PL.L[i].Count<>2)// or not tpList(PL.L[i].look[1])
            then raise ELE.Malformed('INS')
            else tmp.Append(eval(PL.L[i][1]) as TVList)
        end
        else tmp.Add(PL[i]);
    PL.Clear;
    PL.Append(tmp);
    //TODO: много копировании при разворачивании INS
    //TODO: обработку INS нужно перенести в вычислитель параметров функций
    //это сделает его более предсказуемым по поведению
end;

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
    tmp: TValue;
    tmp_list: TVList;
    PV: PVariable;
    i, j: integer;
    o: TOperatorEnum;
    type_v: (selfEval, symbol, list, unexpected);
    head, uname: unicodestring;
    function op(oe: TOperatorEnum): TValue;
    begin result := TVOperator.Create(uname, oe, TVList.Create); end;

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
            uname := (V as TVSymbol).uname;
            //WriteLn(uname);
            for o := low(ops) to high(ops) do
                if ops[o].n=uname then begin
                    result := TVOperator.Create(uname, o, ops[o].s);
                    goto return;
                end;

            try
                //WriteLn('eval symbol>> ',uname);
                PV := nil;
                PV := stack.find_ref(uname);
                if tpProcedure(PV.V) and not (PV.V as TVProcedure).evaluated
                then procedure_complement(PV.V);

                if PV.V is TVChainPointer
                then result := (PV.V as TVChainPointer).value
                else result := PV.V.Copy;

                if tpProcedure(PV.V) and (PV.V as TVProcedure).is_macro_symbol
                then try
                    PL := TVList.Create([PV.V.Copy]);
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
            expand_ins(V as TVList);

            (V as TVList)[0] := eval((V as TVList)[0]);

            if tpInternalFunction((V as TVList).look[0])
            then result := internal_function_call(V as TVList)
            else

            if tpOperator((V as TVList).look[0])
            then
                case ((V as TVList).look[0] as TVOperator).op_enum of
                    oeAND       : result := op_and(V as TVList);
                    oeAPPEND    : result := op_append(V as TVList);
                    oeAPPLY     : result := op_apply(V as TVList);
                    oeBLOCK     : result := op_block(V as TVList);
                    oeBREAK     : result := TVBreak.Create;
                    oeCASE      : result := op_case(V as TVList);
                    oeCOND      : result := op_cond(V as TVList);
                    oeCONST     : result := op_const(V as TVList);
                    oeCONTINUE  : result := TVContinue.Create;
                    oeDEBUG     : result := op_debug(V as TVList);
                    oeDEFAULT   : result := op_default(V as TVList);
                    oeELT       : result := op_elt(V as TVList);
                    oeELSE      : result := op_secondary_error(V as TVList);
                    oeEXCEPTION : result := op_secondary_error(V as TVList);
                    oeFILTER    : result := op_filter(V as TVList);
                    oeFOR       : result := op_for(V as TVList);
                    oeGOTO      : result := op_goto(V as TVList);
                    oeIF        : result := op_if(V as TVList);
                    oeIF_NIL    : result := op_if_nil(V as TVList);
                    oeLAST      : result := op_last(V as TVList);
                    oeLET       : result := op_let(V as TVList);
                    oeMACRO     : result := op_procedure(V as TVList);
                    oeMACRO_SYMBOL: result := op_macro_symbol(V as TVList);
                    oeMAP       : result := op_map(V as TVList);
                    oeOR        : result := op_OR(V as TVList);
                    oePACKAGE   : result := op_package(V as TVList);
                    oePOP       : result := op_pop(V as TVList);
                    oePROCEDURE : result := op_procedure(V as TVList);
                    oePUSH      : result := op_push(V as TVList);
                    oeQUOTE     : result := (V as TVList)[1];
                    //TODO: QUOTE не проверяет количество аргументов
                    oeRECORD    : result := op_structure(V as TVList);
                    oeRECORD_AS : result := op_structure_as(V as TVList);
                    oeSET       : result := op_set(V as TVList);
                    oeTHEN      : result := op_secondary_error(V as TVList);
                    oeVAL       : result := op_val(V as TVList);
                    oeVAR       : result := op_var(V as TVList);
                    oeWHEN      : result := op_when(V as TVList);
                    oeWHILE     : result := op_while(V as TVList);
                    oeWITH      : result := op_with(V as TVList);
                    else raise ELE.Create('неизвестный оператор');
                end
            else
                if (V as TVList).look[0] is TVProcedure
                then begin
                    if ((V as TVList).look[0] as TVProcedure).is_macro
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
       E.Message := V.AsString+new_line+'=> '+E.Message;
       //E.message := 'eval ELE';
        V.Free;
        raise ELE.Create(E.Message, E.EClass);
    end;
    on E:Exception do begin
        //WriteLn('eval E>> ');
        try
            E.Message:=V.AsString+new_line+'=> '+E.Message+' (!'+E.ClassName+'!)';
        except
            E.Message:='XXXX'+new_line+'=> '+E.Message+' (!'+E.ClassName+'!)';
        end;
  //      stack.Print;
  //      WriteLn('>>>> ',E.Message);
        raise ELE.Create(E.Message, E.ClassName);
    end;
end;
end;



initialization
    fill_base_stack;
    root_evaluation_flow := TEvaluationFlow.Create(base_stack.Copy as TVSymbolStack);
    fill_ops_array;

finalization
    clear_ops_array;
    root_evaluation_flow.Free;
    base_stack.Free;
end.

