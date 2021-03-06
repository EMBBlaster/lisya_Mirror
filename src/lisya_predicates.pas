﻿unit lisya_predicates;
{$mode delphi}
{$ASSERTIONS ON}
interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    process, Classes, SysUtils, math, ucomplex
    {$IFDEF mysql55}
    ,mysql_55
    {$ENDIF}
    {$IFDEF mysql50}
    ,mysql_50
    {$ENDIF}
    ,dlisp_values, lisya_streams;


function tpAny                                      (V: TValue): boolean;

function tpAtom                                     (V: TValue): boolean;

function tpBoolean                                  (V: TValue): boolean;

function tpBreak                                    (V: TValue): boolean;

function tpBytes                                    (V: TValue): boolean;

function tpCharacter                                (V: TValue): boolean;

function tpComplex                                  (V: TValue): boolean;

function tpSequence                                 (V: TValue): boolean;

function tpContinue                                 (V: TValue): boolean;

function tpCompound                                 (V: TValue): boolean;

function tpDateTime                                 (V: TValue): boolean;

function tpFloat                                    (V: TValue): boolean;

function tpFunction                                 (V: TValue): boolean;

function tpGoto                                     (V: TValue): boolean;

function tpHashTable                                (V: TValue): boolean;

function tpInteger                                  (V: tValue): boolean;

function tpIntegerOrNIL                             (V: tValue): boolean;

function tpInternalFunction                         (V: TValue): boolean;

function tpKeyword                                  (V: TValue): boolean;

function tpKeywordOrNIL                             (V: TValue): boolean;

function tpList                                     (V: TValue): boolean;

function tpListNotEmpty                             (V: TValue): boolean;

function tpListOfByteses                            (V: TValue): boolean;

function tpListOfIntegers                           (V: TValue): boolean;

function tpListOfLists                              (V: TValue): boolean;

function tpListOfNumbers                            (V: TValue): boolean;

function tpListOfReals                              (V: TValue): boolean;

function tpListOfTimes                              (V: TValue): boolean;

function tpListOfDurations                          (V: TValue): boolean;

function tpListOfOrdinarySymbols                    (V: TValue): boolean;

function tpListOfStrings                            (V: TValue): boolean;

function tpListOfSubprograms                        (V: TValue): boolean;

function tpListOfSymbols                            (V: TValue): boolean;

function tpListOrSymbol                             (V: TValue): boolean;

function tpListOrQueue                              (V: TValue): boolean;

function tpMacro                                    (V: TValue): boolean;

function tpNIL                                      (V: TValue): boolean;

function tpNumber                                   (V: TValue): boolean;

function tpOperator                                 (V: TValue): boolean;

function tpOrdinarySymbol                           (V :TValue): boolean;

function tpQueue                                    (V: TValue): boolean;

function tpPredicate                                (V: TValue): boolean;

function tpProcedure                                (V: TValue): boolean;

function tpRoutine                                  (V: TValue): boolean;

function tpProcessPipes                             (V: TValue): boolean;

function tpProcessPointer                           (V: TValue): boolean;

function tpRange                                    (V: TValue): boolean;

function tpReal                                     (V: TValue): boolean;

function tpRealOrNIL                                (V: TValue): boolean;

function tpRecord                                   (V: TValue): boolean;

//function tpReferenceOnly                            (V: TValue): boolean;

function tpReturn                                   (V: TValue): boolean;

function tpSelfEvaluating                           (V: TValue): boolean;

function tpStreamPointer                            (V: TValue): boolean;

function tpStreamSerial                             (V: TValue): boolean;

function tpString                                   (V: TValue): boolean;

function tpStringOrNIL                              (V: TValue): boolean;

function tpSubprogram                               (V: TValue): boolean;

function tpSymbol                                   (V: TValue): boolean;

function tpT                                        (V: TValue): boolean;

function tpTime                                     (V: TValue): boolean;

function tpDuration                                 (V: TValue): boolean;

function tpTrue                                     (V: TValue): boolean;

function tpZIPArchivePointer                        (V: TValue): boolean;

/////////////////////////////////////
/// Value Predicates ////////////////
/////////////////////////////////////

function vpComplexNotZero                           (V: TValue): boolean;


function vpDurationNotNegative                      (V: TValue): boolean;


function vpEmpty                                    (V: TValue): boolean;


function vpFlag_LAZY                                (V :TValue): boolean;

function vpFlag_BY_HEAD                             (V :TValue): boolean;


function vpInteger60Range                           (V: TValue): boolean;

function vpInteger60RangeOrNIL                      (V: TValue): boolean;

function vpInteger1000Range                         (V: TValue): boolean;

function vpInteger1000RangeOrNIL                    (V: TValue): boolean;

function vpIntegerAbsOne                            (V: TValue): boolean;

function vpIntegerByte                              (V: TValue): boolean;

function vpIntegerNegative                          (V: TValue): boolean;

function vpIntegerNotNegative                       (V: TValue): boolean;

function vpIntegerNotNegativeOrNIL                  (V: TValue): boolean;

function vpIntegerNotZero                           (V: TValue): boolean;

function vpIntegerPositive                          (V: TValue): boolean;

function vpIntegerRoundToRange                      (V: TValue): boolean;

function vpIntegerWORD                              (V: TValue): boolean;

function vpIntegerWORDorNIL                         (V: TValue): boolean;


function vpKeyword__                                (V: TValue): boolean;

function vpKeyword_ALL                              (V: TValue): boolean;

function vpKeyword_APPEND                           (V: TValue): boolean;

function vpKeyword_BOM                              (V: TValue): boolean;

function vpKeyword_CAPTION                          (V: TValue): boolean;

function vpKeyword_CAPTURE                          (V: TValue): boolean;

function vpKeyword_CAPTURED                         (V: TValue): boolean;

function vpKeyword_CASE_INSENSITIVE                 (V: TValue): boolean;

function vpKeyword_CENTER                           (V: TValue): boolean;

function vpKeyword_CP1251                           (V: TValue): boolean;

function vpKeyword_CP1252                           (V: TValue): boolean;

function vpKeyword_CP866                            (V: TValue): boolean;

function vpKeyword_CPU_COUNT                        (V: TValue): boolean;

function vpKeyword_CSV                              (V: TValue): boolean;

function vpKeyword_DEFLATE                          (V: TValue): boolean;

function vpKeyword_EQUAL                            (V: TValue): boolean;

function vpKeyword_FIRST                            (V: TValue): boolean;

function vpKeyword_FLAG                             (V: TValue): boolean;

function vpKeyword_HTML                             (V: TValue): boolean;

function vpKeyword_INTEGER                          (V: TValue): boolean;

function vpKeyword_KEY                              (V: TValue): boolean;

function vpKeyword_KOI8R                            (V: TValue): boolean;

function vpKeyword_LAST                             (V: TValue): boolean;

function vpKeyword_LATIN1                           (V: TValue): boolean;

function vpKeyword_LEFT                             (V: TValue): boolean;

function vpKeyword_LESS                             (V: TValue): boolean;

function vpKeyword_MORE                             (V: TValue): boolean;

function vpKeyword_OPTIONAL                         (V: TValue): boolean;

function vpKeyword_PRINT_STACK                      (V: TValue): boolean;

function vpKeyword_PRINT_HASH_TABLE                 (V: TValue): boolean;

function vpKeyword_PRINT_LINKS                      (V: TValue): boolean;

function vpKeyword_PROLOGUE                         (V: TValue): boolean;

function vpKeyword_READ                             (V: TValue): boolean;

function vpKeyword_RESET_STACK                      (V: TValue): boolean;

function vpKeyword_RESET_PACKAGES                   (V: TValue): boolean;

function vpKeyword_REST                             (V: TValue): boolean;

function vpKeyword_RESULT                           (V: TValue): boolean;

function vpKeyword_RIGHT                            (V: TValue): boolean;

function vpKeyword_SECOND                           (V: TValue): boolean;

function vpKeyword_SEPARATE                         (V: TValue): boolean;

function vpKeyword_SHOW_GRAPH                       (V: TValue): boolean;

function vpKeyword_SHOW_LINKS                       (V: TValue): boolean;

function vpKeyword_THREADS_COUNT                    (V: TValue): boolean;

function vpKeyword_UTF16BE                          (V: TValue): boolean;

function vpKeyword_UTF16LE                          (V: TValue): boolean;

function vpKeyword_UTF32BE                          (V: TValue): boolean;

function vpKeyword_UTF32LE                          (V: TValue): boolean;

function vpKeyword_UTF8                             (V: TValue): boolean;

function vpKeyword_WIDTH                            (V: TValue): boolean;

function vpKeyword_WRITE                            (V: TValue): boolean;

function vpKeywordAlign                             (V: TValue): boolean;

function vpKeywordAlignOrNil                        (V: TValue): boolean;

function vpKeywordEncoding                          (V: TValue): boolean;

function vpKeywordEncodingOrNIL                     (V: TValue): boolean;

function vpKeywordFileMode                          (V: TValue): boolean;

function vpKeywordFileModeOrNIL                     (V: TValue): boolean;

function vpKeywordTableModeOrNIL                    (V: TValue): boolean;



function vpListEvenLength                           (V: TValue): boolean;

function vpListHeaded_ELSE                          (V: TValue): boolean;

function vpListHeaded_ELT                           (V: TValue): boolean;

function vpListHeaded_EXCEPTION                     (V: TValue): boolean;

function vpListHeaded_INS                           (V: TValue): boolean;

function vpListHeaded_INSET                         (V: TValue): boolean;

function vpListHeaded_SUBSEQ                        (V: TValue): boolean;

function vpListHeaded_THEN                          (V: TValue): boolean;

function vpListHeaded_VALUE                         (V: TValue): boolean;

function vpListHeadedByString                       (V: TValue): boolean;

function vpListLength_1_2                           (V: TValue): boolean;


function vpListKeywordValue                         (V: TValue): boolean;

function vpListLambdaExpression                     (V: TValue): boolean;

function vpListMatrix                               (V: TValue): boolean;

function vpListMatrixInteger                        (V: TValue): boolean;

function vpListOfByte                               (V: TValue): boolean;

function vpListOfDurationsForMul                    (V: TValue): boolean;

function vpListOfListsEqualLength                   (V: TValue): boolean;

function vpListOfSymbolValuePairs                   (V: TValue): boolean;

function vpListOfSummableTimes                      (V: TValue): boolean;

function vpListRoutineExpression                    (V: TValue): boolean;

function vpListSymbolValue                          (V: TValue): boolean;

function vpListVariableExpression                   (V: TValue): boolean;

function vpListWithLastList                         (V: TValue): boolean;


function vpNumberNotZero                            (V: TValue): boolean;


function vpOperatorVarDeclaration                   (V: TValue): boolean;

function vpOperatorVarOrConst                       (V: TValue): boolean;

function vpOperatorRoutine                          (V: TValue): boolean;


function vpPairOfMatrixInteger                      (V: TValue): boolean;


function vpRangeNotNegative                         (V: TValue): boolean;


function vpRealAbsOneOrMore                         (V: TValue): boolean;

function vpRealNegative                             (V: TValue): boolean;

function vpRealNotNegative                          (V: TValue): boolean;

function vpRealNotZero                              (V: TValue): boolean;

function vpRealPositive                             (V: TValue): boolean;

function vpRealZero                                 (V: TValue): boolean;


function vpSequenceNotEmpty                         (V: TValue): boolean;


function vpSQLPointerActive                         (V: TValue): boolean;


function vpStreamEnd                                (V: TValue): boolean;

function vpStreamPointerActive                      (V: TValue): boolean;


function vpStringEmpty                              (V: TValue): boolean;


function vpSymbol__                                 (V: TValue): boolean;

function vpSymbol_IN                                (V: TValue): boolean;

function vpSymbol_LAST                              (V: TValue): boolean;

function vpSymbol_LIST                              (V: TValue): boolean;

function vpSymbol_OTHERWISE                         (V: TValue): boolean;

function vpSymbol_RANGE                             (V: TValue): boolean;

function vpSymbolQualified                          (V: TValue): boolean;


implementation /////////////////////////////////////////////////////////////////

uses lisya_protected_objects;

function tphListOf(V: TValue; p: TTypePredicate): boolean; inline;
var i: integer;
begin
    result := false;
    if V is TVList
    then begin
        for i := 0 to (V as TVList).high do
            if not p((V as TVList).look[i]) then Exit;
        result := true;
    end;
end;


function tpAny(V: TValue): boolean;
begin
    result := (V is TValue);
end;

function tpAtom(V: TValue): boolean;
begin
    result := (not (V is TVList)) or ((V as TVList).Count=0);
end;

function tpBoolean(V:  TValue): boolean;
begin
    result := tpT(V) or tpNIL(V);
end;

function tpBreak(V: TValue): boolean;
begin
    result := V is TVBreak;
end;

function tpBytes(V: TValue): boolean;
begin
    result := V is TVBytes;
end;

function tpCharacter(V: TValue): boolean;
begin
    result := (V is TVString) and ((V as TVString).Count=1);
end;

function tpComplex(V: TValue): boolean;
begin
    result := V is TVComplex;
end;

function tpSequence(V: TValue): boolean;
begin
    result := (V is TVSequence);
end;

function tpContinue(V: TValue): boolean;
begin
    result := V is TVContinue;
end;

function tpCompound(V: TValue): boolean;
begin
    result := V is TVCompound;
end;

function tpDateTime(V: TValue): boolean;
begin
    result := (V is TVDateTime);
end;

function tpFloat(V: TValue): boolean;
begin
    result := V is TVFloat;
end;

function tpFunction(V: TValue): boolean;
begin
    result := V is TVFunction;
end;

function tpGoto(V: TValue): boolean;
begin
    result := V is TVGoto;
end;

function tpHashTable(V: TValue): boolean;
begin
    result := V is TVHashTable;
end;

function tpInteger(V: tValue): boolean;
begin
    result := V is TVInteger;
end;

function tpIntegerOrNIL(V: tValue): boolean;
begin
    result := tpInteger(V) or tpNIL(V);
end;

function tpInternalFunction(V: TValue): boolean;
begin
    result := V is TVInternalFunction;
end;

function tpKeyword(V: TValue): boolean;
begin
    result := V is TVKeyword;
end;

function tpKeywordOrNIL(V: TValue): boolean;
begin
    result := tpKeyword(V) or tpNIL(V);
end;

function tpList(V: TValue): boolean;
begin
    result := V is TVList;
end;

function tpListNotEmpty(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count>0);
end;

function tpListOfByteses(V: TValue): boolean;
begin
    result := tphListOf(V, tpBytes);
end;

function tpListOfIntegers(V: TValue): boolean;
begin
    result := tphListOf(V, tpInteger);
end;

function tpListOfLists(V: TValue): boolean;
begin
    result := tphListOf(V, tpList);
end;

function tpListOfNumbers(V: TValue): boolean;
begin
    result := tphListOf(V, tpNumber);
end;

function tpListOfReals(V: TValue): boolean;
begin
    result := tphListOf(V, tpReal);
end;

function tpListOfTimes(V: TValue): boolean;
begin
    result := tphListOf(V, tpTime);
end;

function tpListOfDurations(V: TValue): boolean;
begin
    result := tphListOf(V, tpDuration);
end;

function tpListOfOrdinarySymbols(V: TValue): boolean;
begin
    result := tphListOf(V, tpOrdinarySymbol);
end;

function tpListOfStrings(V: TValue): boolean;
begin
    result := tphListOf(V, tpString);
end;

function tpListOfSubprograms(V: TValue): boolean;
begin
    result := tphListOf(V, tpSubprogram);
end;

function tpListOfSymbols(V: TValue): boolean;
begin
    result := tphListOf(V, tpSymbol);
end;

function tpListOrSymbol(V: TValue): boolean;
begin
    result := (V is TVList) or (V is TVSymbol);
end;

function tpListOrQueue(V: TValue): boolean;
begin
    result := (V is TVList) or (V is TVQueue);
end;

function tpMacro(V: TValue): boolean;
begin
    result := V is TVMacro;
end;

function tpNIL(V:  TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count=0);
end;

function tpNumber(V: TValue): boolean;
begin
    result := V is TVNumber;
end;

function tpOperator(V: TValue): boolean;
begin
    result := V is TVOperator;
end;

function tpOrdinarySymbol(V :TValue): boolean;
begin
    result := (V is TVSymbol) and not (V is TVKeyword);
end;

function tpQueue(V: TValue): boolean;
begin
    result := V is TVQueue;
end;

function tpPredicate(V: TValue): boolean;
begin
    result := V is TVPredicate;
end;

function tpProcedure(V: TValue): boolean;
begin
    result := V is TVProcedure;
end;

function tpRoutine(V: TValue): boolean;
begin
    result := V is TVRoutine;
end;

function tpProcessPipes(V: TValue): boolean;
begin
    result := (V is TVStreamPointer) and ((V as TVStreamPointer).body is TLProcessPipes);
end;

function tpProcessPointer(V: TValue): boolean;
begin
    result := V is TVProcessPointer;
end;

function tpRange(V: TValue): boolean;
begin
    result := V is TVRange;
end;

function tpReal(V: TValue): boolean;
begin
    result := (V is TVReal);
end;

function tpRealOrNIL(V: TValue): boolean;
begin
    result := tpReal(V) or tpNIL(V);
end;

function tpRecord(V: TValue): boolean;
begin
    result := V is TVRecord;
end;

//function tpReferenceOnly(V: TValue): boolean;
//begin
//    result := V is TVProcedure;
//end;

function tpReturn(V: TValue): boolean;
begin
    result := V is TVReturn;
end;

function tpSelfEvaluating(V: TValue): boolean;
begin
    result := not (tpListNotEmpty(V) or tpOrdinarySymbol(V));
end;

function tpStreamPointer(V: TValue): boolean;
begin
    result := V is TVStreamPointer;
end;

function tpStreamSerial(V: TValue): boolean;
begin
    result := (V is TVStreamPointer)
        and ((V as TVStreamPointer).body is TLSerialStream);
end;

function tpString(V: TValue): boolean;
begin
    result := V is TVString;
end;

function tpStringOrNIL(V: TValue): boolean;
begin
    result := tpString(V) or tpNIL(V);
end;

function tpSubprogram(V: TValue): boolean;
begin
    result := V is TVSubprogram;
end;

function tpSymbol(V: TValue): boolean;
begin
    result := V is TVSymbol;
end;

function tpT(V: TValue): boolean;
begin
    result := V is TVT;
end;


function tpTime(V: TValue): boolean;
begin
    result := V is TVTime;
end;

function tpDuration(V: TValue): boolean;
begin
    result := V is TVDuration
end;

function tpTrue(V:  TValue): boolean;
begin
    result := not tpNIL(V);
end;

function tpZIPArchivePointer(V: TValue): boolean;
begin
    result := V is TVZipArchivePointer;
end;


/////////////////////////////////////
/// Value Predicates ////////////////
/////////////////////////////////////

function vphKeywordName(V: TValue; const n: unicodestring): boolean;
begin
    result := (V is TVKeyword) and ((V as TVKeyword).uname = n);
end;

function vphKeywordNames(V: TValue; const n: array of unicodestring): boolean;
var i: integer;
begin
    result := true;
    if V is TVKeyword then for i := 0 to high(n) do
        if (V as TVKeyword).uname=n[i] then Exit;

    result := false;
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

function vphListOpCall(V: TValue; const n: unicodestring;
                                                op_en: TOperatorEnum): boolean;
begin
    result := (V is TVList)
        and ((V as TVList).Count>0)
        and (vphSymbolName((V as TVList).look[0], n)
            or (((V as TVList).look[0] is TVOperator)
                and (((V as TVList).look[0] as TVOperator).op_enum = op_en)));
end;


function vpComplexNotZero                           (V: TValue): boolean;
begin
    result := (V is TVComplex) and
        (((V as TVComplex).fC.re<>0) or ((V as TVComplex).fC.im<>0));
end;

function vpDurationNotNegative(V: TValue): boolean;
begin
    result := (V is TVDuration) and ((V as TVDuration).fDT>=0);
end;

function vpEmpty(V: TValue): boolean;
begin
    result := ((V is TVCompound) and ((V as TVCompound).Count=0))
        or ((V is TVQueue) and (V as TVQueue).target.empty);
end;

function vpFlag_LAZY(V: TValue): boolean;
begin
    result := tpNIL(V) or vphSymbolName(V, ':LAZY');
end;

function vpFlag_BY_HEAD(V: TValue): boolean;
begin
    result := tpNIL(V) or vphSymbolName(V, ':BY-HEAD');
end;

function vpInteger60Range(V: TValue): boolean;
begin
    result := (V is TVInteger)
        and ((V as TVInteger).fI>=0)
        and ((V as TVInteger).fI<60);
end;

function vpInteger60RangeOrNIL(V: TValue): boolean;
begin
    result := vpInteger60Range(V) or tpNIL(V);
end;

function vpInteger1000Range(V: TValue): boolean;
begin
    result := (V is TVInteger)
        and ((V as TVInteger).fI>=0)
        and ((V as TVInteger).fI<1000);
end;

function vpInteger1000RangeOrNIL(V: TValue): boolean;
begin
    result := vpInteger1000Range(V) or tpNIL(V);
end;

function vpIntegerAbsOne                            (V: TValue): boolean;
begin
    result := (V is TVInteger) and (abs((V as TVInteger).fI) = 1);
end;

function vpIntegerByte                              (V: TValue): boolean;
begin
    result := (V is TVInteger)
        and ((V as TVInteger).fI>=0)
        and ((V as TVInteger).fI<256);
end;

function vpIntegerNegative(V: TValue): boolean;
begin
    result := (V is TVInteger) and ((V as TVInteger).fI < 0);
end;

function vpIntegerNotNegative                       (V: TValue): boolean;
begin
    result := (V is TVInteger) and ((V as TVInteger).fI >= 0);
end;

function vpIntegerNotNegativeOrNIL                  (V: TValue): boolean;
begin
    result := tpNIL(V) or ((V is TVInteger) and ((V as TVInteger).fI >= 0));
end;

function vpIntegerNotZero                           (V: TValue): boolean;
begin
    result := (V is TVInteger) and ((V as TVInteger).fI <> 0);
end;

function vpIntegerPositive(V: TValue): boolean;
begin
    result := (V is TVInteger) and ((V as TVInteger).fI > 0);
end;

function vpIntegerRoundToRange                      (V: TValue): boolean;
begin
    result := (V is TVInteger)
        and Math.InRange((V as TVInteger).fI,
            low(math.TRoundToRange),
            high(math.TRoundToRange));
end;

function vpIntegerWORD(V: TValue): boolean;
begin
    result := (V is TVInteger)
        and ((V as TVInteger).fI>=0)
        and ((V as TVInteger).fI<(256*256));
end;

function vpIntegerWORDorNIL(V: TValue): boolean;
begin
    result := vpIntegerWORD(V) or tpNIL(V);
end;


function vpKeyword__                                (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).N = _.N);
end;

function vpKeyword_ALL                              (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':ALL');
end;

function vpKeyword_APPEND                           (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':APPEND');
end;

function vpKeyword_BOM                              (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':BOM');
end;

function vpKeyword_CAPTION(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':CAPTION');
end;

function vpKeyword_CAPTURE                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':CAPTURE');
end;

function vpKeyword_CAPTURED                         (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':CAPTURED');
end;

function vpKeyword_CASE_INSENSITIVE(V: TValue): boolean;
begin
    result := vphKeywordName(V,':CASE-INSENSITIVE');
end;

function vpKeyword_CENTER                           (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':CENTER');
end;

function vpKeyword_CP1251                           (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':CP1251')
        or ((V as TVSymbol).uname = ':WINDOWS-1251'));
end;

function vpKeyword_CP1252                           (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':CP1252')
        or ((V as TVSymbol).uname = ':WINDOWS-1252'));
end;

function vpKeyword_CP866(V: TValue): boolean;
begin
    result := vphKeywordNames(V, [':CP866',':DOS']);
end;

function vpKeyword_CPU_COUNT(V: TValue): boolean;
begin
    result := vphSymbolName(V, ':CPU-COUNT');
end;

function vpKeyword_CSV(V: TValue): boolean;
begin
    result := vphSymbolName(V, ':CSV');
end;

function vpKeyword_DEFLATE                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':DEFLATE'));
end;

function vpKeyword_EQUAL                            (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':LESS');
end;

function vpKeyword_FIRST                            (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':FIRST');
end;

function vpKeyword_FLAG                             (V: TValue): boolean;
begin
    result := V.equal(kwFLAG);
end;

function vpKeyword_HTML(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':HTML');
end;

function vpKeyword_INTEGER(V: TValue): boolean;
begin
    result := vphKeywordNames(V, [':INTEGER',':ЦЕЛОЕ']);
end;

function vpKeyword_KEY                              (V: TValue): boolean;
begin
    result := V.equal(kwKEY);
end;

function vpKeyword_KOI8R                            (V: TValue): boolean;
begin
    result := vphKeywordNames(V, [':KOI8R',':KOI8-R', ':КОИ8']);
end;

function vpKeyword_LAST                             (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':LAST');
end;

function vpKeyword_LATIN1(V: TValue): boolean;
begin
    result := vphKeywordNames(V, [':LATIN1',':LATIN-1', ':ISO-8859-1', ':CP819']);
end;

function vpKeyword_LEFT                             (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':LEFT');
end;

function vpKeyword_LESS                             (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':LESS');
end;

function vpKeyword_MORE                             (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':MORE');
end;

function vpKeyword_OPTIONAL                         (V: TValue): boolean;
begin
    result := V.equal(kwOPTIONAL);
end;

function vpKeyword_PRINT_STACK                      (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':PRINT-STACK');
end;

function vpKeyword_PRINT_HASH_TABLE(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':PRINT-HASH-TABLE');
end;

function vpKeyword_PRINT_LINKS(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':PRINT-LINKS');
end;

function vpKeyword_PROLOGUE(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':PROLOGUE');
end;

function vpKeyword_READ                             (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':READ');
end;

function vpKeyword_RESET_STACK                      (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':RESET-STACK');
end;

function vpKeyword_RESET_PACKAGES(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':RESET-PACKAGES');
end;

function vpKeyword_REST                             (V: TValue): boolean;
begin
    result := V.equal(kwREST);
end;

function vpKeyword_RESULT                           (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':RESULT'));
end;

function vpKeyword_RIGHT                            (V: TValue): boolean;
begin
    result := vphKeywordName(V, ':RIGHT');
end;

function vpKeyword_SECOND                           (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':SECOND');
end;

function vpKeyword_SEPARATE(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':SEPARATE');
end;

function vpKeyword_SHOW_GRAPH(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':SHOW-GRAPH');
end;

function vpKeyword_SHOW_LINKS(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':SHOW-LINKS');
end;

function vpKeyword_THREADS_COUNT(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':THREADS-COUNT');
end;

function vpKeyword_UTF16BE                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':UTF16BE')
        or ((V as TVSymbol).uname = ':UTF-16BE'));
end;

function vpKeyword_UTF16LE                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':UTF16LE')
        or ((V as TVSymbol).uname = ':UTF-16LE'));
end;

function vpKeyword_UTF32BE                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':UTF32BE')
        or ((V as TVSymbol).uname = ':UTF-32BE'));
end;

function vpKeyword_UTF32LE                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':UTF32LE')
        or ((V as TVSymbol).uname = ':UTF-32LE'));
end;

function vpKeyword_UTF8                             (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
           ((V as TVSymbol).uname = ':UTF8')
        or ((V as TVSymbol).uname = ':UTF-8'));
end;

function vpKeyword_WIDTH(V: TValue): boolean;
begin
    result := vphKeywordName(V, ':WIDTH');
end;

function vpKeyword_WRITE                            (V: TValue): boolean;
begin
    result := (V is TVKeyword) and ((V as TVSymbol).uname = ':WRITE');
end;

function vpKeywordAlign                             (V: TValue): boolean;
begin
    result := vpKeyword_LEFT(V) or vpKeyword_RIGHT(V) or vpKeyword_CENTER(V);
end;

function vpKeywordAlignOrNil                        (V: TValue): boolean;
begin
    result := tpNIL(V) or vpKeywordAlign(V);
end;

function vpKeywordEncoding                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
            ((V as TVSymbol).uname = ':BOM')
        or vpKeyword_UTF8(V)
        or vpKeyword_CP1251(V)
        or vpKeyword_CP1252(V)
        or vpKeyword_CP866(V)
        or vpKeyword_KOI8R(V)
        or vpKeyword_UTF16LE(V)
        or vpKeyword_UTF16BE(V)
        or vpKeyword_UTF32LE(V)
        or vpKeyword_UTF32BE(V)
        or vpKeyword_LATIN1(V)
        );
end;

function vpKeywordEncodingOrNIL                     (V: TValue): boolean;
begin
    result := tpNIL(V) or vpKeywordEncoding(V);
end;

function vpKeywordFileMode                          (V: TValue): boolean;
begin
    result := (V is TVKeyword) and (
        ((V as TVSymbol).uname = ':READ')
        or ((V as TVSymbol).uname = ':WRITE')
        or ((V as TVSymbol).uname = ':APPEND'));
end;

function vpKeywordFileModeOrNIL                     (V: TValue): boolean;
begin
    result := tpNil(V) or vpKeywordFileMode(V);
end;

function vpKeywordTableModeOrNIL(V: TValue): boolean;
begin
    result := tpNil(V) or vphKeywordNames(V, [':CSV', ':HTML']);
end;



function vpListEvenLength                           (V: TValue): boolean;
begin
    result := (V is TVList) and (((V as TVList).count mod 2) = 0);
end;

function vpListHeaded_ELSE                          (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'ELSE');
end;

function vpListHeaded_ELT                           (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'ELT');
end;

function vpListHeaded_EXCEPTION                     (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'EXCEPTION');
end;

function vpListHeaded_INS                           (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'INS');
end;

function vpListHeaded_INSET(V: TValue): boolean;
begin
    result := vphListHeaded(V, 'INSET');
end;

function vpListHeaded_SUBSEQ(V: TValue): boolean;
begin
    result := vphListHeaded(V, 'SUBSEQ');
end;

function vpListHeaded_THEN                          (V: TValue): boolean;
begin
    result := vphListHeaded(V, 'THEN');
end;

function vpListHeaded_VALUE(V: TValue): boolean;
begin
    result := vphListHeaded(V, 'VALUE');
end;

function vpListHeadedByString(V: TValue): boolean;
begin
    result := (V is TVList)
            and ((V as TVList).Count>0)
            and ((V as TVList).look[0] is TVString);
end;

function vpListLength_1_2(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count<=2) and ((V as TVList).Count>=1);
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

function vpListLambdaExpression(V: TValue): boolean;
var L: TVList;
begin
    result := V is TVList;
    if not result then Exit;
    L := V as TVList;
    result := (L.Count>=2) and (L.look[0] is TVOperator)
        and ((L.look[0] as TVOperator).op_enum in [oePROCEDURE, oeFUNCTION, oeMACRO])
        and tpList(L.look[1]);
end;

function vpListMatrix(V: TValue): boolean;
var L: TVList; i: integer;
begin
    result := vpListOfListsEqualLength(V);
    if not result then Exit;
    L := V as TVList;
    for i := 0 to L.high do begin
        result := tpListOfNumbers(L.L[i]);
        if not result then Exit;
    end;
end;

function vpListMatrixInteger(V: TValue): boolean;
var L: TVList; i: integer;
begin
    result := vpListOfListsEqualLength(V);
    if not result then Exit;
    L := V as TVList;
    for i := 0 to L.high do begin
        result := tpListOfIntegers(L.L[i]);
        if not result then Exit;
    end;
end;

function vpListOfByte                               (V: Tvalue): boolean;
begin
    result := tphListOf(V, vpIntegerByte);
end;

function vpListOfDurationsForMul(V: TValue): boolean;
var is_dt: boolean; L: TVList; i: integer;
begin
    result := V is TVList;
    if not result then Exit;
    L := V as TVList;
    is_dt := false;
    for i := 0 to L.high do begin
        result := (L.look[i] is TVDuration) or (L.look[i] is TVReal);
        if not result then Exit;
        result := not ((L.look[i] is TVDuration) and is_dt);
        if not result then Exit;
        is_dt := is_dt or (L.look[i] is TVDuration);
    end;
    result := is_dt;
end;

function vpListOfListsEqualLength(V: TValue): boolean;
var L: TVList; i, ln: integer;
begin
    result := V is TVList;
    if not result then exit;
    L := V as TVList;

    if L.Count=0 then Exit;

    result := tpList(L.look[0]);
    if not result then Exit;

    ln := L.L[0].Count;

    for i := 1 to L.high do begin
        result := tpList(L.look[i]) and (L.L[i].Count=ln);
        if not result then exit;
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

function vpListOfSummableTimes(V: TValue): boolean;
var is_dt: boolean; L: TVList; i: integer;
begin
    result := V is TVList;
    if not result then Exit;
    L := V as TVList;
    is_dt := false;
    for i := 0 to L.high do begin
        result := (L.look[i] is TVTime);
        if not result then Exit;
        result := not ((L.look[i] is TVDateTime) and is_dt);
        if not result then Exit;
        is_dt := is_dt or (L.look[i] is TVDateTime);
    end;
    result := is_dt;
end;


function vpListRoutineExpression(V: TValue): boolean;
var L: TVList;
begin
    result := V is TVList;
    if not result then Exit;
    L := V as TVList;
    result := (L.Count>=3) and (L.look[0] is TVOperator)
        and ((L.look[0] as TVOperator).op_enum in [oePROCEDURE, oeFUNCTION, oeMACRO])
        and tpOrdinarySymbol(L.look[1])
        and tpList(L.look[2]);
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

function vpListVariableExpression(V: TValue): boolean;
var L: TVList;
begin
    result := V is TVList;
    if not result then Exit;
    L := V as TVList;
    result := (L.Count>=2) and (L.look[0] is TVOperator)
        and ((L.look[0] as TVOperator).op_enum in [oeCONST, oeVAR, oeMACRO_SYMBOL])
        and tpOrdinarySymbol(L.look[1])
        and tpList(L.look[2]);
end;

function vpListWithLastList(V: TValue): boolean;
begin
    result := (V is TVList)
        and ((V as TVList).Count>0)
        and tpList((V as TVList).look[(V as TVList).high]);
end;


function vpNumberNotZero(V: TValue): boolean;
begin
    result := (V is TVNumber) and not ((V as TVNumber).C = _0);
end;

function vpOperatorVarDeclaration(V: TValue): boolean;
begin
    result := (V is TVOperator)
        and ((V as TVOperator).op_enum in
            [oeVAR, oeCONST, oePROCEDURE, oeMACRO, oeMACRO_SYMBOL, oeFUNCTION]);
end;

function vpOperatorVarOrConst(V: TValue): boolean;
begin
    result := (V is TVOperator) and ((V as TVOperator).op_enum in [oeVAR, oeCONST]);
end;

function vpOperatorRoutine(V: TValue): boolean;
begin
    result := (V is TVOperator)
        and ((V as TVOperator).op_enum in
            [oePROCEDURE, oeMACRO, oeMACRO_SYMBOL, oeFUNCTION]);
end;

function vpPairOfMatrixInteger(V: TValue): boolean;
var L: TVList;
begin
    result := V is TVList;
    if not result then Exit;
    L := V as TVList;
    result := L.Count=2;
    if not result then Exit;
    result := (L.look[0] is TVMatrixInteger) and (L.look[1] is TVMatrixInteger);
end;

function vpRangeNotNegative                         (V: TValue): boolean;
begin
    result := (V is TVRange) and ((V as TVRange).high>=(V as TVRange).low);
end;


function vpRealAbsOneOrMore                         (V: TValue): boolean;
begin
    result := (V is TVReal) and (abs((V as TVReal).F) >= 1);
end;

function vpRealNegative                             (V: TValue): boolean;
begin
    result := (V is TVReal) and ((V as TVReal).F < 0);
end;

function vpRealNotNegative                          (V: TValue): boolean;
begin
    result := (V is TVReal) and ((V as TVReal).F >= 0);
end;

function vpRealNotZero                              (V: TValue): boolean;
begin
    result := (V is TVReal) and ((V as TVReal).F <> 0);
end;

function vpRealPositive                             (V: TValue): boolean;
begin
    result := (V is TVReal) and ((V as TVReal).F > 0);
end;

function vpRealZero                                 (V: TValue): boolean;
begin
    result := (V is TVReal) and ((V as TVReal).F = 0);
end;

function vpSequenceNotEmpty(V: TValue): boolean;
begin
    result := (V is TVSequence) and ((V as TVSequence).Count>0);
end;

function vpSQLPointerActive                         (V: TValue): boolean;
begin
    result := (V is TVSQLPointer) and
        ((V as TVSQLPointer).body.V <> nil);
end;


function vpStreamEnd(V: TValue): boolean;
begin
    result :=
    (V as TVStreamPointer).body.Position = (V as TVStreamPointer).body.Size;
end;

function vpStreamPointerActive                      (V: TValue): boolean;
begin
    result := (V is TVStreamPointer) and ((V as TVStreamPointer).body<>nil);
end;


function vpStringEmpty                              (V: TValue): boolean;
begin
    result := (V is TVString) and ((V as TVString).S = '');
end;


function vpSymbol__                                 (V: TValue): boolean;
begin
    result := V.equal(_);
end;

function vpSymbol_IN                                (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'IN');
end;

function vpSymbol_LAST                              (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'LAST');
end;

function vpSymbol_LIST                              (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'LIST');
end;

function vpSymbol_OTHERWISE                         (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'OTHERWISE');
end;

function vpSymbol_RANGE                             (V: TValue): boolean;
begin
    result := vphSymbolName(V, 'RANGE');
end;

function vpSymbolQualified(V: TValue): boolean;
begin
    result := tpOrdinarySymbol(V) and (Pos(':', (V as TVSymbol).name)>1);
end;



end.  //955 1157

