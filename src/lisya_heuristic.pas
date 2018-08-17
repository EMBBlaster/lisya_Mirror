unit lisya_heuristic;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils;

function levenshtein(s1, s2: unicodestring): integer;


implementation

type TWordVec = array[0..2345] of byte;


function num(a: unicodechar): integer;
begin
    case a of
        ' ': result := 0;
        'а': result := 1;
        'б': result := 2;
        'в': result := 3;
        'г': result := 4;
        'д': result := 5;
        'е': result := 6;
        'ё': result := 7;
        'ж': result := 8;
        'з': result := 9;
        'и': result := 10;
        'й': result := 11;
        'к': result := 12;
        'л': result := 13;
        'м': result := 14;
        'н': result := 15;
        'о': result := 16;
        'п': result := 17;
        'р': result := 18;
        'с': result := 19;
        'т': result := 20;
        'у': result := 21;
        'ф': result := 22;
        'х': result := 23;
        'ц': result := 24;
        'ч': result := 25;
        'ш': result := 26;
        'щ': result := 27;
        'ъ': result := 28;
        'ы': result := 29;
        'ь': result := 30;
        'э': result := 31;
        'ю': result := 32;
        'я': result := 33;
    end;
end;

function to_vec(s: unicodestring): TWordVec;
var i: integer;
begin
    for i := low(result) to high(result) do result[i] := 0;
    for i := 1 to length(s) do inc(result[num(s[i])]);
    for i := 1 to length(s)-1 do inc(result[34+34*num(s[i])+num(s[i+1])]);
    for i := 1 to length(s)-2 do inc(result[34+34*34+34*num(s[i])+num(s[i+2])]);
end;

function vec_dist(const v1, v2: TWordVec): integer;
var i: integer;
begin
    result := 0;
    for i := low(TWordVec) to high(TWordVec) do
        result := result + abs(integer(v1[i])-integer(v2[i]));
end;

function normalize(s: unicodestring): unicodestring;
    procedure res(s: unicodestring); inline;
    begin
        if (s=' ') and (result[Length(result)]=' ')
        then
        else result := result + s;
    end;
var i: integer;
begin
    result := ' ';
    for i := 1 to Length(s) do
        case s[i] of
            'A','a': res('а');
            'B','b': res('б');
            'C','c': res('ц');
            'D','d': res('д');
            'E','e': res('е');
            'F','f': res('ф');
            'G','g': res('г');
            'H','h': res('ч');
            'I','i': res('и');
            'J','j': res('й');
            'K','k': res('к');
            'L','l': res('л');
            'M','m': res('л');
            'N','n': res('н');
            'O','o': res('о');
            'P','p': res('п');
            'Q','q': res('ку');
            'R','r': res('р');
            'S','s': res('с');
            'T','t': res('т');
            'U','u': res('ю');
            'V','v': res('в');
            'W','w': res('дв');
            'X','x': res('кс');
            'Y','y': res('у');
            'Z','z': res('з');
            'А','а': res('а');
            'Б','б': res('б');
            'В','в': res('в');
            'Г','г': res('г');
            'Д','д': res('д');
            'Е','е': res('е');
            'Ё','ё': res('ё');
            'Ж','ж': res('ж');
            'З','з': res('з');
            'И','и': res('и');
            'Й','й': res('й');
            'К','к': res('к');
            'Л','л': res('л');
            'М','м': res('м');
            'Н','н': res('н');
            'О','о': res('о');
            'П','п': res('п');
            'Р','р': res('р');
            'С','с': res('с');
            'Т','т': res('т');
            'У','у': res('у');
            'Ф','ф': res('ф');
            'Х','х': res('х');
            'Ц','ц': res('ц');
            'Ч','ч': res('ч');
            'Ш','ш': res('ш');
            'Щ','щ': res('щ');
            'Ъ','ъ': res('ъ');
            'Ы','ы': res('ы');
            'Ь','ь': res('ь');
            'Э','э': res('э');
            'Ю','ю': res('ю');
            'Я','я': res('я');
            '1': res('один');
            '2': res('два');
            '3': res('три');
            '4': res('четыре');
            '5': res('пять');
            '6': res('шесть');
            '7': res('семь');
            '8': res('восемь');
            '9': res('девять');
            '0': res('ноль');
            else res(' ');
        end;
end;


function levenshtein(s1, s2: unicodestring): integer;
var v1, v2: TWordVec;
begin
    //WriteLn(normalize(s1));
    //WriteLn(normalize(s2));
    v1 := to_vec(normalize(s1));
    v2 := to_vec(normalize(s2));
    result := vec_dist(v1, v2);
end;


end.

