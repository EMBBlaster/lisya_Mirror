﻿unit mar;

{$mode delphi}

interface

uses
    Classes, SysUtils;

function PosU(const ss, s: unicodestring; offset: integer = 1): integer;
function UpperCaseU(s: unicodestring): unicodestring;
function LowerCaseU(s: unicodestring): unicodestring;

implementation

function PosU(const ss, s: unicodestring; offset: integer = 1): integer;
var i, j: integer;
begin
    for i:=offset to length(s)-length(ss)+1 do begin
        result := i;
        for j := 1 to length(ss) do
            if ss[j]<>s[i+j-1] then begin
                result := 0;
                break;
            end;
        if result=i then exit;
    end;
    result := 0;
    //TODO: PosU возможно нужно переделать на CompareByte аналогично PosEx
end;

function UpperCaseU(s: unicodestring): unicodestring;
var i: integer;
begin
    result := UpperCase(s);
    for i := 1 to Length(result) do
        case result[i] of
            'а': result[i] := 'А';
            'б': result[i] := 'Б';
            'в': result[i] := 'В';
            'г': result[i] := 'Г';
            'д': result[i] := 'Д';
            'е': result[i] := 'Е';
            'ё': result[i] := 'Ё';
            'ж': result[i] := 'Ж';
            'з': result[i] := 'З';
            'и': result[i] := 'И';
            'к': result[i] := 'К';
            'л': result[i] := 'Л';
            'м': result[i] := 'М';
            'н': result[i] := 'Н';
            'о': result[i] := 'О';
            'п': result[i] := 'П';
            'р': result[i] := 'Р';
            'с': result[i] := 'С';
            'т': result[i] := 'Т';
            'у': result[i] := 'У';
            'ф': result[i] := 'Ф';
            'х': result[i] := 'Х';
            'ц': result[i] := 'Ц';
            'ч': result[i] := 'Ч';
            'ш': result[i] := 'Ш';
            'щ': result[i] := 'Щ';
            'ъ': result[i] := 'Ъ';
            'ы': result[i] := 'Ы';
            'ь': result[i] := 'Ь';
            'э': result[i] := 'Э';
            'ю': result[i] := 'Ю';
            'я': result[i] := 'Я';
        end;
end;

function LowerCaseU(s: unicodestring): unicodestring;
var i: integer;
begin
    result := UpperCase(s);
    for i := 1 to Length(result) do
        case result[i] of
            'А': result[i] := 'а';
            'Б': result[i] := 'б';
            'В': result[i] := 'в';
            'Г': result[i] := 'г';
            'Д': result[i] := 'д';
            'Е': result[i] := 'е';
            'Ё': result[i] := 'ё';
            'Ж': result[i] := 'ж';
            'З': result[i] := 'з';
            'И': result[i] := 'и';
            'К': result[i] := 'к';
            'Л': result[i] := 'л';
            'М': result[i] := 'м';
            'Н': result[i] := 'н';
            'О': result[i] := 'о';
            'П': result[i] := 'п';
            'Р': result[i] := 'р';
            'С': result[i] := 'с';
            'Т': result[i] := 'т';
            'У': result[i] := 'у';
            'Ф': result[i] := 'ф';
            'Х': result[i] := 'х';
            'Ц': result[i] := 'ц';
            'Ч': result[i] := 'ч';
            'Ш': result[i] := 'ш';
            'Щ': result[i] := 'щ';
            'Ъ': result[i] := 'ъ';
            'Ы': result[i] := 'ы';
            'Ь': result[i] := 'ь';
            'Э': result[i] := 'э';
            'Ю': result[i] := 'ю';
            'Я': result[i] := 'я';
        end;
end;

end.

