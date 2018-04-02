unit mar;

{$mode delphi}

interface

uses
    Classes, SysUtils;

type
    TStringArray = array of unicodestring;

function PosU(const ss, s: unicodestring; offset: integer = 1): integer;
function UpperCaseU(s: unicodestring): unicodestring;
function LowerCaseU(s: unicodestring): unicodestring;
function SplitString(S: unicodestring; separator: unicodestring = ' '): TStringArray;

function PointerToStr(ptr: Pointer): unicodestring;
function DirSep(s: unicodestring): unicodestring;

type

    { TCountingObject }

    TCountingObject = class
    private
        ref_count: integer;
    public
        constructor Create;
        function Ref: TCountingObject;
        function Release: boolean;
        function description: unicodestring; virtual; abstract;
        property refs: integer read ref_count;
    end;


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
    result := LowerCase(s);
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

function SplitString(S: unicodestring; separator: unicodestring = ' '): TStringArray;
var p1, p2: integer;
begin
    SetLength(result, 0);
    p1 := 1;
    p2 := PosU(separator, S);
    while p2>0 do begin
        SetLength(result, Length(result)+1);
        result[high(result)] := S[p1..p2-1];
        p1 := p2+Length(separator);
        p2 := PosU(separator, S, p1+1);
    end;
    SetLength(result, Length(result)+1);
    result[high(result)] := S[p1..Length(s)];
end;

function PointerToStr(ptr: Pointer): unicodestring;
var d: record case byte of 0: (p: pointer); 1: (b: array [1..SizeOf(ptr)] of Byte); end;
    i: integer;
begin
    d.p:=ptr;
    result := '';
    for i := low(d.b) to high(d.b) do result := IntToHex(d.b[i],2)+result;
end;

function DirSep(s: unicodestring): unicodestring;
var buf: string;
begin
    buf := s;
    DoDirSeparators(buf);
    result := buf;
end;


    { TCountingObject }

constructor TCountingObject.Create;
begin
    ref_count := 1;
end;

function TCountingObject.Ref: TCountingObject;
begin
    Inc(ref_count);
    result := self;
end;

function TCountingObject.Release: boolean;
begin
    Dec(ref_count);
    if ref_count=0 then begin
        result := true;
        self.Free;
    end
    else result := false;
end;

end.

