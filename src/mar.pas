unit mar;

{$mode delphi}

interface

uses
    Classes, SysUtils;

type
    TStringArray = array of unicodestring;
    TIntegers = array of integer;

function PosU(const ss, s: unicodestring; offset: integer = 1): integer;
function SplitString(S: unicodestring; separator: unicodestring = ' '): TStringArray;
function StringSubstitute(const src, a, b: unicodestring): unicodestring;

function PointerToStr(ptr: Pointer): unicodestring;
function DirSep(s: unicodestring): unicodestring;

function concat_integers(i1, i2: TIntegers): TIntegers;
procedure append_integers(var i1: TIntegers; i2: TIntegers);
procedure append_integer(var i1: TIntegers; i: integer);

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

function StringSubstitute(const src, a, b: unicodestring): unicodestring;
var p1, p2: integer;
begin
    result := '';
    p1 := 1;
    p2 := PosU(a, src);
    while p2>0 do begin
        result := result + src[p1..p2-1] + b;
        p1 := p2 + Length(a);
        p2 := PosU(a,src, p1);
    end;
    result := result + src[p1..Length(src)];
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

function concat_integers(i1, i2: TIntegers): TIntegers;
var i: integer;
begin
    result := nil;
    SetLength(result, Length(i1)+Length(i2));
    for i := 0 to high(i1) do result[i] := i1[i];
    for i := 0 to high(i2) do result[i+Length(i1)] := i2[i];
end;

procedure append_integers(var i1: TIntegers; i2: TIntegers);
var i, b: integer;
begin
    b := Length(i1);
    SetLength(i1, Length(i1)+Length(i2));
    for i:=0 to high(i2) do i1[b+i] := i2[i];
end;

procedure append_integer(var i1: TIntegers; i: integer);
begin
    SetLength(i1, Length(i1)+1);
    i1[high(i1)] := i;
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


end.  //195

