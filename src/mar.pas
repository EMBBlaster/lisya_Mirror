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

function PointerToStr(ptr: Pointer; digits: integer = -1): unicodestring;
function PointerToQWORD(ptr: Pointer): QWORD;
function DirSep(s: unicodestring): unicodestring;

function concat_integers(i1, i2: TIntegers): TIntegers;
procedure append_integers(var i1: TIntegers; i2: TIntegers); inline;
procedure append_integer(var i1: TIntegers; i: integer); inline;

type

    { TCountingObject }

    TCountingObject = class
    private
        ref_count: integer;
        CS: TRTLCriticalSection;
        res_cs: TRTLCriticalSection;
    public
        constructor Create;
        function Ref: TCountingObject;
        function Release: boolean;
        function description: unicodestring; virtual; abstract;
        property refs: integer read ref_count;
        procedure Lock; virtual;
        procedure Unlock; virtual;
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
        p2 := PosU(separator, S, p1);
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

function PointerToStr(ptr: Pointer; digits: integer): unicodestring;
var d: record case byte of 0: (p: pointer); 1: (b: array [1..SizeOf(ptr)] of Byte); end;
    i, _digits: integer;
begin
    d.p:=ptr;
    result := '';
    for i := low(d.b) to high(d.b) do begin
        result := IntToHex(d.b[i],2)+result;
        if i=4 then result := '_'+result;
    end;

    if (digits>0) and (digits<length(result))
    then result := result[Length(result)-digits+1..Length(result)];
end;

function PointerToQWORD(ptr: Pointer): QWORD;
var d: record case byte of 0: (p: pointer); 1: (c: QWORD); end;
    i: integer;
begin
    d.p:=ptr;
    result := d.c;
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
    InitCriticalSection(cs);
    InitCriticalSection(res_cs);
end;

function TCountingObject.Ref: TCountingObject;
begin
    EnterCriticalSection(cs);
    Inc(ref_count);
    LeaveCriticalSection(cs);
    result := self;
end;

function TCountingObject.Release: boolean;
var rel: boolean;
begin
    EnterCriticalSection(cs);
    Dec(ref_count);
    rel := ref_count=0;
    LeaveCriticalSection(cs);

    if rel then begin
        DoneCriticalSection(cs);
        DoneCriticalSection(res_cs);
        result := true;
        self.Free;
    end
    else result := false;
end;

procedure TCountingObject.Lock;
begin
    EnterCriticalSection(res_cs);
end;

procedure TCountingObject.Unlock;
begin
    LeaveCriticalSection(res_cs);
end;


end.  //195

