unit lisya_symbols;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils;

function symbol_n(name: unicodestring): Int64;
function symbol_uname(n: Int64): unicodestring; inline;
function gensym_n: Int64;

implementation

var g_n: Int64 = -1;
var cs: TRTLCriticalSection;
var names: array of unicodestring;

function symbol_n(name: unicodestring): Int64;
var uname: unicodestring; res: integer;
begin
    uname := UnicodeUpperCase(name);
    EnterCriticalSection(cs);
    try
        for res := 0 to high(names) do if names[res]=uname then Exit;
        SetLength(names, Length(names)+1);
        names[high(names)] := uname;
        res := high(names);
    finally
        LeaveCriticalSection(cs);
        result := res;
    end;
end;


function symbol_uname(n: Int64): unicodestring;
begin
    if n<0
    then result := '#G'+IntToStr(n)
    else try
        EnterCriticalSection(cs);
        result := names[n];
    finally
        LeaveCriticalSection(cs);
    end;
end;


function gensym_n: Int64;
begin
    EnterCriticalSection(cs);
    result := g_n;
    Dec(g_n);
    LeaveCriticalSection(cs);
end;



initialization
    InitCriticalSection(cs);

finalization
    DoneCriticalSection(cs);

end.

