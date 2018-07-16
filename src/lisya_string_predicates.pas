unit lisya_string_predicates;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    regexpr in './fpc_backport/regexpr.pas',
    Classes, SysUtils;

function sp_integer(s: unicodestring): boolean;
function sp_float(s: unicodestring): boolean;
function sp_char(s: unicodestring): boolean;

implementation

var re_integer, re_float, re_char: TRegExpr;

function sp_integer(s: unicodestring): boolean;
begin
    result := re_integer.Exec(s);
end;

function sp_float(s: unicodestring): boolean;
begin
    result := re_float.Exec(s);
end;

function sp_char(s: unicodestring): boolean;
begin
    result := re_char.Exec(s);
end;

initialization
    re_integer := TRegExpr.Create(
    '^((\$|0x)(?i)[0-9A-F]+(_[0-9A-F]+)*(?-i)|[-+]?[0-9]+(_[0-9]+)*[кkМMГGТT]?)$');
    re_integer.Compile;
    re_float := TRegExpr.Create(
    '^[-+]?[0-9]+(_[0-9]+)*([.,][0-9]+(_[0-9]+)*)?([eE][-+]?[0-9]+|([пpнnuмmкkМMГGТT°]|мк|гр|deg))?$');
    re_float.Compile;
    re_char := TRegExpr.Create(
    '^\#([0-9]+|\$(?i)[0-9A-F]+)$');
    re_char.Compile;

finalization
    re_integer.Free;
    re_float.Free;
    re_char.Free;

end.

