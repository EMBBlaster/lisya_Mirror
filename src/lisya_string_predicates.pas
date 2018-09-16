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
function sp_complex_alg(s: unicodestring): boolean;
function sp_complex_exp(s: unicodestring): boolean;
function sp_char(s: unicodestring): boolean;
function sp_time(s: unicodestring): boolean;
function sp_time_min(s: unicodestring): boolean;
function sp_time_sec(s: unicodestring): boolean;
function sp_time_msec(s: unicodestring): boolean;
function sp_date_time(s: unicodestring): boolean;

implementation

var re_integer, re_float, re_complex_alg, re_complex_exp, re_char,
    re_time, re_time_min, re_time_sec, re_time_msec
    , re_date_time: TRegExpr;

function sp_integer(s: unicodestring): boolean;
begin
    result := re_integer.Exec(s);
end;

function sp_float(s: unicodestring): boolean;
begin
    result := re_float.Exec(s);
end;

function sp_complex_alg(s: unicodestring): boolean;
begin
    result := re_complex_alg.Exec(s);
end;

function sp_complex_exp(s: unicodestring): boolean;
begin
    result := re_complex_exp.Exec(s);
end;

function sp_char(s: unicodestring): boolean;
begin
    result := re_char.Exec(s);
end;

function sp_time(s: unicodestring): boolean;
begin
    result := re_time.Exec(s);
end;

function sp_time_min(s: unicodestring): boolean;
begin
    result := re_time_min.Exec(s);
end;

function sp_time_sec(s: unicodestring): boolean;
begin
    result := re_time_sec.Exec(s);
end;

function sp_time_msec(s: unicodestring): boolean;
begin
    result := re_time_msec.Exec(s);
end;

function sp_date_time(s: unicodestring): boolean;
begin
    result := re_date_time.Exec(s);
end;

const
    expr_time = '[0-9]+:[0-5][0-9](:[0-5][0-9](\.[0-9][0-9][0-9])?)?';
    expr_nums = '[0-9]+(_[0-9]+)*';
    expr_float = expr_nums+'([.,]'+expr_nums+')?';
    expr_exp = expr_float+'([eE][-+]?[0-9]+|([пpнnuмmкkМMГGТT]|мк))?';

initialization
    re_integer := TRegExpr.Create(
    '^((\$|0x)(?i)[0-9A-F]+(_[0-9A-F]+)*(?-i)|[-+]?[0-9]+(_[0-9]+)*[кkМMГGТT]?)$');
    re_integer.Compile;

    re_float := TRegExpr.Create(
    '^[-+]?[0-9]+(_[0-9]+)*([.,][0-9]+(_[0-9]+)*)?([eE][-+]?[0-9]+|([пpнnuмmкkМMГGТT°π]|мк|гр|deg|pi))?$');
    re_float.Compile;

    re_complex_alg := TRegExpr.Create('^[-+]?'+expr_exp+'[-+][iм]'+expr_exp+'$');
    re_complex_alg.Compile;

    re_complex_exp := TRegExpr.Create('^[-+]?'+expr_exp+'[aу][-+]?'+expr_float+'(°|гр|deg)$');
    re_complex_exp.Compile;

    re_char := TRegExpr.Create('^\#([0-9]+|\$(?i)[0-9A-F]+)$');
    re_char.Compile;

    re_time := TRegExpr.Create('^[-+]?'+expr_time+'$');
    re_time.Compile;

    re_time_min := TRegExpr.Create('^[-+]?[0-9]+:[0-5][0-9]$');
    re_time_min.Compile;

    re_time_sec := TRegExpr.Create('^[-+]?[0-9]+:[0-5][0-9]:[0-5][0-9]$');
    re_time_sec.Compile;

    re_time_msec := TRegExpr.Create('^[-+]?[0-9]+:[0-5][0-9]:[0-5][0-9]\.[0-9][0-9][0-9]$');
    re_time_msec.Compile;

    re_date_time := TRegExpr.Create(
    '^[1-3][0-9][0-9][0-9][-.](01|02|03|04|05|06|07|08|09|10|11|12)[-.][0-3][0-9]([-_/]'+expr_time+')?$');
    re_date_time.Compile;


finalization
    re_integer.Free;
    re_float.Free;
    re_complex_alg.Free;
    re_complex_exp.Free;
    re_char.Free;
    re_time.Free;
    re_time_min.Free;
    re_time_sec.Free;
    re_time_msec.Free;
    re_date_time.Free;

end.

