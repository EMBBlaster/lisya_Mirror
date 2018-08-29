unit dlisp_read;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_values, mar, lisia_charset, lisya_exceptions,
    lisya_streams, lisya_string_predicates;



function read(s: TLStream): TValue; overload;
function read(sp: TVStreamPointer): TValue; overload;
function read_from_string(s: unicodestring): TValue;

procedure print(V:TValue; stream: TVStreamPointer; line_end: boolean = false);



implementation


const special_keywords: array[1..6] of unicodestring = (
    'ELSE', 'EXCEPTION', 'INSET', 'OTHERWISE', 'THEN', 'VALUE');

procedure raise_malformed(t: TStringList; i: integer; msg: unicodestring='');
var j, first, last: integer; s: unicodestring;
begin
    //добавляет в сообщение об ошибке конец списка токенов
    first := i-10;
    if first<0 then first := 0;
    if i<t.Count then last:=i else last:=t.Count-1;
    s:='';
    for j:=first to last do s := s + ' ' + t[j];
    raise ELE.Create(msg+s, 'syntax');
end;

function str_is_range(s: unicodestring; out l, h: Int64): boolean;
var p: integer;
begin
try
    result := false;
    p := PosU('..', s);
    if p>0 then begin
        l := StrToInt64(s[1..p-1]);
        h := StrToInt64(s[p+2..Length(s)]);
        result := true;
    end;
except
    on EConvertError do result := false;
end;
end;

function str_is_str(s: unicodestring; out ss: unicodestring): boolean;
var i: integer; escaped: boolean;
begin
    ss := '';
    escaped := false;
    result := (Length(s)>=2) and (s[1]='"') and (s[Length(s)]='"');
    if result then
        for i := 2 to length(s)-1 do
            case escaped of
                false: if s[i]='\' then escaped:=true else ss:=ss+s[i];
                true: begin escaped:=false; ss := ss+s[i]; end;
            end;
end;

function str_is_char(s: unicodestring; out ss: unicodestring): boolean;
begin
    result := sp_char(s);
    if result then ss := unicodechar(StrToInt(s[2..Length(s)]));
end;


function str_is_time(s: unicodestring; out dt: TDateTime): boolean;
var hours,p: integer; minutes, seconds, milliseconds: word;
begin
    result := sp_time(s);
    if not result then exit;
    p := pos(':',s);
    if s[1] in ['-','+']
    then hours := StrToInt(s[2..p-1])
    else hours := StrToInt(s[1..p-1]);
    minutes := StrToInt(s[p+1..p+2]);
    seconds := 0;
    milliseconds := 0;
    if sp_time_sec(s) or sp_time_msec(s) then seconds := StrToInt(s[p+4..p+5]);
    if sp_time_msec(s) then milliseconds := StrToInt(s[p+7..p+9]);
    dt := hours/24+minutes/(24*60)+seconds/(24*60*60)+milliseconds/(24*60*60*1000);
    if s[1]='-' then dt := -dt;
end;

function str_is_datetime(s: unicodestring; out dt: TDateTime): boolean;
var year, month, day: word;
    d: TDateTime;
begin try
    result := sp_date_time(s);
    if not result then exit;

    year := StrToInt(s[1..4]);
    month := StrToInt(s[6..7]);
    day := StrToInt(s[9..10]);
    dt := EncodeDate(year, month, day);

    if str_is_time(s[12..Length(s)],d) then dt := dt+d;
except
    on E:EConvertError do begin
        result := false;
        raise ELE.Create('invalid date-time '+s,'!'+E.ClassName+'!');
    end;
end;
end;

function str_is_elt_call(s: unicodestring; out elt: TStringList): boolean;
var i: integer; acc: unicodestring;
    state: (sNormal, sString, sEscaped);
    procedure add; begin if acc<>'' then begin elt.Add(acc); acc:=''; end; end;
begin
    result := (PosU('\', s)>1) or (PosU('/', s)>1);
    if not result then Exit;
try
    elt := TStringList.Create;
    elt.Add('(');
    elt.Add('ELT');
    acc := '';
    state := sNormal;

    for i := 1 to Length(s) do
        case state of
            sNormal: case s[i] of
                '"': begin acc:=acc+s[i]; state:=sString; end;
                '/': add;
                '\': begin add; acc:=' \'; add; end;
                else acc:=acc+s[i];
            end;
            sString: case s[i] of
                '\': begin acc:=acc+s[i]; state:=sEscaped; end;
                '"': begin acc:=acc+s[i]; state:=sNormal; end;
                else acc:=acc+s[i];
            end;
            sEscaped: begin acc:=acc+s[i]; state:=sString; end;
        end;

    if acc='' then raise ELE.Malformed('ELT '+s);
    add;
    elt.Add(')');
except
    FreeAndNil(elt);
    raise;
end;
end;

function str_is_integer(s: unicodestring; out i: Int64): boolean;
var ss: unicodestring; j: integer; m: Int64;
begin
    result := sp_integer(s);
    if not result then exit;

    ss := '';
    m := 1;
    for j:=1 to length(s) do case s[j] of
        '_':;
        'k','к': m := 1000;
        'M','М': m := 1000000;
        'G','Г': m := 1000000000;
        'T','Т': m := 1000000000000;
        else ss := ss+s[j];
    end;
    i := StrToInt64(ss)*m;
end;


function str_is_float(s: unicodestring; out f: double): boolean;
const ml: array[1..19] of record n: unicodestring; v: real; end = (
    (n:'п';  v:1e-12),  (n:'p'; v:1e-12),
    (n:'н';  v:1e-9),   (n:'n'; v:1e-9),
    (n:'мк'; v:1e-6),   (n:'u'; v:1e-6),
    (n:'м';  v:1e-3),   (n:'m'; v:1e-3),
    (n:'к';  v:1e3),    (n:'k'; v:1e3),
    (n:'М';  v:1e6),    (n:'M'; v:1e6),
    (n:'Г';  v:1e9),    (n:'G'; v:1e9),
    (n:'Т';  v:1e12),   (n:'T'; v:1e12),
    (n:'гр'; v:pi/180),  (n:'deg'; v:pi/180), (n:'°'; v:pi/180));
var i: integer; fs: TFormatSettings; m: double; ss: unicodestring;
    function str_at_end(ss: unicodestring): boolean;
    begin
        result := (Length(s)>=Length(ss)) and (ss=s[Length(s)-Length(ss)+1..Length(s)]);
    end;
begin
    result := sp_float(s);
    if not result then Exit;

    fs.DecimalSeparator:='.';
    m := 1;
    ss := '';
    for i:=1 to length(s) do case s[i] of
        '_':;
        ',','.': ss := ss+'.';
        '0','1','2','3','4','5','6','7','8','9','E','e','-','+': ss := ss+s[i];
        else break;
    end;

    for i := low(ml) to high(ml) do
        if str_at_end(ml[i].n) then begin
            m := ml[i].v;
            Break;
        end;

    f := StrToFloat(ss,fs)*m;
end;


function str_is_complex(s: unicodestring; out re, im: double): boolean;
var fs: TFormatSettings; i_pos: integer;
    re_s, im_s: unicodestring;
    sign: unicodechar;
begin
    result := sp_complex_alg(s);
    if not result then Exit;

    i_pos := PosU('i', s);
    if i_pos=0 then i_pos := PosU('м', s);
    if i_pos=0 then exit;

    if i_pos>2 then re_s := s[1..i_pos-2] else re_s :='0';
    im_s := s[i_pos+1..Length(s)];

    if i_pos>1 then sign := s[i_pos-1] else sign := '+';

    if not (str_is_float(re_s, re) and str_is_float(im_s, im)) then exit;

    if sign='-' then im := - im;
end;

function str_is_keyword(s: unicodestring): boolean;
var i: integer; us: unicodestring;
begin
    result := true;
    if s[1]=':' then exit;

    us := UnicodeUpperCase(s);
    for i := 1 to high(special_keywords) do
        if us = special_keywords[i] then Exit;

    result := false;
end;


const screen_width = 50;
var ind: integer = 0;

procedure print(V:TValue; stream: TVStreamPointer; line_end: boolean);
var  i: integer; sn: unicodestring;
    procedure wr(s: unicodestring);
    begin
        if stream<>nil then stream.body.write_string(s) else System.Write(s);
    end;
    procedure indent;
    var i: integer;
    begin for i:=1 to ind do wr(' '); end;
begin try

    //TODO: нужен специальный код для печати списков ключ-значение, чтобы пара помещалась на одной строке
    if (V is TVList) then begin
        if (V as TVList).count=0
        then wr('NIL')
        else begin
            if (Length(V.AsString)+ind*3)<=screen_width
            then wr(V.AsString)
            else begin
                Inc(ind,2);
                wr('( ');
                print((V as TVList).look[0], stream);
                for i := 1 to (V as TVList).high do begin
                    wr(LineEnding);
                    indent;
                    print((V as TVList).look[i], stream);
                end;
                wr(')');
                Dec(ind,2);
            end;
        end;
        exit;
    end;

    if (V is TVRecord) then begin
        if ((Length(V.AsString)+ind*3)<=screen_width)
            or ((V as TVRecord).count=0)
        then wr(V.AsString)
        else begin
            Inc(ind,3);
            wr('#R(');
            sn := (V as TVRecord).name_n(0)+' ';
            wr(sn);
            Inc(ind, Length(sn));
            print((V as TVRecord).look[0], stream);
            Dec(ind, length(sn));
            for i := 1 to (V as TVRecord).count-1 do begin
                wr(LineEnding);
                indent;
                sn := (V as TVRecord).name_n(i)+' ';
                wr(sn);
                inc(ind, Length(sn));
                print((V as TVRecord).look[i], stream);
                dec(ind, Length(sn));
            end;
            wr(')');
            Dec(ind,3);
        end;
        exit;
    end;

    wr(V.asString);
    exit;

    if not (V is TVList)
    then wr(V.AsString())
    else
        if (V as TVList).count=0
        then wr('NIL')
        else
            if (Length((V as TVList).AsString())+ind*2)<=screen_width
            then wr((V as TVList).AsString())
            else begin
                Inc(ind);
                wr('(  ');
                print((V as TVList).look[0], stream);
                for i:=1 to (V as TVList).count-1 do begin
                    wr(LineEnding);
                    indent;
                    print((V as TVList).look[i], stream);
                end;
                wr(')');
                Dec(ind);
            end;

finally
    if line_end then wr(LineEnding);
end;

end;


function read_tokens(s: TLStream): TStringList;
var ch: unicodechar; depth: integer; state: (sToken, sString, sQuoted, sComment);
    acc: unicodestring;
    procedure add;
    begin
        if acc='' then Exit;
        while (Length(acc)>1) and (acc[1] in ['/','\','@']) do begin
            result.Add(' '+acc[1]);
            acc := acc[2..Length(acc)];
        end;
        result.Add(acc);
        acc := '';
    end;
begin try
    result := TStringList.Create;
    acc := '';
    depth := 0;
    state := sToken;
    while s.read_character(ch) do begin
        case state of
            sToken: case ch of
                ' ',#13,#10,#$FEFF,#9: begin add; end;
                '(': begin acc:=acc+ch; add; inc(depth); end;
                ')': begin add; acc:=acc+ch; add; dec(depth);end;
                '"': begin {add;} acc:=acc+ch; state:= sString; end;
                '''': begin add; acc:=acc+ch; state:=sQuoted; end;
                ';': begin add; state:=sComment; end;
                else acc:=acc+ch;
            end;
            sString: case ch of
                '"': begin acc:=acc+ch; {add;} state:=sToken; end;
                '\': begin acc:=acc+'\'+s.read_character; end;
                else acc:=acc+ch;
            end;
            sQuoted: case ch of
                '''': begin acc:=acc+ch; add; state:=sToken; end;
                '\': begin acc:=acc+s.read_character; end;
                else acc:=acc+ch;
            end;
            sComment: case ch of
                #13,#10: state:=sToken;
            end;
        end;
        if (state=sToken) and (depth=0) and (result.Count>0) then break;
    end;
    add;
    if state=sString then raise_malformed(result, result.Count, 'unmatched quotes');
    if state=sQuoted then raise_malformed(result, result.Count, 'unmatched quotes');
    if depth<>0 then raise_malformed(result, result.Count, 'unmatched parenthesis');
except
    FreeAndNil(result);
    raise;
end;
end;


function closing_parenthesis(t: TStringList; i: integer): boolean;
begin
    if i>=t.Count
    then raise_malformed(t,i, 'unmatched parenthesis')
    else result := t[i]=')';
end;

function s_expr(t: TStringList; var i: integer): TValue;
var slot: TValue; dt: TDateTime; re, im: double; l,h: Int64; str: unicodestring;
    elt: TStringList; j: integer;
begin
    if i>=t.Count then raise_malformed(t,i,'malformed expression');

    if str_is_str(t[i], str)
    then result := TVString.Create(str)
    else

    if str_is_char(t[i], str)
    then result := TVString.Create(str)
    else

    if t[i]='('
    then begin
        result := TVList.Create;
        Inc(i);
        while not closing_parenthesis(t,i) do (result as TVList).Add(s_expr(t, i));
    end
    else

    if UnicodeUpperCase(t[i])='#R('
    then begin
        result := TVRecord.Create;
        Inc(i);
        slot := nil;
        while not closing_parenthesis(t,i) do try
            slot := s_expr(t, i);
            if not (slot is TVSymbol)
            then raise ELE.Create('invalid slot name '+slot.AsString,'syntax');
            if closing_parenthesis(t,i) then raise_malformed(t,i, 'malformed record');
            (result as TVRecord).AddSlot(slot as TVSymbol, s_expr(t,i));
        finally
            slot.Free;
        end;
    end
    else

    if t[i]=' \'
    then begin
        Inc(i);
        result := TVList.Create([TVSymbol.Create('QUOTE'), s_expr(t,i)]);
        Dec(i);
    end
    else

    if t[i]=' /'
    then begin
        Inc(i);
        result := TVList.Create([TVSymbol.Create('VALUE'), s_expr(t,i)]);
        Dec(i);
    end
    else

    if t[i]=' @'
    then begin
        Inc(i);
        result := TVList.Create([TVSymbol.Create('INSET'), s_expr(t,i)]);
        Dec(i);
    end
    else

    if str_is_datetime(t[i], dt)
    then result := TVDateTime.Create(dt)
    else

    if str_is_time(t[i], dt)
    then result := TVDuration.Create(dt)
    else

    if UnicodeUpperCase(t[i])='NIL'
    then result := TVList.Create
    else

    if UnicodeUpperCase(t[i])='T'
    then result := TVT.Create
    else

    if str_is_integer(t[i], l)
    then result := TVInteger.Create(l)
    else

    if str_is_float(t[i], re)
    then result := TVFloat.Create(re)
    else

    if str_is_complex(t[i], re, im)
    then result := TVComplex.Create(re, im)
    else

    if str_is_range(t[i], l, h)
    then result := TVRange.Create(l,h)
    else

    if str_is_elt_call(t[i], elt)
    then try
        //for j:=0 to elt.Count-1 do WriteLn(elt[j]);
        j := 0;
        result := s_expr(elt, j);
    finally
        elt.Free;
    end
    else

    if str_is_keyword(t[i])
    then result := TVKeyword.Create(t[i])
    else

    if t[i]=')'
    then raise_malformed(t,i, 'unmatched parenthesis')
    else

    result := TVSymbol.Create(t[i]);

    Inc(i);
end;


function read(s: TLStream): TValue;
var tokens: TStringList; i: integer;
begin try
    tokens := nil;
    tokens := read_tokens(s);
    //for i:=0 to tokens.count-1 do WriteLn(tokens[i]);
    i := 0;
    if tokens.Count>0
    then result := s_expr(tokens, i)
    else result := TVEndOfStream.Create;
finally
    tokens.Free;
end;
end;


function read(sp: TVStreamPointer): TValue;
begin
    //result := read(sp.stream.fstream, sp.stream.encoding);
  result := read(sp.body);
end;

function read_from_string(s: unicodestring): TValue;
var stream: TLMemoryStream;
begin try
    result := nil;
    stream := TLMemoryStream.Create(s);
    result := read(stream);
finally
    stream.Free;
end;
end;

end. //514
