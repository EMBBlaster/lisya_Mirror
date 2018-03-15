unit lisya_xml;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_values
    //, strutils
    , mar
    ,lisya_predicates
    ,lisia_charset
    ;

function xml_read_from_string(s: unicodestring): TValue;
function xml_read_from_string_l(s: unicodestring): TVList;
function xml_read_from_string_sl(s: unicodestring): TVList;

function xml_read(s: TStream): TVList;
procedure xml_write(s: TStream; xml: TVList);

function xml_write_to_string(xml: TVList): unicodestring;


implementation

function decode(s: unicodestring): unicodestring;
var i, p: integer;
    function decode_symbol(s: unicodestring): unicodestring;
    begin
        if s='amp'  then result := '&' else
        if s='gt'   then result := '>' else
        if s='lt'   then result := '<' else
        if s='quot' then result := '"' else

        if s[1]='#' then result := unicodechar(StrToInt(s[2..Length(s)])) else
        result := s;
    end;
begin
    SetLength(result, Length(s));
    SetLength(result, 0);
    i := 0;
    while i<Length(s) do begin
        Inc(i);
        if s[i]='&' then begin
            p := PosU(';', s, i);
            if abs(p-i)>6 then raise ELE.Create('invalid symbol '+s[i..i+4],'xml');
            result := result + decode_symbol(s[i+1..p-1]);
            i := p;
        end
        else result := result + s[i];

    end;
end;

function encode(s: unicodestring): unicodestring;
var i: integer;
begin
    result := '';
    for i := 1 to length(s) do
        case s[i] of
            '&': result := result + '&amp;';
            '>': result := result + '&gt;';
            '<': result := result + '&lt;';
            else result := result + s[i];
        end;
end;

function word_start(const s: unicodestring; start: integer): integer;
var i: integer;
begin
    for i := start downto 1 do
        if s[i] in [' ', #8, #13, #10] then begin
            result := i+1;
            exit;
        end;
    result := i
end;

function word_end(const s: unicodestring; start: integer): integer;
var i: integer; p: boolean;
begin
    p := false;
    for i := start to Length(s) do begin
        if s[i]='"' then p := not p;
        if s[i]='''' then p := not p;
        if (s[i] in [' ', #8, #13, #10]) and not p then begin
            result := i-1;
            exit;
        end;
    end;
    result := i
end;

function decode_attributes(s: unicodestring): TVList;
var ep, ws, we: integer;
begin
    result := TVList.Create;
    ep := PosU('=', s);
    while ep>0 do begin
        ws := word_start(s, ep);
        we := word_end(s, ep);
        result.Add(TVSymbol.Create(s[ws..ep-1]));
        result.add(TVString.Create(decode(s[ep+2..we-1])));
        ep := PosU('=', s, ep+1);
    end;
end;


const sNAME = 0; sATTR = 1; sCHILD = 2;

function decode_tag(S: unicodestring): TVRecord;
var word_end: integer; closed: boolean;
begin
    // Разбирает переданную строку вида <tag key="val"> и создаёт на её основе
    //структуру, представляющую элемент XML включая теги, но без дочерних
    //элементов.
    // Если тэг закрытый, то список элементов = T, если нет, то NIL
    result := TVRecord.Create(['NAME', 'ATTRIBUTES', 'CHILDREN']);
    if (Length(S)>=7) and (S[1..5]='<?xml') and (S[length(s)-1..Length(s)]='?>')
    then begin
        (result as TVRecord)[sNAME] := TVSymbol.Create(':PROLOGUE');
        (result as TVRecord)[sCHILD] := TVT.Create;
        (result as TVRecord)[sATTR] := decode_attributes(s[6..Length(s)-2]);
        Exit;
    end;


    if (S[1]='<') and (S[Length(S)]='>') then begin
        word_end := PosU(' ', S) - 1;
        if word_end<0 then word_end := PosU('/', S) - 1;
        if word_end<0 then word_end := Length(S) - 1;
        (result as TVRecord)[sNAME] := TVString.Create(decode(S[2..word_end]));
        closed := PosU('/', S)>0;
        if closed then begin
            (result as TVRecord)[sCHILD] := TVT.Create;
            (result as TVRecord)[sATTR] := decode_attributes(
                                            s[word_end+1..Length(s)-2]);
        end
        else (result as TVRecord)[sATTR] := decode_attributes(
                                            s[word_end+1..Length(s)-1]);
        exit;
    end;
    (result as TVRecord)[sNAME] := TVSymbol.Create(':TEXT');
    (result as TVRecord)[sNAME] := TVString.Create(decode(S));
end;

function decode_tag_l(S: unicodestring; out closed: boolean): TValue;
var word_end: integer; attr_str: unicodestring;
begin
    //WriteLn(s);
    // Разбирает переданную строку вида <tag key="val"> и создаёт на её основе
    //структуру, представляющую элемент XML включая теги, но без дочерних
    //элементов.
    if (Length(S)>=7) and (S[1..5]='<?xml') and (S[length(s)-1..Length(s)]='?>')
    then begin
        result := TVList.Create([
            TVKeyword.Create(':PROLOGUE'),
            decode_attributes(s[6..Length(s)-2])]);
        closed := true;
        Exit;
    end;

    if (S[1]='<') and (S[Length(S)]='>') then begin
        word_end := PosU(' ', S) - 1;
        if word_end<0 then word_end := PosU('/', S) - 1;
        if word_end<0 then word_end := Length(S) - 1;
        closed := S[Length(S)-1]='/';// PosU('/', S)>0;
        if closed
        then attr_str := s[word_end+1..Length(s)-2]
        else attr_str := s[word_end+1..Length(s)-1];
        result := TVList.Create([
            TVString.Create(decode(S[2..word_end])),
            decode_attributes(attr_str)]);
        exit;
    end;

    result := TVString.Create(decode(S));
    closed := true;
end;

type
    TSSrec = record
        os, oe, cs, ce: integer;
    end;

function find_first_tag(const s: unicodestring; var sr: TSSrec): boolean;
begin
    result := false;
    sr.os := PosU('<', s, sr.ce+1);
    if sr.os=0 then Exit;
    sr.oe := PosU('>', s, sr.os+1);
    if sr.oe=0 then Exit;
    result := true;
    sr.cs:=sr.oe;
    sr.ce:=sr.oe;
end;

function find_closing_tag(const s: unicodestring;
                            const name: unicodestring;
                            var sr: TSSrec): boolean;
var depth, np: integer;
begin
    result := false;
    depth := 1;
    np := sr.oe;
    while true do begin
        np := PosU(name, s, np);
        if np=0 then exit;
        //второе условие требуется, что бы самозакрытые теги не влияли на
        //глубину вложенности
        if (s[np-1]='<') and (s[PosU('>',s,np)-1]<>'/') then Inc(depth);
        if (s[np-2]='<') and (s[np-1]='/') then Dec(depth);
        if depth=0 then begin
            sr.cs := np-2;
            sr.ce := PosU('>', s, sr.cs);
            if sr.ce>0 then result := true;
            exit;
        end;
        np := np + Length(name);
    end;
end;

function xml_read_from_string(S: unicodestring): TValue;
var node: TVRecord;
    elts: TVList;
    sr: TSSrec;
    tag_name: unicodestring;
begin
    elts := TVList.Create;
    sr.os := 1; sr.oe:=1; sr.cs:=1; sr.ce:=0;

    while find_first_tag(s, sr) do begin
        //кусок текста до начала тега считается текстовым элементом
        if (sr.os-sr.ce)>1 then elts.Add(decode_tag(S[sr.ce+1..(sr.os-1)]));
        //из найденного тега делаем элемент, который далее заполняется дочерними
        node := decode_tag(S[sr.os..sr.oe]);
        if node.look[sCHILD] is TVT
        then node[sCHILD] := TVList.Create
        else begin
            //  если тег не закрытый, то ищется закрывающий
            tag_name := (node.look[sNAME] as TVString).S;
            if not find_closing_tag(s, tag_name, sr)
            then raise ELE.Create('tag '+tag_name+' not closed', 'xml');

            node[sCHILD] := xml_read_from_string(S[sr.oe+1..sr.cs-1]);
        end;
        elts.Add(node);
    end;
    if sr.ce<Length(S) then elts.add(decode_tag(S[sr.ce+1..Length(s)]));

    result := elts;
end;

function xml_read_from_string_l(S: unicodestring): TVList;
var node: TValue;
    elts: TVList;
    sr: TSSrec;
    tag_name: unicodestring;
    closed: boolean;
begin
    elts := TVList.Create;
    sr.os := 1; sr.oe:=1; sr.cs:=1; sr.ce:=0;

    while find_first_tag(s, sr) do begin
        //кусок текста до начала тега считается текстовым элементом
        if (sr.os-sr.ce)>1 then elts.Add(decode_tag(S[sr.ce+1..(sr.os-1)]));
        //из найденного тега делаем элемент, который далее заполняется дочерними

        node := decode_tag_l(S[sr.os..sr.oe], closed);
        if not closed
        then begin
            tag_name := (node as TVList).S[0];
            if not find_closing_tag(s, tag_name, sr)
            then raise ELE.Create('tag '+tag_name+' not closed', 'xml');
            (node as TVList).Append(xml_read_from_string_l(S[sr.oe+1..sr.cs-1]));
        end;
        elts.Add(node);
    end;
    if sr.ce<Length(S) then elts.add(decode_tag_l(S[sr.ce+1..Length(s)], closed));

    result := elts;
end;

function xml_read_from_string_sl(S: unicodestring): TVList;
var node: TValue;
    elts: TVList;
    sr: TSSrec;
    tag_name: unicodestring;
    closed: boolean;
begin
    elts := TVList.Create;
    sr.os := 1; sr.oe:=1; sr.cs:=1; sr.ce:=0;

    while find_first_tag(s, sr) do begin
        //кусок текста до начала тега считается текстовым элементом
        if (sr.os-sr.ce)>1 then elts.Add(decode_tag(S[sr.ce+1..(sr.os-1)]));
        //из найденного тега делаем элемент, который далее заполняется дочерними

        node := decode_tag_l(S[sr.os..sr.oe], closed);
        if not closed
        then begin
            tag_name := (node as TVList).S[0];
            if not find_closing_tag(s, tag_name, sr)
            then raise ELE.Create('tag '+tag_name+' not closed', 'xml');
            (node as TVList).Add(xml_read_from_string_sl(S[sr.oe+1..sr.cs-1]));
        end;
        elts.Add(node);
    end;
    if sr.ce<Length(S) then elts.add(decode_tag_l(S[sr.ce+1..Length(s)], closed));

    result := elts;
end;

function tag_write_to_string(tag: TVList): unicodestring;
var i: integer;
begin
    //WriteLn(tag.AsString);
    if vpKeyword_PROLOGUE(tag.look[0]) then begin
        result := '<?xml';
        for i := 0 to (tag.L[1].count div 2)-1 do
            result := result+' '+tag.L[1].SYM[i*2].name+'="'+tag.L[1].S[i*2+1]+'"';
        result := result+'?>';
        exit;
    end;

    result := '<'+tag.S[0];
    for i := 0 to (tag.L[1].count div 2)-1 do
        result := result+' '+tag.L[1].SYM[i*2].name+'="'+encode(tag.L[1].S[i*2+1])+'"';
    result := result+'>';

    for i := 2 to tag.high do
        if tpString(tag.look[i])
        then result := result+encode(tag.S[i])
        else result := result+tag_write_to_string(tag.L[i]);

    result := result+'</'+tag.S[0]+'>';
end;

function xml_write_to_string(xml: TVList): unicodestring;
var i: integer;
begin
    result := '';
    for i := 0 to xml.high do
        if tpString(xml.look[i])
        then result := result + encode(xml.S[i])
        else result := result + tag_write_to_string(xml.L[i]);
end;

function tag_open(s: unicodestring): boolean;
var last: integer;
begin
    last := Length(s);
    result := (Length(s)>2)
        and (s[1]='<')
        and (s[last]='>')
        and (s[2]<>'/')
        and (s[2]<>'?')
        and (s[2]<>'!')
        and (s[last-1]<>'/');
end;

function tag_close(s: unicodestring): boolean;
var last: integer;
begin
    last := Length(s);
    result := (Length(s)>2)
        and (s[1]='<')
        and (s[last]='>')
        and (s[2]='/')
        and (s[last-1]<>'/');
end;

function tag_empty(s: unicodestring): boolean;
var last: integer;
begin
    last := Length(s);
    result := (Length(s)>2)
        and (s[1]='<')
        and (s[last]='>')
        and (s[2]<>'/')
        and (s[last-1]='/');
end;

function tag_text(s: unicodestring): boolean;
begin
    result := (Length(s)=0) or (s[1]<>'<');
end;

function tag_quote(s: unicodestring): boolean;
var last: integer;
begin
    last := Length(s);
    result := (Length(s)>=7)
        and (s[1..4]='<!--')
        and (s[last-2..last]='-->');
end;

procedure read_tags(s: TStream; var tags: TStringList; encoding: TStreamEncoding);
var depth, i: integer; ch: unicodechar; acc: unicodestring;
    quoted: boolean;
    procedure add(s: unicodestring);
    begin
        if acc='' then Exit;
        //костыль для пропуска переводов строки до начала разметки
        //не добавлять текст первым элементом
        if not ((tags.count=0) and tag_text(s)) then tags.Add(s);
        if tag_open(s) then Inc(depth)
        else
            if tag_close(s) then Dec(depth);
    end;
    function tail(s: unicodestring; n: integer):unicodestring;
    begin
        if n<Length(s)
        then result := s[length(s)-n+1..Length(s)]
        else result := s;
    end;

begin
    quoted := false;
    depth := 0;
    acc := '';
    tags.Clear;

    while s.Position<s.Size do begin
        ch := read_character(s, encoding);
        case ch of
            '<': if not quoted then begin
                add(acc);
                acc := '<';
            end else acc := acc+'<';
            '>': begin
                acc := acc + '>';
                if tail(acc, 3)='-->' then quoted := false;
                if not quoted then begin
                    add(acc);
                    acc := '';
                end;
                if depth=0 then Break;
            end;
            '-': begin
                acc := acc+'-';
                if acc='<!--' then quoted := true;
            end
            else acc := acc + ch;
        end;
    end;
end;

function decode_tag_open(s: unicodestring): TVList;
var state : (sTag, sName, sValue, sQuoted, sDQuoted);
var i: integer; acc: unicodestring; attr: TVList;
    procedure add_attr; begin attr.Add(TVString.Create(acc)); acc := ''; end;
begin
    result := TVList.Create;
    attr := TVList.Create;
    state := sTag;
    acc := '';
    for i := 1 to Length(s) do
        case state of
            sTag: case s[i] of
                ' ','/','>': begin
                        result.Add(TVString.Create(acc));
                        acc := '';
                        state := sName;
                    end;
                '<': begin end;
                else acc := acc + s[i];
                end;
            sName: case s[i] of
                ' ': begin end;
                '=': begin add_attr; state := sValue; end;
                else acc := acc + s[i];
                end;
            sValue: case s[i] of
                '''': state := sQuoted;
                '"': state := sDQuoted;
                end;
            sQuoted: if s[i]=''''
                then begin add_attr; state := sName; end
                else acc := acc + s[i];
            sDQuoted: if s[i]='"'
                then begin add_attr; state := sName; end
                else acc := acc + s[i];
        end;
    result.Add(attr);
end;

function tags_tree(tags: TStringList; var i: integer): TVList;
var  closed: boolean;
begin
    result := decode_tag_open(tags[i]);

    if tag_open(tags[i]) then
        while i<tags.Count do begin
            Inc(i);
            if tag_open(tags[i])
            then result.Add(tags_tree(tags, i))
            else

            if tag_text(tags[i])
            then result.Add(TVString.Create(decode(tags[i])))
            else

            if tag_close(tags[i])
            then begin
                if tags[i]<>('</'+result.S[0]+'>')
                then raise ELE.Create('Непарный тэг '+result.S[0]+' -- '+tags[i], 'xml');
                Break;
            end;
        end;
end;


function xml_read(s: TStream): TVList;
var tags: TStringList; i: integer; ch: unicodechar;
    encoding: TStreamEncoding;
    prologue, s_encoding: unicodestring;
    attr: TStringList;

begin try
    tags := TStringList.Create;
    attr := TStringList.Create;
    case s.ReadDWord of
        $6D783F3C: begin prologue := '<?xm';    encoding := seUTF8;     end;
        $3CBFBBEF: begin prologue := '<';       encoding := seUTF8;     end;
        $003CFEFF: begin prologue := '<';       encoding := seUTF16LE;  end;
        $3C00FFFE: begin prologue := '<';       encoding := seUTF16BE;  end;
        $0000FEFF: begin prologue := '';        encoding := seUTF32LE;  end;
        $FFFE0000: begin prologue := '';        encoding := seUTF32BE;  end;
        else raise ELE.Create('Damaged byte order mask','xml');
    end;

    repeat
        ch := read_character(s, encoding);
        prologue := prologue + ch;
        if ch in [#13, #10, '<'] then raise ELE.Create('Повреждён пролог '+prologue,'xml');
    until (ch='>');

    attr.Delimiter := ' ';
    attr.DelimitedText := prologue[7..Length(prologue)-2];
    s_encoding := UpperCase(attr.Values['encoding']);
    if s_encoding='"UTF-8"' then encoding := seUTF8;
    if s_encoding='"WINDOWS-1251"' then encoding := seCP1251;
    if s_encoding='"WINDOWS-1252"' then encoding := seCP1252;
    if s_encoding='"KOI-8"' then encoding := seKOI8R;

    repeat
        read_tags(s, tags, encoding);
    until tag_open(tags[0]) or (s.Position=s.Size);
    i := 0;
    result := tags_tree(tags,i);
finally
    tags.Free;
    attr.Free;
end;
end;

procedure tag_write(s: TStream; tag: TVList);
var i: integer;
begin
    write_string(s, '<'+tag.S[0]);
    for i := 0 to (tag.L[1].count div 2)-1 do
        write_string(s,' '+tag.L[1].S[i*2]+'="'+encode(tag.L[1].S[i*2+1])+'"');
    write_string(s, '>');

    for i := 2 to tag.high do
        if tpString(tag.look[i])
        then write_string(s, encode(tag.S[i]))
        else tag_write(s, tag.L[i]);

    write_string(s, '</'+tag.S[0]+'>');
end;

procedure xml_write(s: TStream; xml: TVList);
var  i: integer;
begin
    write_BOM(s, seUTF8);
    write_string(s, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
    tag_write(s, xml);
end;

end.

