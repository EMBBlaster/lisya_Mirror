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
    ,lisya_predicates;

function xml_read_from_string(s: unicodestring): TValue;
function xml_read_from_string_l(s: unicodestring): TVList;
function xml_read_from_string_sl(s: unicodestring): TVList;

function xml_write_to_string(xml: TVList): unicodestring;


implementation

function decode(s: unicodestring): unicodestring;
var i, p: integer;
    function decode_symbol(s: unicodestring): unicodestring;
    begin
        if s='amp'  then result := '&' else
        if s='gt'   then result := '>' else
        if s='lt'   then result := '<' else
        if s='quot' then result := '''' else

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
        result := result+' '+tag.L[1].SYM[i*2].name+'="'+tag.L[1].S[i*2+1]+'"';
    result := result+'>';

    for i := 2 to tag.high do
        if tpString(tag.look[i])
        then result := result+tag.S[i]
        else result := result+tag_write_to_string(tag.L[i]);

    result := result+'</'+tag.S[0]+'>';
end;

function xml_write_to_string(xml: TVList): unicodestring;
var i: integer;
begin
    result := '';
    for i := 0 to xml.high do
        if tpString(xml.look[i])
        then result := result+xml.S[i]
        else result := result + tag_write_to_string(xml.L[i]);
end;

end.

