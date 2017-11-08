unit lisya_xml;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_values, strutils, mar;

function xml_read_from_string(s: unicodestring): TValue;



implementation




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
        result.add(TVString.Create(s[ep+2..we-1]));
        ep := PosU('=', s, ep+1);
    end;

end;

function decode_tag(S: unicodestring): TVStructure;
var word_end: integer; closed: boolean;
begin
    // Разбирает переданную строку вида <tag key="val"> и создаёт на её основе
    //структуру, представляющую элемент XML включая теги, но без дочерних
    //элементов.
    // Если тэг закрытый, то список элементов = T, если нет, то NIL
    result := TVStructure.Create(['N', 'A', 'C']);
    if (Length(S)>=7) and (S[1..5]='<?xml') and (S[length(s)-1..Length(s)]='?>')
    then begin
        (result as TVStructure)['N'] := TVSymbol.Create(':PROLOGUE');
        (result as TVStructure)['C'] := TVT.Create;
        (result as TVStructure)['A'] := decode_attributes(s[6..Length(s)-2]);
        Exit;
    end;


    if (S[1]='<') and (S[Length(S)]='>') then begin
        word_end := PosU(' ', S) - 1;
        if word_end<0 then word_end := PosU('/', S) - 1;
        if word_end<0 then word_end := Length(S) - 1;
        (result as TVStructure)['N'] := TVString.Create(S[2..word_end]);
        closed := PosU('/', S)>0;
        if closed then begin
            (result as TVStructure)['C'] := TVT.Create;
            (result as TVStructure)['A'] := decode_attributes(
                                            s[word_end+1..Length(s)-2]);
        end
        else (result as TVStructure)['A'] := decode_attributes(
                                            s[word_end+1..Length(s)-1]);
        exit;
    end;
    (result as TVStructure)['N'] := TVSymbol.Create(':TEXT');
    (result as TVStructure)['C'] := TVString.Create(S);
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
var p_ths, p_the, p_tn, p_tcs, p_tcn, p, depth: integer;
    node: TVStructure;
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
        if node.look['C'] is TVT
        then node['C'] := TVList.Create
        else begin
            //  если тег не закрытый, то ищется закрывающий
            tag_name := (node.look['N'] as TVString).S;
            if not find_closing_tag(s, tag_name, sr)
            then begin
                result := TVError.Create(ecXML, 'tag '+tag_name+' not closed');
                exit;
            end;
            node['C'] := xml_read_from_string(S[sr.oe+1..sr.cs-1]);
        end;
        elts.Add(node);
    end;
    if sr.ce<Length(S) then elts.add(decode_tag(S[sr.ce+1..Length(s)]));


    result := elts;
end;


end.

