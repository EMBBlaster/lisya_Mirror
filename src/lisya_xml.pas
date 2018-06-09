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
    ,lisya_exceptions
    ,lisya_streams
    ;



function xml_read(s: TLStream): TVList;
procedure xml_write(s: TLStream; xml: TVList; declaration: boolean = true);


function xml_from_string(s: unicodestring): TVList;
function xml_to_string(xml: TVList): unicodestring;


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
            '"': result := result + '&quot;';
            else result := result + s[i];
        end;
end;


function tag_declaration(s: unicodestring): boolean;
var last: integer;
begin
    last := Length(s);
    result := (Length(s)>7)
        and (s[1..5]='<?xml')
        and (s[last-1..last]='?>')
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


function decode_tag_open(s: unicodestring): TVList;
var state : (sTag, sName, sValue, sQuoted, sDQuoted);
var i: integer; acc: unicodestring; attr: TVList;
    procedure add_attr;
    begin attr.Add(TVString.Create(decode(acc))); acc := ''; end;
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
                '<':
                else acc := acc + s[i];
                end;
            sName: case s[i] of
                ' ': ;
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

procedure decode_declaration(s: unicodestring; var encoding: TStreamEncoding);
var attr: TStringList; s_encoding: unicodestring;
begin
    //игнорировать кодировку в декларации если ранее многобайтная
    //кодировка была определена через BOM
    if encoding in [seUTF16LE, seUTF16BE, seUTF32LE, seUTF32BE] then exit;
    attr := TStringList.Create;
    attr.CaseSensitive:=false;
    attr.Delimiter := ' ';
    attr.DelimitedText := s[7..Length(s)-2];
    s_encoding := UpperCase(attr.Values['encoding']);
    if s_encoding='"UTF-8"' then encoding := seUTF8;
    if s_encoding='"WINDOWS-1251"' then encoding := seCP1251;
    if s_encoding='"WINDOWS-1252"' then encoding := seCP1252;
    if s_encoding='"KOI-8"' then encoding := seKOI8R;
    attr.Free;
end;


procedure read_tags(s: TLStream; var tags: TStringList);
var depth: integer; ch: unicodechar; acc: unicodestring;
    encoding: TStreamEncoding; BOM: DWORD; b: TBytes;
    quoted: boolean;
    procedure add;
    begin
        if acc='' then Exit;
        tags.Add(acc);
        if tag_open(acc) then if depth<0 then depth:=1 else Inc(depth)
        else
            if tag_close(acc) then Dec(depth)
            else
                if tag_declaration(acc) then decode_declaration(acc, encoding);
    end;
begin
    depth := -1;
    acc := '';
    tags.Clear;
    quoted := false;

    BOM := s.read_DWORD;
    case BOM of
        $6D783F3C: begin acc := '<?xm';    encoding := s.encoding; end;
        $3CBFBBEF: begin acc := '<';       encoding := seUTF8;     end;
        $003CFEFF: begin acc := '<';       encoding := seUTF16LE;  end;
        $3C00FFFE: begin acc := '<';       encoding := seUTF16BE;  end;
        $0000FEFF: begin acc := '';        encoding := seUTF32LE;  end;
        $FFFE0000: begin acc := '';        encoding := seUTF32BE;  end;
        else
            if (BOM and $FF)=$3C then begin
                SetLength(b, 4);
                b[0]:=$3C;
                b[1]:=(BOM shr 8) and $FF;
                b[2]:=(BOM shr 16) and $FF;
                b[3]:=(BOM shr 24) and $FF;
                acc:=bytes_to_string(b, s.encoding);
                encoding := s.encoding;
            end else raise ELE.Create('Damaged byte order mask','xml');
    end;

    while s.Position<s.Size do begin
        ch := s.read_character;
        case ch of
            '<': if not quoted
                then begin add; acc := '<'; end
                else acc := acc+'<';
            '>': begin
                acc := acc + '>';
                if acc[length(acc)-2..Length(acc)]='-->' then quoted := false;
                if not quoted then begin add; acc := ''; end;
                if depth=0 then Break;
            end;
            '-': begin
                acc := acc+'-';
                if acc='<!--' then quoted := true;
            end
            else acc := acc + ch;
        end;
    end;
    if depth<>0 then raise ELE.Create('malformed', 'xml');
end;

function tags_tree(tags: TStringList; var i: integer): TVList;
begin
    result := decode_tag_open(tags[i]);

    if tag_open(tags[i]) then
        while i<tags.Count do begin
            Inc(i);
            if tag_open(tags[i]) or tag_empty(tags[i])
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


function xml_read(s: TLStream): TVList;
var tags: TStringList; i: integer;
begin try
    result := nil;
    tags := TStringList.Create;

    read_tags(s, tags);
    //for i:=0 to tags.Count-1 do WriteLn(tags[i]);
    i := 0;
    while not tag_open(tags[i]) do Inc(i);
    result := tags_tree(tags,i);
finally
    tags.Free;
end;
end;

procedure tag_write(s: TLStream; tag: TVList);
var i: integer;
begin
    s.write_string('<'+tag.S[0]);
    for i := 0 to (tag.L[1].count div 2)-1 do
        s.write_string(' '+tag.L[1].S[i*2]+'="'+encode(tag.L[1].S[i*2+1])+'"');
    s.write_string('>');

    for i := 2 to tag.high do
        if tpString(tag.look[i])
        then s.write_string(encode(tag.S[i]))
        else tag_write(s, tag.L[i]);

    s.write_string('</'+tag.S[0]+'>');
end;

procedure xml_write(s: TLStream; xml: TVList; declaration: boolean);
begin
    s.encoding:=seUTF8;
    if declaration then begin
        s.write_character(BOM);
        s.write_string('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
    end;
    tag_write(s, xml);
end;

function xml_from_string(s: unicodestring): TVList;
var stream: TLMemoryStream;
begin try
    result := nil;
    stream := TLMemorystream.Create(s);
    result := xml_read(stream);
finally
    stream.Free;
end;
end;

function xml_to_string(xml: TVList): unicodestring;
var stream: TLMemoryStream;
begin try
    result := '';
    stream := TLMemoryStream.Create;
    tag_write(stream, xml);
    stream.Position:=0;
    while stream.Position<stream.Size do result := result+stream.read_character;
finally
    stream.Free;
end;

end;

end. //635

