unit dlisp_read;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_values, mar;



function read(str: TVStreamPointer): TValue; overload;
function read_from_string(s: unicodestring): TValue;
procedure print_stdout(V:TValue);
procedure print_stdout_ln(V: TValue);
procedure print_s(V:TValue; stream: TStream); overload;
procedure print(V:TValue; stream: TVStreamPointer); overload;
procedure print_ln(V:TValue; stream: TStream);


implementation

type TSS = record
        s: unicodestring;
        i: integer;
    end;
    PSS = ^TSS;

function str_is_range(s: unicodestring; var l, h: Int64): boolean;
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

function str_is_datetime(s: unicodestring; var dt: TDateTime): boolean;
var p_ym, p_md, p_dh, p_hm, p_hs, p_d: integer;
    year, month, day, hours, minutes, seconds, milliseconds: word;
label ret;
begin try
    result := false;
    if not ((Length(s)>=12) and (s[1]='''') and (s[Length(s)]='''')
        and (s[6]='-') and (s[9]='-') and (Length(s)<=25)) then exit;

    year := StrToInt(s[2..5]);
    month := StrToInt(s[7..8]);
    day := StrToInt(s[10..11]);
    dt := EncodeDate(year, month, day);

    hours := 0;
    minutes := 0;
    seconds := 0;
    milliseconds := 0;

    if Length(s)=12 then begin result := true; exit; end;
    if s[12]<>' ' then exit;
    if Length(s)<18 then exit;
    if s[15]<>':' then exit;
    hours := StrToInt(s[13..14]);
    minutes := StrToInt(s[16..17]);
    dt := dt + hours/24 + minutes/(24*60);
    if Length(s)=18 then begin result := true; exit; end;
    if s[18]<>':' then exit;
    if Length(s)<21 then exit;
    seconds := StrToInt(s[19..20]);
    dt := dt + seconds/(24*60*60);
    if Length(s)=21 then begin result := true; exit; end;
    if (s[21]<>'.') and (s[21]<>',') then exit;
    if Length(s)<25 then exit;
    milliseconds := StrToInt(s[22..24]);
    dt := dt + milliseconds/(24*60*60*1000);

    result := true;
except
    on EConvertError do result := false;
end;
end;

function str_is_elt_call(s: unicodestring; var elt: unicodestring): boolean;
var p2, p1: integer; sep: unicodestring;
    function PosS(offset: integer): integer;
    var f,b: integer;
    begin
//        writeLn('PosS ',s,' ',offset);
        f := PosU('/',s, offset);
        b := PosU('\',s, offset);
        if (f=0) and (b=0) then result := 0
        else
            if (f=0) and (b>0) then begin
                sep := '\';
                result := b;
            end
            else
                if (f>0) and (b=0) then begin
                    sep :='';
                    result := f;
                end
                else
                    if b<f then begin
                        sep := '\';
                        result := b;
                    end
                    else
                        if f<b then begin
                            sep := '';
                            result := f;
                        end;
        if (offset>1) and (s[offset-1]='\') then sep :='\' else sep := '';
        //writeLn('PosS ',s,' ',offset,' ',f,' ',b,' r:',result,' ',sep);
    end;

begin
    //result := false;
    //p2 := PosU(field_separator, s);
    //if (p2<=1) or
    //    (S[Length(s)-Length(field_separator)+1..Length(s)]=field_separator)
    //then exit;
    //
    //elt := '(ELT '+S[1..p2-1];
    //p1 := p2 + Length(field_separator);
    //while p2>0 do begin
    //    p2 := PosU(field_separator,s, p1);
    //    if p2>0 then begin
    //        elt := elt +' '+ s[p1..p2-1];
    //        p1 := p2 + Length(field_separator);
    //    end;
    //end;
    //elt := elt +' '+s[p1..Length(s)]+')';
    //result := true;

    result := false;
    p2 := PosS(1);
    if (p2<=1) or (S[Length(s)] in ['/','\']) then exit;


    elt := '(ELT '+S[1..p2-1];

    p1 := p2 + 1;
    while p2>0 do begin
        p2 := PosS(p1);
        if p2>0 then begin
            elt := elt +' '+sep+ s[p1..p2-1];
            p1 := p2 + 1;
        end;
    end;
    elt := elt +' '+sep+s[p1..Length(s)]+')';
    result := true;
end;

function str_is_float(s: unicodestring; var f: double): boolean;
var fs: TFormatSettings;
begin
    fs.DecimalSeparator:='.';
    result := TryStrToFloat(s, f, fs);
    if result then exit;

    fs.DecimalSeparator:=',';
    result :=  TryStrToFloat(s, f, fs);
end;

function is_nil(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count=0);
end;


function read_u(sp: TVStreamPointer; ss_in: PSS = nil): TValue;
var
    q, e, r, sq: boolean;
    p: integer;
    f: double;
    i,l, h: Int64;
    ch: unicodechar;
    acc, trans: unicodestring;
    ss: TSS;
    vi, vi2: TValue;
    dt: TDateTime;

    function read_char: boolean;
    var b1, b2: byte;
    begin
        if sp<>nil then begin result := sp.read_char(ch); exit; end;
        if ss_in<>nil then begin
            if ss_in.i <=Length(ss_in.s) then begin
                ch := ss_in.S[ss_in.i];
                inc(ss_in.i);
                result := true;
            end
            else result := false;
            Exit;
        end;
    end;

    procedure accum; begin if (not r) then acc := acc + ch; end;
    function trim(s: unicodestring): unicodestring; begin trim:=Copy(s,2,length(s)-2); end;
    procedure set_ss(s: unicodestring); begin ss.s := s; ss.i := 1; end;

begin
    q := false;
    sq := false;
    e := false;
    p := 0;
    r := false;
    acc := '';
    while read_char do begin
        case ch of
            ' ', #13, #10, #$FEFF, #9: begin
                    if (not q) and (not sq) and (p<=0) and (acc<>'') then break;
                    if q or sq or (p>0) then accum;
                    if ((ch=#13) or (ch=#10)) then r := false;
                end;
            '"': begin
                    if (not r) then q := not q;
                    accum;
                end;
            '''': begin
                    if (not q) and (not r) then sq := not sq;
                    accum;
                end;
            '(': begin
                    if (not q) and (not r) then Inc(p);
                    accum;
                end;
            ')': begin
                    accum;
                    if (not q) and (not r) then
                    begin Dec(p); if p<=0 then break end;
                end;
            '\': begin
                    if q and (not r)
                    //and (p<=0)
                    then begin
                        if p>0 then accum;
                        read_char;
                    end;
                    accum;
                end;
            ';': if not q then r := true;
            else accum;
        end;
    end;

    result := nil;
    //writeln('>>', acc);
    if acc=''
    then result := TVEndOfStream.Create
    else

    if (Length(acc)>=2) and (acc[1]='"') and (acc[Length(acc)]='"')
    then result := TVString.Create(trim(acc))
    else

    if (acc[1]='(') and (acc[Length(acc)]=')')
    then begin
        result := TVList.Create;
        set_ss(trim(acc));
        vi := read_u(nil, @ss);
        //vi.Print;
        while not (vi is TVEndOfStream) do begin
            (result as TVList).Add(vi);
            vi := read_u(nil, @ss);
        end;
        vi.Free;
    end
    else


    if (length(acc)>=4) and (UpperCaseU(acc[1..3])='#S(') and (acc[Length(acc)]=')')
    then begin
        result := TVRecord.Create;
        set_ss(acc[4..Length(acc)-1]);
        vi := read_u(nil, @ss);
        vi2 := read_u(nil, @ss);
        //WriteLn('read #S  ', vi.AsString, '  ', vi2.AsString);
        //TODO: при считывании структур игнорируется нечётный аргумент
        while not ((vi is TVEndOfStream) or (vi2 is TVEndOfStream))
        do begin

            if vi is TVSymbol
            then (result as TVRecord).AddSlot((vi as TVSymbol).name, vi2)
            else raise ELE.Create(vi.AsString+' is not symbol', 'syntax');
            FreeAndNil(vi);
            //vi2 здесь не освобождается потому что сохранено в слоте результата
            vi := read_u(nil, @ss);
            vi2 := read_u(nil, @ss);
            //WriteLn('read #S  ', vi.AsString, '  ', vi2.AsString);
        end;
        vi.Free;
        vi2.Free;
    end
    else

    if (Length(acc)>=2) and (acc[1]='\')
    then begin
        set_ss(Copy(acc,2,length(acc)-1));
        result := TVList.Create([TVSymbol.Create('QUOTE'), read_u(nil, @ss)]);
    end
    else

    if (Length(acc)>=2) and (acc[1]='/')
    then begin
        set_ss(Copy(acc,2,length(acc)-1));
        result := TVList.Create([TVSymbol.Create('INS'), read_u(nil, @ss)]);
    end
    else

    if str_is_datetime(acc, dt)
    then result := TVDateTime.Create(dt)
    else

    if (acc[Length(acc)]=')') or (Pos(' ',acc)>0)
    then raise ELE.Create(acc, 'syntax')
    else

    if UpperCaseU(acc)='NIL'
    then result := TVList.Create
    else

    if UpperCaseU(acc)='T'
    then result := TVT.Create
    else

    if StrToInt64Def(acc,0)=StrToInt64Def(acc,1)
    then result := TVInteger.Create(StrToInt64(acc))
    else

    if str_is_float(acc, f)
    then result := TVFloat.Create(f)
    else

    if str_is_range(acc, l, h)
    then result := TVRange.Create(l,h)
    else

    if str_is_elt_call(acc, trans)
    then result := read_from_string(trans)
    else

    result := TVSymbol.Create(acc);
end;


function read(str: TVStreamPointer): TValue;
begin
    result := read_u(str);

end;

function read_from_string(s: unicodestring): TValue;
var
    ss: TSS;
begin
    ss.s := s;
    ss.i := 1;
    result := read_u(nil, @ss);
end;



const screen_width = 50;
var ind: integer = 0;

procedure print_stdout(V:TValue);
var  i: integer;
    procedure indent;
    var i: integer;
    begin for i:=1 to ind do write('  '); end;
begin
    if not (V is TVList)
    then Write(V.AsString())
    else
        if (V as TVList).count=0
        then write('NIL')
        else
            if (Length((V as TVList).AsString())+ind*2)<=screen_width
            then Write((V as TVList).AsString())
            else begin
                Inc(ind);
                Write('( ');
                print_stdout((V as TVList)[0]);
                for i:=1 to (V as TVList).count-1 do begin
                    writeln('');
                    indent;
                    print_stdout((V as TVList)[i]);
                end;
                write(')');
                Dec(ind);
            end;
end;


procedure print_stdout_ln(V: TValue);
begin
    print_stdout(V);
    WriteLn();
end;


procedure print_s(V:TValue; stream: TStream);
var  i: integer;
    procedure wr(s: unicodestring);
    begin
        stream.Write(s[1],Length(S));
//        stream.w
    end;
    procedure indent;
    var i: integer;
    begin for i:=1 to ind do wr('  '); end;
begin
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
                wr('( ');
                print_s((V as TVList).look[0], stream);
                for i:=1 to (V as TVList).count-1 do begin
                    //S.writeln('');
                    //wr(#13+#10);
                    Stream.writebyte(13); Stream.writebyte(10);
                    indent;
                    print_s((V as TVList).look[i], stream);
                end;
                wr(') ');
                Dec(ind);
            end;
end;

procedure print(V:TValue; stream: TVStreamPointer);
var  i: integer; sn: unicodestring;
    procedure wr(s: unicodestring);
    var i: integer;
    begin
        if stream<>nil
        then for i := 1 to Length(s) do stream.write_char(s[i])
        else System.Write(s);
    end;
    procedure indent;
    var i: integer;
    begin for i:=1 to ind do wr(' '); end;
begin
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
                    wr(new_line);
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
            wr('#S(');
            sn := (V as TVRecord).name_n(0)+' ';
            wr(sn);
            Inc(ind, Length(sn));
            print((V as TVRecord).look[0], stream);
            Dec(ind, length(sn));
            for i := 1 to (V as TVRecord).count-1 do begin
                wr(new_line);
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
                    wr(new_line);
                    indent;
                    print((V as TVList).look[i], stream);
                end;
                wr(')');
                Dec(ind);
            end;
end;

procedure print_ln(V:TValue; stream: TStream);
begin
    print_s(V, stream);
    Stream.writebyte(13); Stream.writebyte(10);
end;


end.

