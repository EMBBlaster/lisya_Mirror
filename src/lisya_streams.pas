unit lisya_streams;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    zstream, LResources,
    Classes, SysUtils, mar, lisia_charset, lisya_exceptions, lisya_zip;

type

    { TLStream }

    TLStream = class(TCountingObject)
    private
      fencoding: TStreamEncoding;
      stream: TStream;
      procedure CheckState;
      procedure SetEncoding(enc: TStreamEncoding);
      function GetPosition: Int64;
      procedure SetPosition(p: Int64);
      function GetSize: Int64;
      procedure SetSize(s: Int64);
    public
      constructor Create(trg: TStream; enc: TStreamEncoding=seUTF8);
      constructor FindBuiltIn(name: unicodestring);

      destructor Destroy; override;

      property encoding: TStreamEncoding read fencoding write SetEncoding;
      function read_byte(out b: byte): boolean;
      function read_bytes(var bb: TBytes; count: integer): boolean;
      function read_DWORD: DWORD;
      function write_byte(b: byte): boolean;
      function write_bytes(bb: TBytes): boolean;
      function read_char(out ch: unicodechar): boolean;
      function read_character: unicodechar;
      function write_char(ch: unicodechar): boolean;
      function write_string(s: unicodestring): boolean;

      property position: Int64 read GetPosition write SetPosition;
      property size: Int64 read GetSize write SetSize;

      procedure close_stream;
      function active: boolean;
      procedure write_BOM;
    end;

    { TLMemoryStream }

    TLMemoryStream = class(TLStream)
      constructor Create(enc: TStreamEncoding = seUTF8); overload;
      constructor Create(const b: TBytes; enc: TStreamEncoding = seUTF8); overload;
      constructor Create(const s: unicodestring); overload;
      function description: unicodestring; override;
    end;

    { TLFileStream }

    TLFileStream = class(TLStream)
        constructor Create(fn: unicodestring; mode: WORD; enc: TStreamEncoding);
        function description: unicodestring; override;
    end;

    { TLDeflateStream }

    TLDeflateStream = class(TLStream)
      target: TLStream;
      constructor Create(trg: TLStream; head: boolean = false; enc: TStreamEncoding=seUTF8);
      destructor Destroy; override;
      function description: unicodestring; override;
    end;

    { TLInflateStream }

    TLInflateStream = class(TLStream)
      target: TLStream;
      constructor Create(trg: TLStream; head: boolean = false; enc: TStreamEncoding=seUTF8);
      destructor Destroy; override;
      function description: unicodestring; override;
    end;


    { TLZipFile }

    TLZipFile = class(TLStream)
        archive: TZipArchive;
        file_name: unicodestring;
        constructor Create(Z: TZipArchive; fn: unicodestring; mode: WORD; enc: TStreamEncoding);
        destructor Destroy; override;
        function description: unicodestring; override;
    end;


implementation


{ TLZipFile }

constructor TLZipFile.Create(Z: TZipArchive; fn: unicodestring; mode: WORD;
    enc: TStreamEncoding);
begin
    archive := Z.Ref as TZipArchive;
    file_name := fn;
    inherited Create(archive.GetFileStream(fn, mode), enc);
end;

destructor TLZipFile.Destroy;
begin
    archive.Release;
    inherited Destroy;
end;

function TLZipFile.description: unicodestring;
begin
    result := '#<FILE '+archive.description+': '+file_name+'>';
end;

{ TLInflateStream }

constructor TLInflateStream.Create(trg: TLStream; head: boolean;
    enc: TStreamEncoding);
begin
    target := trg;
    inherited Create(TDecompressionStream.create(target.stream,  not head), enc);
end;

destructor TLInflateStream.Destroy;
begin
    target.Release;
    inherited Destroy;
end;

function TLInflateStream.description: unicodestring;
begin
    if stream<>nil
    then result := 'INFLATE STREAM -> '+target.description
    else result := 'INFLATE STREAM';
end;

{ TLDeflateStream }

constructor TLDeflateStream.Create(trg: TLStream; head: boolean;
    enc: TStreamEncoding);
begin
    target := trg;
    inherited Create(TCompressionStream.create(clDefault, target.stream , not head), enc);
end;

destructor TLDeflateStream.Destroy;
begin
    // сначала должен быть освобождён свой CompressionStream, а потом поток
    // в который он записывает, в противном случае происходит обращение к
    // не существующему объекту
    inherited Destroy;
    target.Release;
end;

function TLDeflateStream.description: unicodestring;
begin
    if stream<>nil
    then result := 'DEFLATE STREAM ('+IntToStr(refs)+') -> '+target.description
    else result := 'DEFLATE STREAM';
end;

{ TLFileStream }

constructor TLFileStream.Create(fn: unicodestring; mode: WORD;
    enc: TStreamEncoding);
begin
    case mode of
        fmOpenRead: if not FileExists(fn)
                        then ELE.Create(fn, 'file not found')
                        else stream := TFileStream.Create(fn, fmOpenRead);
        fmCreate: stream := TFileStream.Create(fn, fmCreate);
        fmOpenReadWrite: begin
            if FileExists(fn)
            then stream := TFileStream.Create(fn, fmOpenReadWrite)
            else stream := TFileStream.Create(fn, fmCreate);
            stream.Seek(stream.Size,0);
        end;
    end;
    inherited Create(stream, enc);
end;

function TLFileStream.description: unicodestring;
begin
    if stream<>nil
    then result := 'FILE ('+IntToStr(refs)+') '+(stream as TFileStream).FileName
    else result := 'FILE';
end;

{ TLMemoryStream }

constructor TLMemoryStream.Create(enc: TStreamEncoding);
begin
    inherited Create(TMemoryStream.Create, enc);
end;

constructor TLMemoryStream.Create(const b: TBytes; enc: TStreamEncoding);
var i: integer;
begin
    inherited Create(TMemoryStream.Create, seUTF8);
    for i := 0 to high(b) do stream.WriteByte(b[i]);
    stream.Position:=0;
    encoding := enc;
end;

constructor TLMemoryStream.Create(const s: unicodestring);
begin
    inherited Create(TMemoryStream.Create, seUTF8);
    lisia_charset.write_string(stream, s, encoding);
    stream.Position:=0;
end;

function TLMemoryStream.description: unicodestring;
begin
    if stream<>nil
    then result := 'MEMORY STREAM ('+IntToStr(stream.Size)+' bytes)'
    else result := 'MEMORY STREAM (NIL)';
end;



{ TLStream }

procedure TLStream.CheckState;
begin
    if stream=nil then raise ELE.Create('operation on closed stream','stream');
end;


constructor TLStream.Create(trg: TStream; enc: TStreamEncoding);
begin
    inherited Create;
    stream := trg;
    encoding := enc;
end;

constructor TLStream.FindBuiltIn(name: unicodestring);
var i: integer;
begin
    inherited Create;
    stream := nil;
    fencoding := seUTF8;

    for i:=0 to lazarusResources.Count-1 do
        if lazarusResources.Items[i].Name=name then begin
            stream := TLazarusResourceStream.CreateFromHandle(lazarusResources.Items[i]);
            Exit;
        end;
end;

destructor TLStream.Destroy;
begin
    FreeAndNil(stream);
    inherited Destroy;
end;


procedure TLStream.SetEncoding(enc: TStreamEncoding);
begin
    CheckState;
    if enc = seBOM
    then fencoding := lisia_charset.read_BOM(stream)
    else fencoding := enc;
end;

function TLStream.GetPosition: Int64;
begin
    CheckState;
    result := stream.Position;
end;

procedure TLStream.SetPosition(p: Int64);
begin
    CheckState;
    stream.Position:=p;
end;

function TLStream.GetSize: Int64;
begin
    CheckState;
    result:=stream.Size;
end;

procedure TLStream.SetSize(s: Int64);
begin
    CheckState;
    stream.Size:=s;
end;


function TLStream.read_byte(out b: byte): boolean;
begin
    CheckState;
    try
        b := stream.ReadByte;
        result := true;
    except
        on E:EStreamError do result := false;
    end;
end;

function TLStream.read_bytes(var bb: TBytes; count: integer): boolean;
var i: integer; _count: integer;
begin
    CheckState;
    if count>=0 then _count := count else _count := stream.Size - stream.Position;
    SetLength(bb, _count);
    try
        result := true;
        for i := 0 to _count-1 do bb[i] := stream.ReadByte;
        //TODO: по загадочным причинам readbuffer вызывает разрушение памяти
        //при освобождении буфера
    except
        on E:EStreamError do result := false;
    end;
end;

function TLStream.read_DWORD: DWORD;
begin
    result := stream.ReadDWord;
end;

function TLStream.write_byte(b: byte): boolean;
begin
    CheckState;
    stream.WriteByte(b);
    result := true;
end;

function TLStream.write_bytes(bb: TBytes): boolean;
var i: integer;
begin
    CheckState;
    for i := 0 to high(bb) do stream.WriteByte(bb[i]);
    //TODO: writeBuffer глючит так же как readbuffer
    result := true;
end;


procedure TLStream.write_BOM;
begin
    CheckState;
    lisia_charset.write_BOM(stream, encoding);
end;

function TLStream.read_char(out ch: unicodechar): boolean;
begin
    CheckState;
    try
        ch := lisia_charset.read_character(stream, encoding);
        result := true;
    except
        on E:EStreamError do result := false;
    end;
end;

function TLStream.read_character: unicodechar;
begin
    result := lisia_charset.read_character(stream, encoding);
end;

function TLStream.write_char(ch: unicodechar): boolean;
begin
    CheckState;
    lisia_charset.write_character(stream, ch, encoding);
    result := true;
end;

function TLStream.write_string(s: unicodestring): boolean;
begin
    CheckState;
    lisia_charset.write_string(stream, s, encoding);
    result := true;
end;

procedure TLStream.close_stream;
begin
    FreeAndNil(stream);
end;

function TLStream.active: boolean;
begin
    result := stream <> nil;
end;

end.

