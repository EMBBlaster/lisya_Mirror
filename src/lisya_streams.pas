unit lisya_streams;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring, unix, BaseUnix,
    {$ENDIF}
    {$IFDEF WINDOWS}
    windows,
    {$ENDIF}
    zstream, LResources, serial, pipes,
    Classes, SysUtils, mar, lisia_charset, lisya_exceptions, lisya_zip, lisya_process;

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
      function ReadBytes(var Buffer; count: integer; EoE: boolean = false): integer; virtual;
      function WriteBytes(const Buffer; count: integer; EoE: boolean = false): integer; virtual;
      function ReadCharacter(out ch: unicodechar; EoE: boolean = false): boolean;
    public
      constructor Create(trg: TStream; enc: TStreamEncoding=seUTF8);
      constructor FindBuiltIn(name: unicodestring);

      destructor Destroy; override;

      property encoding: TStreamEncoding read fencoding write SetEncoding;
      function read_byte: byte;
      function read_bytes(var bb: TBytes; count: integer): boolean;
      function read_DWORD: DWORD;
      function read_single: single;
      function read_double: double;

      function read_character: unicodechar; overload;
      function read_character(out ch: unicodechar): boolean; overload;
      function read_string(count: integer): unicodestring;
      function read_line(ls: unicodestring): unicodestring; overload;
      function read_line(out ln: unicodestring; ls: unicodestring): boolean; overload;

      procedure write_byte(b: byte);
      procedure write_bytes(bb: TBytes);
      procedure write_character(ch: unicodechar);
      procedure write_string(s: unicodestring);
      procedure write_single(f: double);
      procedure write_double(f: double);

      property position: Int64 read GetPosition write SetPosition;
      property size: Int64 read GetSize write SetSize;

      procedure close_stream; virtual;
      function active: boolean;
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
    private
        function GetFileName: unicodestring;
    public
        constructor Create(fn: unicodestring; mode: WORD; enc: TStreamEncoding);
        function description: unicodestring; override;
        procedure Log(msg: unicodestring);
        property FileName: unicodestring read GetFileName;
    end;

    { TLProxyStream }

    TLProxyStream = class(TLStream)
        target: TLStream;
        procedure Lock; override;
        procedure Unlock; override;
    end;

    { TLDeflateStream }

    TLDeflateStream = class(TLProxyStream)
      constructor Create(trg: TLStream; head: boolean = false; enc: TStreamEncoding=seUTF8);
      destructor Destroy; override;
      function description: unicodestring; override;
    end;

    { TLInflateStream }

    TLInflateStream = class(TLProxyStream)
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

        procedure Lock; override;
        procedure Unlock; override;
    end;


    { TLProcessPipes }

    TLProcessPipes = class(TLStream)
    private
        function ReadBytes(var Buffer; count: integer; EoE: boolean = false): integer; override;
        function WriteBytes(const Buffer; count: integer; EoE: boolean = false): integer; override;
    public
        proc: TLProcess;
        out_pipe: TStream;
        in_pipe: TStream;
        constructor Create(P: TLProcess; enc: TStreamEncoding);
        destructor Destroy; override;
        function description: unicodestring; override;

        procedure close_stream; override;
        procedure Lock; override;
        procedure Unlock; override;
    end;

    { TLSerialStream }

    TLSerialStream = class(TLStream)
    private
        port: {$IFDEF LINUX}TSerialHandle{$ELSE}THandle{$ENDIF};
        timeout: integer;
        function ReadBytes(var Buffer; count: integer; EoE: boolean = false): integer; override;
        function WriteBytes(const Buffer; count: integer; EoE: boolean = false): integer; override;
    public
        constructor Create(port_name: unicodestring; boud: integer;
            enc: TStreamEncoding=seUTF8;
            _timeout: integer=0);
        destructor Destroy; override;
        function description: unicodestring; override;
        procedure discard_input;
    end;


function bytes_to_string(const bytes: TBytes; encoding: TStreamEncoding): unicodestring;
function string_to_bytes(const s: unicodestring; encoding: TStreamEncoding): TBytes;

implementation

function bytes_to_string(const bytes: TBytes; encoding: TStreamEncoding
    ): unicodestring;
var stream: TLMemoryStream;
begin try
    stream := TLMemoryStream.Create(bytes, encoding);
    result := stream.read_string(-1);
finally
    stream.Release;
end;
end;

function string_to_bytes(const s: unicodestring; encoding: TStreamEncoding
    ): TBytes;
var stream: TLMemoryStream;
begin try
    stream := TLMemoryStream.Create(s);
    stream.encoding := encoding;
    stream.read_bytes(result, -1);
finally
    stream.Release;
end;
end;

{ TLProxyStream }

procedure TLProxyStream.Lock;
begin
    target.Lock;
end;

procedure TLProxyStream.Unlock;
begin
    target.Unlock;
end;


{ TLSerialStream }

function TLSerialStream.ReadBytes(var Buffer; count: integer; EoE: boolean
    ): integer;
var b: array of byte;
begin
    if timeout=0
    then result := SerRead(port, Buffer, Count)
    else begin
        SetLength(b, count);
        result := SerReadTimeout(port, b, count, timeout);
        Move(b[0],buffer,result);
        SetLength(b,0);
    end;
    if EoE and (result<count) then raise ELEmptyStream.Create('empty stream', 'serial/read');
end;

function TLSerialStream.WriteBytes(const Buffer; count: integer; EoE: boolean
    ): integer;
{$IFDEF WINDOWS}var BytesWritten: DWORD;{$ENDIF}
begin
    {$IFDEF WINDOWS}
    BytesWritten := 0;
    if not WriteFile(port, Buffer, Count, BytesWritten, nil)
    then result := 0
    else result := BytesWritten;
    {$ENDIF}
    {$IFDEF LINUX}
    result := fpWrite(port, Buffer, Count);
    {$ENDIF}
    if EoE and (result<>count) then raise ELEmptyStream.Create('write error', 'serial');
end;

constructor TLSerialStream.Create(port_name: unicodestring; boud: integer;
    enc: TStreamEncoding; _timeout: integer);
begin
    inherited Create(nil, enc);
    timeout := _timeout;
    port := SerOpen(port_name);
    if port=0 then raise ELE.Create('device not found '+port_name,'serial');
    SerSetParams(port, boud, 8, NoneParity, 1, []);
end;



destructor TLSerialStream.Destroy;
begin
    SerClose(port);
    port := 0;
    inherited Destroy;
end;

function TLSerialStream.description: unicodestring;
begin
    result := 'serial stream';
end;

procedure TLSerialStream.discard_input;
begin
    SerFlushInput(port);
end;



{ TLProcessPipes }

function TLProcessPipes.ReadBytes(var Buffer; count: integer; EoE: boolean
    ): integer;
begin
   if out_pipe=nil then raise ELE.Create('reading from closed pipe', 'stream');
   result := out_pipe.Read(Buffer, count);
   if EoE and (result<count) then raise ELEmptyStream.Create('pipe read', 'stream');
end;

function TLProcessPipes.WriteBytes(const Buffer; count: integer; EoE: boolean
    ): integer;
begin
    if in_pipe=nil then raise ELE.Create('writing to closed stream', 'stream');
    result := in_pipe.Write(Buffer, count);
    if EoE and (result<>count) then raise ELEmptyStream.Create('pipe write', 'stream');
end;

constructor TLProcessPipes.Create(P: TLProcess; enc: TStreamEncoding);
begin
    proc := P.Ref as TLProcess;
    inherited Create(nil, enc);
    out_pipe := P.Out_pipe;
    in_pipe := P.In_pipe;
end;

destructor TLProcessPipes.Destroy;
begin
    proc.Release;
    out_pipe := nil;
    in_pipe := nil;
end;

function TLProcessPipes.description: unicodestring;
begin
    result := 'PIPES OF '+proc.description;
end;

procedure TLProcessPipes.close_stream;
begin
    proc.CloseInput;
    in_pipe := nil;
end;

procedure TLProcessPipes.Lock;
begin
    proc.Lock;
end;

procedure TLProcessPipes.Unlock;
begin
    proc.Unlock;
end;


{ TLZipFile }

constructor TLZipFile.Create(Z: TZipArchive; fn: unicodestring; mode: WORD;
    enc: TStreamEncoding);
begin
    archive := Z;
    file_name := fn;
    inherited Create(archive.GetFileStream(fn, mode), enc);
end;

destructor TLZipFile.Destroy;
begin
    archive.Release;
    stream := nil; //при освобождении файла в архиве его поток не должен
                    //освобождаься, поскольку им управляет TLZipFile
    inherited Destroy;
end;

function TLZipFile.description: unicodestring;
begin
    result := '#<FILE '+archive.description+': '+file_name+'>';
end;

procedure TLZipFile.Lock;
begin
    archive.Lock;
end;

procedure TLZipFile.Unlock;
begin
    archive.Unlock;
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

function TLFileStream.GetFileName: unicodestring;
begin
    result := (stream as TFileStream).FileName;
end;

constructor TLFileStream.Create(fn: unicodestring; mode: WORD;
    enc: TStreamEncoding);
begin
    case mode of
        fmOpenRead: if not FileExists(fn)
                        then raise ELE.Create(fn, 'file not found')
                        else stream := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
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

procedure TLFileStream.Log(msg: unicodestring);
begin
    self.write_string(msg);
    {$IFDEF WINDOWS}
    FlushFileBuffers((self.stream as TFileStream).Handle);
    {$ELSE}
    fpfsync((self.stream as TFileStream).Handle);
    {$ENDIF}
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
    write_string(s);
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
    function b: byte; inline; begin ReadBytes(result, 1, true); end;
var p: integer;
begin
    if enc <> seBOM then begin fencoding := enc; Exit; end;
    CheckState;

    p := stream.Position;
    //UTF-8
    if (b=$EF) and (b=$BB) and (b=$BF) then begin
        fencoding := seUTF8;
        exit;
    end else stream.Seek(p,0);
    //UTF16BE
    if (b=$FE) and (b=$FF) then begin
        fencoding := seUTF16BE;
        exit;
    end else stream.Seek(p,0);
    //UTF32BE
    if (b=$00) and (b=$00) and (b=$FE) and (b=$FF) then begin
        fencoding := seUTF32BE;
        exit;
    end else stream.Seek(p,0);
    //UTF32LE
    if (b=$FF) and (b=$FE) and (b=$00) and (b=$00) then begin
        fencoding := seUTF32LE;
        exit;
    end else stream.Seek(p,0);
    //UTF16LE
    if (b=$FF) and (b=$FE) then begin
        fencoding := seUTF16LE;
        exit;
    end else stream.Seek(p,0);

    fencoding := default_encoding;
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
    //if stream is TInputPipeStream
    //then result := (stream as TInputPipeStream).NumBytesAvailable
    //else
    result:=stream.Size;
end;

procedure TLStream.SetSize(s: Int64);
begin
    CheckState;
    stream.Size:=s;
end;

function TLStream.ReadBytes(var Buffer; count: integer; EoE: boolean): integer;
begin
    if stream=nil then raise ELE.Create('reading from closed stream', 'stream');
    result := stream.Read(Buffer, count);
    if EoE and (result<count) then raise ELEmptyStream.Create('empty stream', 'stream');
end;


function TLStream.ReadCharacter(out ch: unicodechar; EoE: boolean): boolean;
var b: array[1..4] of byte;
begin
    result := false;
    if ReadBytes(b[1],1,EoE)<>1 then Exit;
    case encoding of
        seUTF8: begin
            case b[1] of
                {0xxxxxxx}$00..$7F: begin
                    ch := unicodechar(b[1]);
                end;
                {110xxxxx}$C0..$DF: begin
                    ReadBytes(b[2],1,true);
                    ch := unicodechar(64*(b[1] and 31)
                                       + (b[2] and 63));
                end;
                {1110xxxx}$E0..$EF: begin
                    ReadBytes(b[2],2,true);
                    ch := unicodechar(64*64*(b[1] and 15)
                                       + 64*(b[2] and 63)
                                          + (b[3] and 63));
                end;
                {11110xxx}$F0..$F7: begin
                    ReadBytes(b[2],3,true);
                    ch := unicodechar(64*64*64*(b[1] and 7)
                                       + 64*64*(b[2] and 63)
                                          + 64*(b[3] and 63)
                                             + (b[4] and 63));
                end;
                else ch := '?';
            end;
        end;
        seUTF16LE: begin
            ReadBytes(b[2],1,true);
            ch := unicodechar(b[1]+b[2]*256);
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF16BE: begin
            ReadBytes(b[2],1,true);
            ch := unicodechar(256*b[1]+b[2]);
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF32LE: begin
            ReadBytes(b[2],3,true);
            ch := unicodechar(b[1]+b[2]*256+b[3]*256*256+b[4]*256*256*256);
        end;
        seUTF32BE: begin
            ReadBytes(b[2],3,true);
            ch := unicodechar(256*256*256*b[1]+256*256*b[2]+256*b[3]+b[4]);
        end;
        seCP1251: ch := cp1251_cp[b[1]];
        seCP1252: ch := cp1252_cp[b[1]];
        seCP866:  ch := cp866_cp[b[1]];
        seKOI8R:  ch := KOI8_R_cp[b[1]];
        seLatin1: ch := Latin1_cp[b[1]];
        else raise ECharsetError.Create('');
    end;
    result := true;
end;

function TLStream.read_byte: byte;
begin
    ReadBytes(result, 1, true)
end;


function TLStream.read_bytes(var bb: TBytes; count: integer): boolean;
const bs = 4096;
var bc: integer;
begin
    if count>=0
    then begin
        SetLength(bb, count);
        ReadBytes(bb[0], count, true);
    end
    else begin
        SetLength(bb, 0);
        repeat
            SetLength(bb, Length(bb)+bs);
            bc := ReadBytes(bb[Length(bb)-bs], bs);
        until bc<bs;
        SetLength(bb, Length(bb)-bs+bc);
    end;
end;

function TLStream.read_DWORD: DWORD;
var bb: array[1..4] of byte;
begin
    ReadBytes(bb, 4, true);
    result := bb[1]+bb[2]*256+bb[3]*256*256+bb[4]*256*256*256;
end;

function TLStream.read_single: single;
var d: record case byte of 0: (s: single); 1: (b: array [1..SizeOf(single)] of Byte); end;
begin
    ReadBytes(d.b[1], SizeOf(single), true);
    result := d.s;
end;

function TLStream.read_double: double;
var d: record case byte of 0: (d: double); 1: (b: array [1..SizeOf(double)] of Byte); end;
begin
    ReadBytes(d.b[1], SizeOf(double), true);
    result := d.d;
end;


function TLStream.read_character: unicodechar;
begin
    ReadCharacter(result, true);
end;

function TLStream.read_character(out ch: unicodechar): boolean;
begin
    result := ReadCharacter(ch, false);
end;

function TLStream.read_string(count: integer): unicodestring;
var i: integer; ch: unicodechar;
begin
    result := '';
    if count>=0
    then for i := 1 to count do begin ReadCharacter(ch, true); result := result + ch end
    else while ReadCharacter(ch, false) do result := result + ch;
end;


function TLStream.read_line(ls: unicodestring): unicodestring;
var ch: unicodechar; ln: unicodestring;
begin
    ln := '';
    while ReadCharacter(ch, false) do begin
        ln := ln + ch;
        if ln[Length(ln)-Length(ls)+1..Length(ln)]=ls
        then begin
            result := ln[1..Length(ln)-Length(ls)];
            Exit;
        end;
    end;
    result := ln;
    if result='' then raise ELEmptyStream.Create('empty stream', 'stream');
end;

function TLStream.read_line(out ln: unicodestring; ls: unicodestring): boolean;
var ch: unicodechar;
begin
    result := true;
    ln := '';
    while ReadCharacter(ch, false) do begin
        ln := ln + ch;
        if ln[Length(ln)-Length(ls)+1..Length(ln)]=ls
        then begin
            ln := ln[1..Length(ln)-Length(ls)];
            Exit;
        end;
    end;
    if ln='' then result := false;
end;


function TLStream.WriteBytes(const Buffer; count: integer; EoE: boolean
    ): integer;
begin
    if stream=nil then raise ELE.Create('writing to closed stream', 'stream');
    result := stream.Write(Buffer, count);
    if EoE and (result<>count) then raise ELEmptyStream.Create('write error', 'stream');
end;

procedure TLStream.write_byte(b: byte);
begin
    WriteBytes(b, 1, true);
end;

procedure TLStream.write_bytes(bb: TBytes);
begin
    WriteBytes(bb[0], Length(bb), true);
end;


procedure TLStream.write_character(ch: unicodechar);
    procedure write(b: byte); inline; begin WriteBytes(b, 1, true); end;
    procedure write8bit(const codepage: TCodePage);
    var i: byte;
    begin
        for i := 0 to 255 do
            if codepage[i]=ch then begin
                write(i);
                exit;
            end;
        write(ord('?'));
    end;
var cp: cardinal;
begin
    cp := ord(ch);
    case encoding of
        seUTF8: case cp of
            0..127: begin
                write(cp);
            end;
            128..2047: begin
                write((cp shr 6) or 192);
                write((cp and 63) or 128);
            end;
            2048..65535: begin
                write((cp shr 12) or 224);
                write(((cp shr 6) and 63) or 128);
                write((cp and 63) or 128);
            end;
            65536..2097152: begin
                write((cp shr 18) or 240);
                write(((cp shr 12) and 63) or 128);
                write(((cp shr 6) and 63) or 128);
                write((cp and 63) or 128);
            end
            else raise ECharsetError.Create('Символ вне диапазона');
        end;
        seUTF16LE: begin
            write(cp and $FF);
            write(cp shr 8);
        end;
        seUTF16BE: begin
            write(cp shr 8);
            write(cp and $FF);
        end;
        seUTF32LE: begin
            write(cp and $FF);
            write((cp shr 8) and $FF);
            write((cp shr 16) and $FF);
            write((cp shr 24) and $FF);
        end;
        seUTF32BE: begin
            write((cp shr 24) and $FF);
            write((cp shr 16) and $FF);
            write((cp shr 8) and $FF);
            write(cp and $FF);
        end;
        seCP1251: write8bit(cp1251_cp);
        seCP1252: write8bit(cp1252_cp);
        seCP866:  write8bit(cp866_cp);
        seKOI8R:  write8bit(KOI8_R_cp);
        seLatin1: write8bit(Latin1_cp);
        else raise ECharsetError.Create('неизвестная кодировка');
    end;
end;


procedure TLStream.write_string(s: unicodestring);
var i: integer;
begin
    for i := 1 to Length(s) do write_character(s[i]);
end;

procedure TLStream.write_single(f: double);
var d: record case byte of 0: (f: single); 1: (b: array [1..SizeOf(single)] of Byte); end;
begin
    d.f := f;
    WriteBytes(d.b[1], SizeOf(single), true);
end;

procedure TLStream.write_double(f: double);
var d: record case byte of 0: (f: double); 1: (b: array [1..SizeOf(double)] of Byte); end;
begin
    d.f:=f;
    WriteBytes(d.b[1], SizeOf(double), true);
end;

procedure TLStream.close_stream;
begin
    FreeAndNil(stream);
end;

function TLStream.active: boolean;
begin
    result := stream <> nil;
end;

end.   //426 659

