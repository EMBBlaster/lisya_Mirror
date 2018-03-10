unit lisya_zip;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, zstream, crc,
    mar,
    lisia_charset;

type
    EZIP = class (Exception);

type

    TLFH = record  //LOCAL FILE HEADER
    private
        versionToExtract:       WORD;   // 20 - требуется для DEFLATE
        generalPurposeBitFlag:  WORD;   // bit 0 - файл зашифрован
                                        // bit 3 - потоковый режим
                                        // bit 11 - имя в UTF-8 без BOM
        compressionMethod:      WORD;
        modificationTime:       WORD;   // MS-DOS формат
        modificationDate:       WORD;   // MS-DOS формат
        crc32:                  DWORD;
        compressedSize:         DWORD;
        uncompressedSize:       DWORD;
        filenameLength:         WORD;
        extraFieldLength:       WORD;
        filename:               unicodestring;
        extraField:             TBytes;
        data:                   TBytesStream;
    end;

    TDDR = record // STREAMING DATA DESCRIPTION
    private
        crc32:                  DWORD;
        compressedSize:         DWORD;
        uncompressedSize:       DWORD;
    end;


    TAED = record  // ARCHIVE EXTRA DATA
    private
        extraFieldLength:       DWORD;
        extraField:             TBytes;
    end;

    TCDFH = record // CENTRAL DIRECTORY FILE HEADER
    private
        versionMadeBy:          WORD;   // $00 MS-DOS атрибуты файла должны быть совместимы
        versionToExtract:       WORD;
        generalPurposeBitFlag:  WORD;
        compressionMethod:      WORD;
        modificationTime:       WORD;
        modificationDate:       WORD;
        crc32:                  DWORD;
        compressedSize:         DWORD;
        uncompressedSize:       DWORD;
        filenameLength:         WORD;
        extraFieldLength:       WORD;
        fileCommentLength:      WORD;
        diskNumber:             WORD;   //номер диска на котором начинается этот файл
        internalFileAttributes: WORD;  // bit 0 - текстовый файл
        externalFileAttributes: DWORD;
        localFileHeaderOffset:  DWORD;  //смещение от начала диска до LFH
        filename:               unicodestring;
        extraField:             TBytes;
        fileComment:            unicodestring;
    end;


    TEOCD = record  // END OF CENTRAL DIRECTORY
    private
        diskNumber:             WORD;   // номер диска на котором находится EOCD
        startDiskNumber:        WORD;   // номер диска с которого начинается CD
        numberCentralDirectoryRecord:   WORD; // количество CDFH на этом диске
        totalCentralDirectoryRecord:    WORD; // всего файлов
        sizeOfCentralDirectory:         DWORD;  //включая AED и CDFH
        centralDirectoryOffset:         DWORD; // смещение от начала диска до CD
        commentLength:          WORD;
        comment:                unicodestring;
    end;


    { TZipFile }

    TZipFile = record
    private
        lfh: array of TLFH;
        ddr: array of TDDR;
        aed: TAED;
        cdfh: array of TCDFH;
        eocd: TEOCD;
        f: TFileStream;
    public
        procedure Open(fn: unicodestring; mode: word; encoding: TStreamEncoding = seUTF8);
        function FileList: TStringArray;
        procedure Close(rollback: boolean = false);
        function GetFileStream(fn: unicodestring): TStream;
    end;
    PZipFile = ^TZipFile;

implementation

const lfh_sign  = $04034B50;
const aed_sign  = $08064B50;
const cdfh_sign = $02014B50;
const eocd_sign = $06054B50;
const ddr_sign  = $08074B50;

function read_string(f: TFileStream; count: integer; encoding: TStreamEncoding): unicodestring;
var i: integer; bb: TBytes;
begin
    SetLength(bb, count);
    for i := 0 to count-1 do bb[i] := f.ReadByte;
    result := lisia_charset.bytes_to_string(bb, encoding);
end;

procedure write_string(f: TFileStream; s: unicodestring);
var i: integer;
begin
    for i := 1 to length(s) do begin
        lisia_charset.write_character(f, s[i], seUTF8);
    end;
end;

function read_bytes(f: TStream; count: integer): TBytes;
var i: integer;
begin
    SetLength(result, count);
    for i := 0 to count-1 do result[i] := f.ReadByte;
end;

procedure write_bytes(f: TStream; bb: TBytes);
var i: integer;
begin
    for i := low(bb) to high(bb) do f.WriteByte(bb[i]);
end;

function read_to_stream(f: TStream; count: integer; crc: DWORD): TBytesStream;
var i: integer; b: byte;
begin
    result := TBytesStream.Create;
    result.SetSize(count);
    for i := 1 to count do result.WriteByte(f.ReadByte);
    if crc32(0,@(result.Bytes[0]), count)<>crc
    then raise EZIP.Create('Нарушение контрольной суммы');
end;


function read_DDR(f: TFileStream): TDDR;
begin
    with result do begin
        crc32 :=                f.ReadDWord;
        compressedSize :=       f.ReadDWord;
        uncompressedSize :=     f.ReadDWord;
    end;
end;

function find_DDR(f: TFileStream): TDDR;
var p, i: cardinal; b1, b2, b3, b4, sb1, sb2, sb3, sb4: byte;
begin
//TODO: этот код полагается на уникальность сигнатуры и потому не надёжен
//но что поделать формат ZIP местами кривоват
    p := f.Position;
    sb1 := ddr_sign and $FF;
    sb2 := (ddr_sign and $FF00) shr 8;
    sb3 := (ddr_sign and $FF0000) shr 16;
    sb4 := (ddr_sign and $FF000000) shr 24;

    b1 := f.ReadByte;
    b2 := f.ReadByte;
    b3 := f.ReadByte;
    b4 := f.ReadByte;
    for i := p to f.Size-40 do begin
        if (b1=sb1) and (b2=sb2) and (b3=sb3) and (b4=sb4)
        then begin
            result := read_DDR(f);
            f.Position := p;
            Exit;
        end
        else begin
            b1 := b2;
            b2 := b3;
            b3 := b4;
            b4 := f.ReadByte;
        end;
    end;
    raise EZIP.Create('Не найден Data Description Record');
end;

function read_LFH(f: TFileStream; encoding: TStreamEncoding): TLFH;
var inflate: TDecompressionStream; p: integer; ddr: TDDR; enc: TStreamEncoding;
begin
    with result do begin
        versionToExtract :=         f.ReadWord;
        generalPurposeBitFlag :=    f.ReadWord;
        compressionMethod :=        f.ReadWord;
        modificationTime :=         f.ReadWord;
        modificationDate :=         f.ReadWord;
        crc32 :=                    f.ReadDWord;
        compressedSize :=           f.ReadDWord;
        uncompressedSize :=         f.ReadDWord;
        filenameLength :=           f.ReadWord;
        extraFieldLength :=         f.ReadWord;

        if (generalPurposeBitFlag and 2048)>0
        then enc := seUTF8 else enc := encoding;

        filename :=  read_string(f, filenameLength, enc);
        extraField := read_bytes(f, extraFieldLength);

        if (generalPurposeBitFlag and 8)>0
        then begin
            ddr := find_DDR(f);
            crc32 := ddr.crc32;
            compressedSize := ddr.compressedSize;
            uncompressedSize := ddr.uncompressedSize;
        end;

        case compressionMethod of
            0: begin
                //uncompressedData := read_bytes(f, uncompressedSize);
                data := read_to_stream(f, uncompressedSize, crc32);
            end;
            8: begin
                p := f.Position;
                inflate := TDecompressionStream.Create(f, true);
                //uncompressedData := read_bytes(inflate, uncompressedSize);
                data := read_to_stream(inflate, uncompressedSize, crc32);
                inflate.Free;
                f.Position := p + compressedSize;
            end;
            else raise EZIP.Create('Неподдерживаемый метод сжатия');
        end;
    end;
end;


procedure write_LFH(f: TFileStream; var lfh: TLFH; var cdfh: TCDFH);
var p_cs, p_db, p_de, p_fnl, p_fnb, p_fne: DWORD; deflate: TCompressionStream;
begin
    if lfh.versionToExtract<20 then lfh.versionToExtract:=20;
    lfh.generalPurposeBitFlag := 2048; //имя файла в UTF-8
    if lfh.data.Size<200
    then lfh.compressionMethod:= 0
    else lfh.compressionMethod:= 8;
    lfh.crc32 := crc32(0, @(lfh.data.Bytes[0]), lfh.data.Size);
    lfh.uncompressedSize := lfh.data.Size;

    cdfh.localFileHeaderOffset := f.Position-4;


    f.WriteWord(lfh.versionToExtract);
    f.WriteWord(lfh.generalPurposeBitFlag);
    f.WriteWord(lfh.compressionMethod);
    f.WriteWord(lfh.modificationTime);
    f.WriteWord(lfh.modificationDate);
    f.WriteDWord(lfh.crc32);
    p_cs := f.Position;
    f.WriteDWord(0);
    f.WriteDWord(lfh.uncompressedSize);
    p_fnl := f.Position;
    f.WriteWord(0);
    f.WriteWord(0);//f.WriteWord(lfh.extraFieldLength);
    p_fnb := f.Position;
    write_string(f, lfh.filename);
    p_fne := f.Position;
    //write_bytes(f, lfh.extraField);
    p_db := f.Position;
    case lfh.compressionMethod of
        0: begin
            lfh.data.SaveToStream(f);
        end;
        8: begin
            deflate := TCompressionStream.Create(cldefault, f, true);
            lfh.data.SaveToStream(f);
            deflate.Free;
        end;
    end;
    p_de := f.Position;
    f.Position := p_cs;
    lfh.compressedSize := p_de - p_db;
    f.WriteDWord(lfh.compressedSize);
    f.Position := p_fnl;
    lfh.filenameLength := p_fne - p_fnb;
    f.WriteWord(lfh.filenameLength);
    f.Position := p_de;

    cdfh.versionToExtract:=lfh.versionToExtract;
    cdfh.generalPurposeBitFlag:=lfh.generalPurposeBitFlag;
    cdfh.compressionMethod:=lfh.compressionMethod;
    cdfh.modificationTime:=lfh.modificationTime;
    cdfh.modificationDate:=lfh.modificationDate;
    cdfh.crc32:=lfh.crc32;
    cdfh.compressedSize:=lfh.compressedSize;
    cdfh.uncompressedSize:=lfh.uncompressedSize;
    cdfh.filenameLength:=lfh.filenameLength;


    cdfh.filename:=lfh.filename;
    cdfh.diskNumber:=0;
end;

function read_AED(f: TFileStream): TAED;
begin
    with result do begin
        extraFieldLength :=     f.ReadDWord;
        extraField := read_bytes(f, extraFieldLength);
    end;
end;

procedure write_AED(f: TFileStream; aed: TAED);
begin
    f.WriteDWord(aed.extraFieldLength);
    write_bytes(f, aed.extraField);
end;

function read_CDFH(f: TFileStream; encoding: TStreamEncoding): TCDFH;
var enc: TStreamEncoding;
begin
    with result do begin
        versionMadeBy :=            f.ReadWord;
        versionToExtract :=         f.ReadWord;
        generalPurposeBitFlag :=    f.ReadWord;
        compressionMethod :=        f.ReadWord;
        modificationTime :=         f.ReadWord;
        modificationDate :=         f.ReadWord;
        crc32 :=                    f.ReadDWord;
        compressedSize :=           f.ReadDWord;
        uncompressedSize :=         f.ReadDWord;
        filenameLength :=           f.ReadWord;
        extraFieldLength :=         f.ReadWord;
        fileCommentLength :=        f.ReadWord;
        diskNumber :=               f.ReadWord;
        internalFileAttributes :=   f.ReadWord;
        externalFileAttributes :=   f.ReadDWord;
        localFileHeaderOffset :=    f.ReadDWord;

        if (generalPurposeBitFlag and 2048)>0
        then enc := seUTF8 else enc := encoding;

        filename := read_string(f, filenameLength, enc);
        extraField := read_bytes(f, extraFieldLength);
        fileComment := read_string(f, fileCommentLength, enc);
    end;
end;

procedure write_CDFH(f: TFileStream; var cdfh: TCDFH);
var p_fcl, p_fcb, p_fce: cardinal;
begin
    f.WriteWord(cdfh.versionMadeBy);
    f.WriteWord(cdfh.versionToExtract);
    f.WriteWord(cdfh.generalPurposeBitFlag);
    f.WriteWord(cdfh.compressionMethod);
    f.WriteWord(cdfh.modificationTime);
    f.WriteWord(cdfh.modificationDate);
    f.WriteDWord(cdfh.crc32);
    f.WriteDWord(cdfh.compressedSize);
    f.WriteDWord(cdfh.uncompressedSize);
    f.WriteWord(cdfh.filenameLength);
    f.WriteWord(0);//f.WriteWord(cdfh.extraFieldLength);
    p_fcl := f.Position;
    f.WriteWord(0);
    f.WriteWord(cdfh.diskNumber);
    f.WriteWord(cdfh.internalFileAttributes);
    f.WriteDWord(cdfh.externalFileAttributes);
    f.WriteDWord(cdfh.localFileHeaderOffset);
    write_string(f, cdfh.filename);
    //write_bytes(f, cdfh.extraField);
    p_fcb := f.Position;
    write_string(f, cdfh.fileComment);
    p_fce := f.Position;

    f.Position := p_fcl;
    cdfh.fileCommentLength := p_fce - p_fcb;
    f.WriteWord(cdfh.fileCommentLength);
    f.Position := p_fce;
end;

function read_EOCD(f: TFileStream; encoding: TStreamEncoding): TEOCD;
begin
    with result do begin
        diskNumber :=               f.ReadWord;
        startDiskNumber :=          f.ReadWord;
        numberCentralDirectoryRecord := f.ReadWord;
        totalCentralDirectoryRecord := f.ReadWord;
        sizeOfCentralDirectory :=   f.ReadDWord;
        centralDirectoryOffset :=   f.ReadDWord;
        commentLength := f.ReadWord;
        comment := read_string(f, commentLength, encoding);
    end;
end;

procedure write_EOCD(f: TFileStream; var eocd: TEOCD);
var p_cl, p_cb, p_ce: cardinal;
begin
    eocd.diskNumber := 0;
    eocd.startDiskNumber := 0;
    eocd.numberCentralDirectoryRecord := eocd.totalCentralDirectoryRecord;

    f.WriteWord(eocd.diskNumber);
    f.WriteWord(eocd.startDiskNumber);
    f.WriteWord(eocd.numberCentralDirectoryRecord);
    f.WriteWord(eocd.totalCentralDirectoryRecord);
    f.WriteDWord(eocd.sizeOfCentralDirectory);
    f.WriteDWord(eocd.centralDirectoryOffset);
    p_cl := f.Position;
    f.WriteWord(0);
    p_cb := f.Position;
    write_string(f, eocd.comment);
    p_ce := f.Position;

    f.Position := p_cl;
    eocd.commentLength := p_ce - p_cb;
    f.WriteWord(eocd.commentLength);
    f.Position := p_ce;
end;

{ TZipFile }

procedure TZipFile.Open(fn: unicodestring; mode: word; encoding: TStreamEncoding
    );
var sign: DWORD;
begin
    f := TFileStream.Create(fn, mode);
    SetLength(lfh, 0);
    SetLength(cdfh, 0);
    SetLength(ddr, 0);
    aed.extraFieldLength := 0;

    if (mode=fmOpenRead) or (mode=fmOpenReadWrite) then
    repeat
        sign := f.ReadDWord;
        case sign of
            lfh_sign: begin
                SetLength(lfh, Length(lfh)+1);
                lfh[high(lfh)] := read_LFH(f, encoding);
            end;
            ddr_sign: begin
                SetLength(ddr, Length(ddr)+1);
                ddr[high(ddr)] := read_DDR(f);
            end;
            cdfh_sign: begin
                SetLength(cdfh, Length(cdfh)+1);
                cdfh[high(cdfh)] := read_CDFH(f, encoding);
            end;
            aed_sign: begin
                aed := read_AED(f)
            end;
            eocd_sign: begin
                eocd := read_EOCD(f, encoding);
                Break;
            end;
            else raise EZIP('Повреждённый архив '+fn);
        end;
    until false;
    if mode = fmOpenRead then FreeAndNil(f);
end;

function TZipFile.FileList: TStringArray;
var i: integer;
begin
    SetLength(result, Length(lfh));
    for i := 0 to high(lfh) do result[i] := lfh[i].filename;
end;

procedure TZipFile.Close(rollback: boolean);
var i: integer;
begin
    if f<>nil then begin
        f.Position:=0;
        for i := 0 to high(lfh) do begin
            f.WriteDWord(lfh_sign);
            write_lfh(f, lfh[i], cdfh[i]);
        end;
        eocd.centralDirectoryOffset := f.Position;
        eocd.totalCentralDirectoryRecord := Length(lfh);
        if aed.extraFieldLength>0 then begin
            f.WriteDWord(aed_sign);
            write_AED(f, aed);
        end;
        for i := 0 to high(cdfh) do begin
            f.WriteDWord(cdfh_sign);
            write_cdfh(f, cdfh[i]);
        end;
        eocd.sizeOfCentralDirectory := f.Position - eocd.centralDirectoryOffset;
        f.WriteDWord(eocd_sign);
        write_EOCD(f, eocd);

        f.Size:=f.Position;
        FreeAndNil(f);
    end;

    for i := 0 to high(lfh) do FreeAndNil(lfh[i].data);
end;

function TZipFile.GetFileStream(fn: unicodestring): TStream;
var i, n: integer; uname: unicodestring;
begin
    result := nil;
    uname := UpperCaseU(fn);
    for i := 0 to high(lfh) do
        if uname = UpperCaseU(lfh[i].filename) then begin
            result := lfh[i].data;
            lfh[i].data.Position := 0;
            Exit;
        end;

    SetLength(lfh, Length(lfh)+1);
    SetLength(cdfh, Length(cdfh)+1);
    n := high(lfh);
    lfh[n].filename:=fn;
    lfh[n].data := TBytesStream.Create;

    cdfh[n].versionMadeBy := $031E; // OS UNIX / версия 3.0;
    cdfh[n].externalFileAttributes := $81A40000; // -rw-r--r--
    cdfh[n].filename:=fn;
    cdfh[n].fileComment:='';

    result := lfh[n].data;
end;



end.

