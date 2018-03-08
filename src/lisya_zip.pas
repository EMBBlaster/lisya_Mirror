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
        data:                   TMemoryStream;
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
        internalFileAttributes: DWORD;  // bit 0 - текстовый файл
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
        sizeOfCentralDirectory:         DWORD;  //включая AED CDFH и EOCD
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
        procedure Open(fn: unicodestring; mode: word);
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

function read_string(f: TFileStream; count: integer): unicodestring;
var i: integer;
begin
    result := '';
    for i := 1 to count do result := result + cp1251_cp[f.ReadByte];
end;

function read_bytes(f: TStream; count: integer): TBytes;
var i: integer;
begin
    SetLength(result, count);
    for i := 0 to count-1 do result[i] := f.ReadByte;
end;

function read_to_stream(f: TStream; count: integer; crc: DWORD): TMemoryStream;
var i: integer; acc: DWORD; b: byte;
begin
    result := TMemoryStream.Create;
    result.SetSize(count);
    acc := 0;
    for i := 1 to count do begin
        b := f.ReadByte;
        acc := crc32(acc, @b, 1);
        result.WriteByte(b);
    end;
    if acc<>crc then raise EZIP.Create('Нарушение контрольной суммы');
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

function read_LFH(f: TFileStream): TLFH;
var inflate: TDecompressionStream; p: integer; ddr: TDDR;
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
        filename :=  read_string(f, filenameLength);
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


function read_AED(f: TFileStream): TAED;
begin
    with result do begin
        extraFieldLength :=     f.ReadDWord;
        extraField := read_bytes(f, extraFieldLength);
    end;
end;

function read_CDFH(f: TFileStream): TCDFH;
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
        filename := read_string(f, filenameLength);
        extraField := read_bytes(f, extraFieldLength);
        fileComment := read_string(f, fileCommentLength);
    end;
end;


function read_EOCD(f: TFileStream): TEOCD;
begin
    with result do begin
        diskNumber :=               f.ReadWord;
        startDiskNumber :=          f.ReadWord;
        numberCentralDirectoryRecord := f.ReadWord;
        totalCentralDirectoryRecord := f.ReadWord;
        sizeOfCentralDirectory :=   f.ReadDWord;
        centralDirectoryOffset :=   f.ReadDWord;
        commentLength := f.ReadWord;
        comment := read_string(f, commentLength);
    end;
end;

{ TZipFile }

procedure TZipFile.Open(fn: unicodestring; mode: word);
var sign: DWORD;
begin
    f := TFileStream.Create(fn, mode);
    SetLength(lfh, 0);
    SetLength(cdfh, 0);
    SetLength(ddr, 0);

    repeat
        sign := f.ReadDWord;
        case sign of
            lfh_sign: begin
                SetLength(lfh, Length(lfh)+1);
                lfh[high(lfh)] := read_LFH(f);
            end;
            ddr_sign: begin
                //raise EZIP.Create('Неподдерживаемый формат');
                SetLength(ddr, Length(ddr)+1);
                ddr[high(ddr)] := read_DDR(f);
            end;
            cdfh_sign: begin
                SetLength(cdfh, Length(cdfh)+1);
                cdfh[high(cdfh)] := read_CDFH(f);
            end;
            aed_sign: begin
                aed := read_AED(f)
            end;
            eocd_sign: begin
                eocd := read_EOCD(f);
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
    for i := 0 to high(lfh) do FreeAndNil(lfh[i].data);
    if f<>nil then FreeAndNil(f);
end;

function TZipFile.GetFileStream(fn: unicodestring): TStream;
var i: integer; uname: unicodestring;
begin
    result := nil;
    uname := UpperCaseU(fn);
    for i := 0 to high(lfh) do
        if uname = UpperCaseU(lfh[i].filename) then begin
            result := lfh[i].data;
            lfh[i].data.Position := 0;
            Exit;
        end;
end;




end.

