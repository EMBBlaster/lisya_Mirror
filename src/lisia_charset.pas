unit lisia_charset;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils;

type
    TStreamEncoding = (seCP1251, seCP1252, seCP866, seKOI8R,
                       seBOM, seUTF8,  seUTF16BE, seUTF16LE, seUTF32BE, seUTF32LE );

type
    ECharsetError = class(Exception);

type
    TCodePage = array[byte] of unicodechar;

const cp1251_cp: TCodePage = (
#$00,   #$01,   #$02,   #$03,   #$04,   #$05,   #$06,   #$07,   #$08,   #$09,   #$0A,   #$0B,   #$0C,   #$0D,   #$0E,   #$0F,
#$10,   #$11,   #$12,   #$13,   #$14,   #$15,   #$16,   #$17,   #$18,   #$19,   #$1A,   #$1B,   #$1C,   #$1D,   #$1E,   #$1F,
#$20,   #$21,   #$22,   #$23,   #$24,   #$25,   #$26,   #$27,   #$28,   #$29,   #$2A,   #$2B,   #$2C,   #$2D,   #$2E,   #$2F,
#$30,   #$31,   #$32,   #$33,   #$34,   #$35,   #$36,   #$37,   #$38,   #$39,   #$3A,   #$3B,   #$3C,   #$3D,   #$3E,   #$3F,
#$40,   #$41,   #$42,   #$43,   #$44,   #$45,   #$46,   #$47,   #$48,   #$49,   #$4A,   #$4B,   #$4C,   #$4D,   #$4E,   #$4F,
#$50,   #$51,   #$52,   #$53,   #$54,   #$55,   #$56,   #$57,   #$58,   #$59,   #$5A,   #$5B,   #$5C,   #$5D,   #$5E,   #$5F,
#$60,   #$61,   #$62,   #$63,   #$64,   #$65,   #$66,   #$67,   #$68,   #$69,   #$6A,   #$6B,   #$6C,   #$6D,   #$6E,   #$6F,
#$70,   #$71,   #$72,   #$73,   #$74,   #$75,   #$76,   #$77,   #$78,   #$79,   #$7A,   #$7B,   #$7C,   #$7D,   #$7E,   #$7F,

#$402,  #$403,  #$201A, #$453,  #$201E, #$2026, #$2020, #$2021, #$20AC, #$2030, #$409,  #$2039, #$40A,  #$40C,  #$40B,  #$40F,
#$452,  #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014, #$003F, #$2122, #$459,  #$203A, #$45A,  #$45C,  #$45B,  #$45F,
#$A0,   #$40E,  #$45E,  #$408,  #$A4,   #$490,  #$A6,   #$A7,   #$401,  #$A9,   #$404,  #$AB,   #$AC,   #$AD,   #$AE,   #$407,
#$B0,   #$B1,   #$406,  #$456,  #$491,  #$B5,   #$B6,   #$B7,   #$451,  #$2116, #$454,  #$BB,   #$458,  #$405,  #$455,  #$457,
#$410,  #$411,  #$412,  #$413,  #$414,  #$415,  #$416,  #$417,  #$418,  #$419,  #$41A,  #$41B,  #$41C,  #$41D,  #$41E,  #$41F,
#$420,  #$421,  #$422,  #$423,  #$424,  #$425,  #$426,  #$427,  #$428,  #$429,  #$42A,  #$42B,  #$42C,  #$42D,  #$42E,  #$42F,
#$430,  #$431,  #$432,  #$433,  #$434,  #$435,  #$436,  #$437,  #$438,  #$439,  #$43A,  #$43B,  #$43C,  #$43D,  #$43E,  #$43F,
#$440,  #$441,  #$442,  #$443,  #$444,  #$445,  #$446,  #$447,  #$448,  #$449,  #$44A,  #$44B,  #$44C,  #$44D,  #$44E,  #$44F);

const cp1252_cp: TCodePage = (
#$00,   #$01,   #$02,   #$03,   #$04,   #$05,   #$06,   #$07,   #$08,   #$09,   #$0A,   #$0B,   #$0C,   #$0D,   #$0E,   #$0F,
#$10,   #$11,   #$12,   #$13,   #$14,   #$15,   #$16,   #$17,   #$18,   #$19,   #$1A,   #$1B,   #$1C,   #$1D,   #$1E,   #$1F,
#$20,   #$21,   #$22,   #$23,   #$24,   #$25,   #$26,   #$27,   #$28,   #$29,   #$2A,   #$2B,   #$2C,   #$2D,   #$2E,   #$2F,
#$30,   #$31,   #$32,   #$33,   #$34,   #$35,   #$36,   #$37,   #$38,   #$39,   #$3A,   #$3B,   #$3C,   #$3D,   #$3E,   #$3F,
#$40,   #$41,   #$42,   #$43,   #$44,   #$45,   #$46,   #$47,   #$48,   #$49,   #$4A,   #$4B,   #$4C,   #$4D,   #$4E,   #$4F,
#$50,   #$51,   #$52,   #$53,   #$54,   #$55,   #$56,   #$57,   #$58,   #$59,   #$5A,   #$5B,   #$5C,   #$5D,   #$5E,   #$5F,
#$60,   #$61,   #$62,   #$63,   #$64,   #$65,   #$66,   #$67,   #$68,   #$69,   #$6A,   #$6B,   #$6C,   #$6D,   #$6E,   #$6F,
#$70,   #$71,   #$72,   #$73,   #$74,   #$75,   #$76,   #$77,   #$78,   #$79,   #$7A,   #$7B,   #$7C,   #$7D,   #$7E,   #$7F,

#$20AC, #$0000, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021, #$02C6, #$2030, #$0160, #$2039, #$0152, #$0000, #$017D, #$0000,
#$0000, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014, #$02DC, #$2122, #$0161, #$203A, #$0153, #$0000, #$017E, #$0178,
#$00A0, #$00A1, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7, #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
#$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7, #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
#$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7, #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
#$00D0, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$00D7, #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$00DD, #$00DE, #$00DF,
#$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7, #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
#$00F0, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00F7, #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$00FF );

const cp866_cp: TCodePage = (
#$00,   #$01,   #$02,   #$03,   #$04,   #$05,   #$06,   #$07,   #$08,   #$09,   #$0A,   #$0B,   #$0C,   #$0D,   #$0E,   #$0F,
#$10,   #$11,   #$12,   #$13,   #$14,   #$15,   #$16,   #$17,   #$18,   #$19,   #$1A,   #$1B,   #$1C,   #$1D,   #$1E,   #$1F,
#$20,   #$21,   #$22,   #$23,   #$24,   #$25,   #$26,   #$27,   #$28,   #$29,   #$2A,   #$2B,   #$2C,   #$2D,   #$2E,   #$2F,
#$30,   #$31,   #$32,   #$33,   #$34,   #$35,   #$36,   #$37,   #$38,   #$39,   #$3A,   #$3B,   #$3C,   #$3D,   #$3E,   #$3F,
#$40,   #$41,   #$42,   #$43,   #$44,   #$45,   #$46,   #$47,   #$48,   #$49,   #$4A,   #$4B,   #$4C,   #$4D,   #$4E,   #$4F,
#$50,   #$51,   #$52,   #$53,   #$54,   #$55,   #$56,   #$57,   #$58,   #$59,   #$5A,   #$5B,   #$5C,   #$5D,   #$5E,   #$5F,
#$60,   #$61,   #$62,   #$63,   #$64,   #$65,   #$66,   #$67,   #$68,   #$69,   #$6A,   #$6B,   #$6C,   #$6D,   #$6E,   #$6F,
#$70,   #$71,   #$72,   #$73,   #$74,   #$75,   #$76,   #$77,   #$78,   #$79,   #$7A,   #$7B,   #$7C,   #$7D,   #$7E,   #$7F,

#$0410, #$0411, #$0412, #$0413, #$0414, #$0415, #$0416, #$0417, #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E, #$041F,
#$0420, #$0421, #$0422, #$0423, #$0424, #$0425, #$0426, #$0427, #$0428, #$0429, #$042A, #$042B, #$042C, #$042D, #$042E, #$042F,
#$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437, #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F,
#$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556, #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
#$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F, #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
#$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B, #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
#$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447, #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$044F,
#$0401, #$0451, #$0404, #$0454, #$0407, #$0457, #$040E, #$045E, #$00B0, #$2219, #$00B7, #$221A, #$2116, #$00A4, #$25A0, #$00A0);

const KOI8_R_cp: TCodePage = (
#$00,   #$01,   #$02,   #$03,   #$04,   #$05,   #$06,   #$07,   #$08,   #$09,   #$0A,   #$0B,   #$0C,   #$0D,   #$0E,   #$0F,
#$10,   #$11,   #$12,   #$13,   #$14,   #$15,   #$16,   #$17,   #$18,   #$19,   #$1A,   #$1B,   #$1C,   #$1D,   #$1E,   #$1F,
#$20,   #$21,   #$22,   #$23,   #$24,   #$25,   #$26,   #$27,   #$28,   #$29,   #$2A,   #$2B,   #$2C,   #$2D,   #$2E,   #$2F,
#$30,   #$31,   #$32,   #$33,   #$34,   #$35,   #$36,   #$37,   #$38,   #$39,   #$3A,   #$3B,   #$3C,   #$3D,   #$3E,   #$3F,
#$40,   #$41,   #$42,   #$43,   #$44,   #$45,   #$46,   #$47,   #$48,   #$49,   #$4A,   #$4B,   #$4C,   #$4D,   #$4E,   #$4F,
#$50,   #$51,   #$52,   #$53,   #$54,   #$55,   #$56,   #$57,   #$58,   #$59,   #$5A,   #$5B,   #$5C,   #$5D,   #$5E,   #$5F,
#$60,   #$61,   #$62,   #$63,   #$64,   #$65,   #$66,   #$67,   #$68,   #$69,   #$6A,   #$6B,   #$6C,   #$6D,   #$6E,   #$6F,
#$70,   #$71,   #$72,   #$73,   #$74,   #$75,   #$76,   #$77,   #$78,   #$79,   #$7A,   #$7B,   #$7C,   #$7D,   #$7E,   #$7F,

#$2500, #$2502, #$250C, #$2510, #$2514, #$2518, #$251C, #$2524, #$252C, #$2534, #$253C, #$2580, #$2584, #$2588, #$258C, #$2590,
#$2591, #$2592, #$2593, #$2320, #$25A0, #$2219, #$221A, #$2248, #$2264, #$2265, #$00A0, #$2321, #$00B0, #$00B2, #$00B7, #$00F7,
#$2550, #$2551, #$2552, #$0451, #$2553, #$2554, #$2555, #$2556, #$2557, #$2558, #$2559, #$255A, #$255B, #$255C, #$255D, #$255E,
#$255F, #$2560, #$2561, #$0401, #$2562, #$2563, #$2564, #$2565, #$2566, #$2567, #$2568, #$2569, #$256A, #$256B, #$256C, #$00A9,
#$044E, #$0430, #$0431, #$0446, #$0434, #$0435, #$0444, #$0433, #$0445, #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E,
#$043F, #$044F, #$0440, #$0441, #$0442, #$0443, #$0436, #$0432, #$044C, #$044B, #$0437, #$0448, #$044D, #$0449, #$0447, #$044A,
#$042E, #$0410, #$0411, #$0426, #$0414, #$0415, #$0424, #$0413, #$0425, #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E,
#$041F, #$042F, #$0420, #$0421, #$0422, #$0423, #$0416, #$0412, #$042C, #$042B, #$0417, #$0428, #$042D, #$0429, #$0427, #$042A);


const BOM: unicodechar = #$FEFF;

function read_character(stream: TStream; encoding: TStreamEncoding): unicodechar;
procedure write_character(stream: TStream; ch: unicodechar;
                            encoding: TStreamEncoding = seUTF8);
procedure write_string(stream: TStream; s: unicodestring; encoding: TStreamEncoding = seUTF8);
function read_BOM(stream: TStream; default: TStreamEncoding = seUTF8): TStreamEncoding;

function bytes_to_string(const bytes: TBytes; encoding: TStreamEncoding = seUTF8): unicodestring;


implementation


function read_character(stream: TStream; encoding: TStreamEncoding): unicodechar;
    function read: byte; inline; begin result := stream.ReadByte; end;
var b1, b2, b3, b4: byte;
begin
    case encoding of
        seUTF8: begin
            b1 := read;
            case b1 of
                {0xxxxxxx}$00..$7F: begin
                    result := unicodechar(b1);
                end;
                {110xxxxx}$C0..$DF: begin
                    b2 := read;
                    result := unicodechar(64*(b1 and 31)
                                           + (b2 and 63));
                end;
                {1110xxxx}$E0..$EF: begin
                    b2 := read;
                    b3 := read;
                    result := unicodechar(64*64*(b1 and 15)
                                           + 64*(b2 and 63)
                                              + (b3 and 63));
                end;
                {11110xxx}$F0..$F7: begin
                    b2 := read;
                    b3 := read;
                    b4 := read;
                    result := unicodechar(64*64*64*(b1 and 7)
                                           + 64*64*(b2 and 63)
                                              + 64*(b3 and 63)
                                                 + (b4 and 63));
                end;
                else result := '?';
            end;
        end;
        seUTF16LE: begin
            b1 := read;
            b2 := read;
            result := unicodechar(b1+b2*256);
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF16BE: begin
            b1 := read;
            b2 := read;
            result := unicodechar(256*b1+b2);
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF32LE: begin
            b1 := read;
            b2 := read;
            b3 := read;
            b4 := read;
            result := unicodechar(b1+b2*256+b3*256*256+b4*256*256*256);
        end;
        seUTF32BE: begin
            b1 := read;
            b2 := read;
            b3 := read;
            b4 := read;
            result := unicodechar(256*256*256*b1+256*256*b2+256*b3+b4);
        end;
        seCP1251: result := cp1251_cp[read];
        seCP1252: result := cp1252_cp[read];
        seCP866:  result := cp866_cp[read];
        seKOI8R:  result := KOI8_R_cp[read];
        else raise ECharsetError.Create('');
    end;
end;

procedure write_character(stream: TStream; ch: unicodechar;
    encoding: TStreamEncoding);
    procedure write(b: byte); inline; begin stream.WriteByte(b); end;
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
        else raise ECharsetError.Create('неизвестная кодировка');
    end
end;

procedure write_string(stream: TStream; s: unicodestring;
    encoding: TStreamEncoding);
var i: integer;
begin
    for i := 1 to Length(s) do write_character(stream, s[i], encoding);
end;

function read_BOM(stream: TStream; default: TStreamEncoding): TStreamEncoding;
    function b: byte; inline; begin result := stream.ReadByte; end;
var p: integer;
begin
    p := stream.Position;
    //UTF-8
    if (b=$EF) and (b=$BB) and (b=$BF) then begin
        result := seUTF8;
        exit;
    end else stream.Seek(p,0);
    //UTF16BE
    if (b=$FE) and (b=$FF) then begin
        result := seUTF16BE;
        exit;
    end else stream.Seek(p,0);
    //UTF32BE
    if (b=$00) and (b=$00) and (b=$FE) and (b=$FF) then begin
        result := seUTF32BE;
        exit;
    end else stream.Seek(p,0);
    //UTF32LE
    if (b=$FF) and (b=$FE) and (b=$00) and (b=$00) then begin
        result := seUTF32LE;
        exit;
    end else stream.Seek(p,0);
    //UTF16LE
    if (b=$FF) and (b=$FE) then begin
        result := seUTF16LE;
        exit;
    end else stream.Seek(p,0);

    result := default;
end;


function bytes_to_string(const bytes: TBytes; encoding: TStreamEncoding
    ): unicodestring;
var stream: TBytesStream;
begin try
    stream := TBytesStream.Create(bytes);
    result := '';
    while stream.Position<Length(bytes) do
        result := result + read_character(stream, encoding);
finally
    stream.Free;
end;
end;


end. //319

