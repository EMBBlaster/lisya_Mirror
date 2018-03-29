unit lisya_packages;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    LResources,
    Classes, SysUtils, dlisp_values, mar;

type

    { TPackage }

    TPackage = class
        name: unicodestring;
        uname: unicodestring;
        stack: TVSymbolStack;
        export_list: TVList;

        constructor Create;
        destructor Destroy; override;
    end;


procedure AddPackage(P: TPackage);
function FindPackage(name: unicodestring): TPackage;
procedure FreePackages;

function GetBuiltInPackageStream(name: unicodestring): TStream;
function FindLisyaFile(name: unicodestring): unicodestring;
//function FindPackageFile(name: unicodestring; out expr: TVList): boolean;

//TODO: в пакете хранится его стэк целиком (хотя он не нужен)
//и перечень экспортируемых переменных отдельно
//при каждом подключении пакета происходит поиск в стеке всего списка экспорта
//нужно удалить стек, а в список экспорта сохранить указатели

implementation

uses dlisp_read;

var packages: array of TPackage;

procedure AddPackage(P: TPackage);
var i: integer; emessage: unicodestring;
begin
    for i := 0 to high(packages) do
        if packages[i].uname = P.uname
        then begin
            emessage := 'package '+P.name+'redefinition';
            FreeAndNil(P);
            raise ELE.Create(emessage, 'syntax');
        end;
    SetLength(packages, Length(packages)+1);
    packages[high(packages)] := P;
end;

function FindPackage(name: unicodestring): TPackage;
var i: integer; uname: unicodestring;
begin
    uname := UpperCaseU(name);
    for i := 0 to high(packages) do begin
        if packages[i].uname = uname then begin
            result := packages[i];
            exit;
        end;
    end;
    result := nil;
end;

{ TPackage }

constructor TPackage.Create;
begin
    stack := nil;
    export_list := nil;
end;

destructor TPackage.Destroy;
begin
    stack.Free;
    export_list.Free;
    inherited Destroy;
end;

procedure FreePackages;
var i: integer;
begin
    for i := 0 to high(packages) do packages[i].Free;
    SetLength(packages, 0);
end;

function GetBuiltInPackageStream(name: unicodestring): TStream;
var i: integer;
begin
    result := nil;

    for i:=0 to lazarusResources.Count-1 do
        if lazarusResources.Items[i].Name=name then begin
            result := TLazarusResourceStream.CreateFromHandle(lazarusResources.Items[i]);
            Exit;
        end;
end;

function FindLisyaFile(name: unicodestring): unicodestring;
var oname,ename,rname, sdir: unicodestring; i: integer; path: TStringList;
begin
     oname := DirSep(name);
     ename := ChangeFileExt(DirSep(name),'.lisya');
     rname := ChangeFileExt(DirSep(name),'.лися');
     //поиск в текущей папке
     if FileExists(oname) then begin result := ExpandFileName(oname); Exit; end;
     if FileExists(ename) then begin result := ExpandFileName(ename); Exit; end;
     if FileExists(rname) then begin result := ExpandFileName(rname); Exit; end;
     //поиск в папке стартового скрипта
     if (ParamCount>0) then begin
        sdir := ExtractFilePath(ExpandFileName(paramStr(1)));
        result := sdir+ename; if FileExists(result) then Exit;
        result := sdir+rname; if FileExists(result) then Exit;
     end;
     //поиск в LISYA_PATH
     try
        path := TStringList.Create;
        path.Delimiter := {$IFDEF WINDOWS}';'{$ELSE}':'{$ENDIF};
        path.DelimitedText := GetEnvironmentVariable('LISYA_PATH');

    for i := 0 to path.Count-1 do begin
        sdir := path[i]+{$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF};
        result := sdir+ename; if FileExists(result) then Exit;
        result := sdir+rname; if FileExists(result) then Exit;
    end;
    finally
           path.Free;
    end;
     //поиск в папке с программой
     sdir := ExtractFilePath(paramstr(0));
     result := sdir+ename; if FileExists(result) then Exit;
     result := sdir+rname; if FileExists(result) then Exit;

    result := '';
end;

initialization
    {$i packages.lrs}

finalization
    FreePackages;


end.

