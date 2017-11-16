unit lisya_packages;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_values, mar;

type

    { TPackage }

    TPackage = class
        name: unicodestring;
        uname: unicodestring;
        stack: TVSymbolStack;
        export_list: TVList;

        constructor Create;
        destructor Destroy;
    end;


procedure AddPackage(P: TPackage);
function FindPackage(name: unicodestring): TPackage;

//TODO: в пакете хранится его стэк целиком (хотя он не нужен)
//и перечень экспортируемых переменных отдельно
//при каждом подключении пакета происходит поиск в стеке всего списка экспорта
//нужно удалить стек, а в список экспорта сохранить указатели

implementation

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
end;

procedure FreePackages;
var i: integer;
begin
    for i := 0 to high(packages) do packages[i].Free;
    SetLength(packages, 0);
end;

finalization
    FreePackages;


end.

