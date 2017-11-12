unit lisya_packages;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_values;

implementation

type
    TPackage = record
        body: TVList;
        name: unicodestring;
        exported: TVSymbolStack;
    end;

end.

