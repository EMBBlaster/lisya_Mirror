unit lisya_exceptions;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils;


type
    { ELisyaError }

    ELisyaError              = class (Exception)
        EClass: unicodestring;
        EStack: unicodestring;
        constructor InvalidParameters;
        constructor Create(msg: unicodestring; ec: unicodestring=''; es: unicodestring='');
        constructor Malformed(msg: unicodestring);
        constructor Restricted(msg: unicodestring);
        constructor Stream(msg: unicodestring);
        destructor Destroy; override;
    end;
    ELE                      = ELisyaError;
    EObjectNotCompound       = class (ELisyaError) end;
    EInvalidParameters       = class (ELisyaError) end;
    ELEmptyStream            = class (ELisyaError) end;

implementation

{ ELisyaError }

constructor ELisyaError.InvalidParameters;
begin
    inherited Create('invalid parameters');
end;

constructor ELisyaError.Create(msg: unicodestring; ec: unicodestring; es: unicodestring='');
begin
    inherited Create(msg);
    EClass := ec;
    EStack := es;
end;

constructor ELisyaError.Malformed(msg: unicodestring);
begin
    inherited Create('malformed '+msg);
    EClass := 'syntax';
end;

constructor ELisyaError.Restricted(msg: unicodestring);
begin
    inherited Create('restricted '+msg);
    EClass := 'syntax';
end;

constructor ELisyaError.Stream(msg: unicodestring);
begin
    inherited Create(msg);
    EClass := 'stream';
end;

destructor ELisyaError.Destroy;
begin
    EClass := '';
    EStack := '';
    inherited Destroy;
end;

end.

