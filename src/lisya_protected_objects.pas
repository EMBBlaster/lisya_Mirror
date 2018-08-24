unit lisya_protected_objects;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils
    , mar;

type

    { TQueue }

    TQueue = class (TCountingObject)
    private
        q: array of TObject;
    public
        constructor Create;
        destructor Destroy; override;
        function description: unicodestring; override;
        procedure push(V: TObject);
        function pop: TObject;
    end;

implementation

uses
    dlisp_values, lisya_gc;

{ TQueue }

constructor TQueue.Create;
begin
    inherited;
    q := nil;
end;

destructor TQueue.Destroy;
var i: integer;
begin
    for i := 0 to high(q) do q[i].Free;
    q := nil;
    inherited Destroy;
end;

function TQueue.description: unicodestring;
begin
    result := 'QUEUE '+IntToStr(Length(q))+'@'+mar.PointerToStr(self);
end;

procedure TQueue.push(V: TObject);
begin
    try
        lock;
        SetLength(q, length(q)+1);
        q[high(q)] := lisya_gc.separate(V as TValue);
    finally
        unlock;
    end;
end;

function TQueue.pop: TObject;
var i: integer;
begin
    try
        lock;
        if length(q)>0
        then begin
            result := q[0];
            for i := 1 to high(q) do q[i-1] := q[i];
            SetLength(q, Length(q)-1);
        end
        else result := TVList.Create;
    finally
        unlock;
    end;
end;

end.

