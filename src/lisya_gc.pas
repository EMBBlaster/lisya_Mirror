unit lisya_gc;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, dlisp_values, lisya_predicates;


function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
function RefVariable(P: PVariable): PVariable;
procedure ReleaseVariable(var P: PVariable);


implementation


function ReleaseRecursiveProcedure(var P: PVariable): boolean;
var vars, links: array of PVariable;
    i, j, c: integer;
    clear: boolean;
    function registered_node(P: PVariable): boolean;
    var i: integer;
    begin
        result := true;
        for i := 0 to high(vars) do if vars[i]=pointer(P) then Exit;
        result := false;
    end;

    procedure add_link(P: PVariable);
    begin
        SetLength(links, Length(links)+1);
        links[high(links)]:=P;
    end;

    procedure add_node(P: PVariable);
    var proc: TVProcedure; i: integer;
    begin
        if not registered_node(P) then begin
            SetLength(vars, length(vars)+1);
            vars[high(vars)]:=P;

            proc := P.V as TVProcedure;
            for i:=0 to high(proc.stack.stack) do begin
                if (proc.stack.stack[i].V<>nil) and tpProcedure(proc.stack.stack[i].V.V)
                then begin
                    add_node(proc.stack.stack[i].V);
                    add_link(proc.stack.stack[i].V);
                end;
            end;
        end;
    end;

begin
    result := false;

    if not tpProcedure(P.V) then Exit;

    add_link(P);
    add_node(P);

    clear := true;
    for i := 0 to high(vars) do begin
        c := 0;
        for j := 0 to high(links) do if vars[i] = links[j] then Inc(c);

        clear := c=vars[i].ref_count;
        if not clear then Break;

        if c>vars[i].ref_count then WriteLn('WARNING: нарушение ссылочной целостности');
    end;

    if clear then begin
        for i := 0 to high(vars) do vars[i].ref_count:=-1;
        for i := 0 to high(vars) do vars[i].V.Free;
        for i := 0 to high(vars) do Dispose(vars[i]);
    end;

    result := clear;
end;

{ TVariable }

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
begin
    New(result);
    result.ref_count:=1;
    result.constant := _constant;
    result.V := _V;
end;

function RefVariable(P: PVariable): PVariable;
begin
    result := P;
    if P<>nil then Inc(P.ref_count);
end;


procedure ReleaseVariable(var P: PVariable);
begin
    try
        if P=nil then Exit;
        if P.ref_count=1 then begin P.V.Free; Dispose(P); Exit; end;
        if tpProcedure(P.V) and ReleaseRecursiveProcedure(P) then Exit;
        Dec(P.ref_count);
    finally
        P := nil;
    end;
end;



initialization

finalization

end.

