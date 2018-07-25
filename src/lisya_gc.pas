unit lisya_gc;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, dlisp_values, lisya_predicates, mar, math;


function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
function RefVariable(P: PVariable): PVariable;
procedure ReleaseVariable(var P: PVariable);



function separate(V: TValue; force_const: boolean=false): TValue;
procedure print_links(V: TValue);


implementation

type
    TLinks = array of PPVariable;
    TBlock = record P: PVariable; l: TLinks; end;
    TBlocks = array of TBlock;



procedure extract_block_links(var l: TLinks; V: TValue);

    procedure add_link(from_: PPVariable); inline;
    begin SetLength(l, Length(l)+1); l[high(l)] := from_; end;

var proc: TVProcedure; ls: TVList; rec: TVRecord; ht: TVHashTable;
    ret: TVReturn; CP: TVChainPointer; ss: TVSymbolStack;
    i: integer;
begin
    if V is TVSymbolStack then begin
        ss := V as TVSymbolStack;
        for i := 0 to high(ss.stack) do add_link(@ss.stack[i].V);
    end

    else if V is TVProcedure then begin
        proc := V as TVProcedure;
        extract_block_links(l, proc.stack);
        extract_block_links(l, proc.body);
    end

    else if V is TVChainPointer then begin
        CP := V as TVChainPointer;
        add_link(CP.get_link);
    end

    else if V is TVList then begin
        ls := V as TVList;
        for i := 0 to ls.high do extract_block_links(l,ls.look[i]);
    end

    else if V is TVRecord then begin
        rec := V as TVRecord;
        for i := 0 to rec.count-1 do extract_block_links(l,rec.look[i]);
    end

    else if V is TVHashTable then begin
        ht := V as TVHashTable;
        for i := 0 to ht.Count-1 do begin
            extract_block_links(l, ht.look[i]);
            extract_block_links(l, ht.look_key[i]);
        end;
    end

    else if V is TVReturn then begin
        ret := V as TVReturn;
        extract_block_links(l, ret.value);
    end;
end;


procedure extract_links(var b: TBlocks; P: PVariable);
var i, this: integer;
begin
    if P=nil then Exit;
    for i := 0 to high(b) do if b[i].P = Pointer(P) then Exit;
    SetLength(b, Length(b)+1);
    this := high(b);
    b[this].P := P;
    SetLength(b[this].l,0);
    extract_block_links(b[this].l, P.V);
    for i := 0 to high(b[this].l) do
        extract_links(b, b[this].l[i]^);
end;


procedure show_links_tree(const b: TBlocks);
var i,j: integer; c: array of qword; mask,n, a1, a2: qword;
label next_n;
begin
    SetLength(c,0);
    for i := 0 to high(b) do begin
        a1 := PointerToqword(b[i].p);
        if a1>0 then begin
            SetLength(c, Length(c)+1);
            c[high(c)] := a1;
        end;
        for j := 0 to high(b[i].l) do begin
            SetLength(c, Length(c)+2);
            c[high(c)-1] := PointerToqword(b[i].l[j]);
            c[high(c)] := PointerToqword(b[i].l[j]^);
        end;
    end;

    n := 0;
    while true do begin
        next_n:
        Inc(n);
        mask := not (2**(n*4)-1);
        a1 := c[0] and mask;
        for i := 1 to high(c) do begin
            a2 := c[i] and mask;
            if a1<>a2 then goto next_n;
        end;
        Break;
    end;

    for i := 0 to high(b) do begin
        if b[i].p<>nil
        then WriteLn(PointerToStr(b[i].p, n), ' [', IntToStr(b[i].P.ref_count),']', '  (', b[i].p.V.AsString, ')')
        else WriteLn(PointerToStr(b[i].p, n));
        for j := 0 to high(b[i].l) do
            WriteLn('    '+PointerToStr(b[i].l[j], n),' --> ', PointerToStr(b[i].l[j]^, n));
    end;
end;



procedure print_links(V: TValue);
var P: PVariable; b: TBlocks;
begin
    New(P);
    P.V := V;
    P.ref_count := 0;
    SetLength(b, 0);
    extract_links(b, P);
    show_links_tree(b);
    Dispose(P);
end;


procedure separate_block(V: TValue);
var proc: TVProcedure; ls: TVList; rec: TVRecord; ht: TVHashTable; ret: TVReturn;
    i: integer;
begin
    if V is TVProcedure then begin
        proc := V as TVProcedure;
        separate_block(proc.body);
        separate_block(proc.rest);
    end

    else if V is TVList then begin
        ls := V as TVList;
        ls.CopyOnWrite;
        for i := 0 to ls.high do separate_block(ls.look[i]);
    end

    else if V is TVRecord then begin
        rec := V as TVRecord;
        for i := 0 to rec.count-1 do separate_block(rec.look[i]);
    end

    else if V is TVHashTable then begin
        ht := V as TVHashTable;
        ht.CopyOnWrite;
        ht.CopyKeys;
        for i := 0 to ht.Count-1 do begin
            separate_block(ht.look[i]);
            separate_block(ht.look_key[i]);
        end;
    end

    else if V is TVReturn then begin
        ret := V as TVReturn;
        separate_block(ret.value);
    end;
end;


function separate(V: TValue; force_const: boolean): TValue;
var i,j,k: integer; b,nb: TBlocks; P: PVariable;

    function variable_mirror(P: PVariable): PVariable;
    begin
        New(result);
        result.V := P.V.Copy;
        result.constant:= P.constant or force_const;
        result.ref_count := 0;
    end;

begin
    //первый проход - извлечение графа связей заданной переменной
    New(P);
    P.ref_count:=0;
    P.V := V;
    SetLength(b,0);
    extract_links(b, P);

    //второй проход - копирование блоков
    SetLength(nb, Length(b));
    for i := 0 to high(b) do begin
        SetLength(nb[i].l, 0);
        nb[i].P := variable_mirror(b[i].P);
        separate_block(nb[i].P.V);
        extract_block_links(nb[i].l, nb[i].P.V);
    end;

    //третий проход - перенаправление ссылок из нового графа в старый
    //(образовавшихся при копировании блоков) в новый граф
    for i := 0 to high(nb) do begin
        for j := 0 to high(nb[i].l) do begin
            for k := 1 to high(b) do if b[k].P=Pointer(nb[i].l[j]^) then Break;
            nb[i].l[j]^ := nb[k].P;
            Inc(nb[k].P.ref_count);
            Dec(b[k].P.ref_count);
        end;
    end;

    result := nb[0].P.V;
    Dispose(nb[0].P);
    Dispose(P);
end;



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
        if P.ref_count<0 then Exit;
        if tpProcedure(P.V) and ReleaseRecursiveProcedure(P) then Exit;
        Dec(P.ref_count);
    finally
        P := nil;
    end;
end;





initialization

finalization

end.

