unit dlisp_values;

{$IFDEF FPC} {$MODE Delphi} {$ENDIF}

{$ASSERTIONS ON}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    SysUtils, Classes, Contnrs, ucomplex, crc,
    lisia_charset, zstream, mar, lisya_zip, lisya_exceptions, lisya_streams,
    lisya_process;


const
    {$ifdef windows}
    dir_separator = '\';
    {$else windows}
    dir_separator = '/';
    {$endif windows}


type

    { TValue }

    TValue = class
        function Copy: TValue; virtual; abstract;
        function AsString: unicodestring; virtual;
        function hash: DWORD; virtual;
        function equal({%H-}V: TValue): boolean; virtual;
    end;
    TValueList = array of TValue;

    { TVInternal }

    TVInternal = class (TValue)
        function Copy: TValue; override;
        function equal({%H-}V: TValue): boolean; override;
    end;

    { TVEndOfStream }

    TVEndOfStream = class (TValue)
        function Copy(): TValue; override;
    end;


    { TVariable }

    TVariable = record
        V: TValue;
        constant: boolean;
    private
        ref_count: integer;
    public
        property references: integer read ref_count;
    end;
    PVariable = ^TVariable;


    TStackRecord = record
        V: PVariable;
        N: integer;
    end;


    { TVT }

    TVT = class (TValue)
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;
    end;


    { TVNumber }

    TVNumber = class (TValue)
        function C: COMPLEX; virtual; abstract;
        function equal(V: TValue): boolean; override;
    end;

    { TVComplex }

    TVComplex = class (TVNumber)
        fC: COMPLEX;
        function C: COMPLEX; override;
        constructor Create(re, im: double); overload;
        constructor Create(_c: COMPLEX); overload;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        destructor Destroy; override;
        function hash: DWORD; override;
    end;

    { TVReal }

    TVReal = class (TVNumber)
        function F: double; virtual; abstract;
        function equal(V: TValue): boolean; override;
    end;


    { TVInteger }

    TVInteger = class (TVReal)
        fI: Int64;
        constructor Create(I: Int64);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        function F: double; override;
        function C: COMPLEX; override;
    end;


    { TVFloat }

    TVFloat = class (TVReal)
        fF: double;
        constructor Create(F: double);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;

        function F: double; override;
        function C: COMPLEX; override;
    end;


    { TVRange }

    TVRange = class (TValue)
        low, high: Int64;
        constructor Create(l,h: Int64);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;
    end;

    { TVTime }

    TVTime = class (TValue)
        fDT: TDateTime;
        function equal(V: TValue): boolean; override;

        //function year: integer;
        //function month: integer;
        //function day: integer;
        //function hours: integer;
        //function minutes: integer;
        //function seconds: integer;
        //function milliseconds: integer;
    end;

    { TVTimeInterval }

    TVTimeInterval = class (TVTime)
        constructor Create(dt: TDateTime);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
    end;

    { TVDateTime }

    TVDateTime = class (TVTime)
        constructor Create(dt: TDateTime);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;

        function AsSQLDateTime: unicodestring;
    end;




    { TVSymbol }

    TVSymbol = class (TValue)
    private
        fN: integer;
        fname: unicodestring;
        function fGetUname: unicodestring;
    public
        property name: unicodestring read fname;
        property uname: unicodestring read fGetUname;
        property N: integer read fN;

        constructor Create(S: unicodestring);
        constructor Gensym;
        constructor CreateEmpty;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        class function symbol_n(n: unicodestring): integer;
        class function symbol_uname(nN: integer): unicodestring;
    end;


    { TVKeyword }

    TVKeyword = class (TVSymbol)
        constructor CreateCopy(origin: TVKeyword);
        function Copy: TValue; override;
    end;

    { TVGo }

    TVGo = class (TVInternal)
    end;


    { TVGoto }

    TVGoto = class (TVGo)
        n: integer;
        constructor Create(mark: TVSymbol);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
    end;


    { TVBreak }

    TVBreak = class (TVGo)
        function hash: DWORD; override;
    end;


    { TVContinue }

    TVContinue = class (TVGo)
        function hash: DWORD; override;
    end;


    { TVReturn }

    TVReturn = class (TVGo)
        value: TValue;
        constructor Create(V: TValue);
        destructor Destroy; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
    end;

    { TVPrimitive }

    TVPrimitive = class (TVInternal)
        //класс заглушка, позволяющий функции look вернуть информацию о том,
        //что компонент является примитивным типом
        function hash: DWORD; override;
    end;


    { TVChainPointer }

    TVChainPointer = class (TVInternal)
    private
        primitive: TVPrimitive;
        V: PVariable;
    public
        index: TIntegers;

        constructor Create(P: PVariable); overload;
        constructor Create(P: PVariable; indices: array of integer); overload;
        destructor Destroy; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;

        function constant: boolean;
        function value: TValue;
        function look: TValue;
        procedure add_index(i: integer);
        procedure set_target(_V: TValue);
        procedure CopyOnWrite;
        procedure set_last_index(i: integer);
    end;


    {TVCompound}

    TVCompound = class (TValue)
    private
        function GetItem(index: integer): TValue; virtual; abstract;
        procedure SetItem(index: integer; _V: TValue); virtual; abstract;
        function LookItem(index: integer): TValue; virtual; abstract;
    public
        property items[index: integer]: TValue read GetItem write SetItem; default;
        property look[index: integer]: TValue read LookItem;
        function Count: integer; virtual; abstract;
    end;


    { TVSequence }

    TVSequence = class (TVCompound)
        function high: integer;
        function subseq(istart: integer; iend: integer = -1): TValue; virtual; abstract;
        //function append(ss: TVSequence): TValue; virtual; abstract;
    end;

    { TVCompoundOfPrimitive }

    TVCompoundOfPrimitive = class (TVSequence)
        function LookItem({%H-}index: integer): TValue; override;
    end;


    { TVString }

    TVString = class (TVCompoundOfPrimitive)
    private
        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
      public
        S: unicodestring;
        constructor Create(S: unicodestring);
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;


        function Count: integer; override;
        function crc32: DWORD;

        function subseq(istart: integer; iend: integer = -1): TValue; override;
    end;


    { TVList }

    { TListBody }

    TListBody = class (TObjectList)
        ref_count: integer;
        constructor Create(free_objects: boolean);
    end;

    TVList = class (TVSequence)
    private
        fL: TListBody;
        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;

        function GetElementName(index: integer): unicodestring;
        function GetElementUName(index: integer): unicodestring;
        function GetElementI(index: integer): Int64;
        function GetElementF(index: integer): double;
        function GetElementS(index: integer): unicodestring;
        function GetElementL(index: integer): TVList;
        function GetElementC(index: integer): COMPLEX;
        function LookElementSYM(index: integer): TVSymbol;
    public
        constructor Create; overload;
        constructor Create(VL: array of TValue; free_objects: boolean = true); overload;
        constructor Create(body: TListBody); overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        function Count: integer; override;

        function subseq(istart: integer; iend: integer = -1): TValue; override;

        function CopyOnWrite: boolean;
        procedure Add(V: TValue);
        procedure Append(VL: TVList);


        property name[index: integer]: unicodestring read GetElementName;
        property uname[index: integer]: unicodestring read GetElementUName;
        property I[index: integer]: Int64 read GetElementI;
        property F[index: integer]: double read GetElementF;
        property S[index: integer]: unicodestring read GetElementS;
        property L[index: integer]: TVList read GetElementL;
        property C[index: integer]: COMPLEX read GetElementC;
        property SYM[index: integer]: TVSymbol read LookElementSYM;

        procedure SetCapacity(c: integer);

        function extract(n: integer): TValue;
        procedure delete(n: integer);
        procedure insert(n: integer; V: TValue);
        function POP: TValue;
        procedure Clear;
        function ValueList: TValueList;
        function CdrValueList: TValueList;
        function CAR: TValue;
        function CDR: TVList;
    end;


    { TVBytes }

    TVBytes = class (TVCompoundOfPrimitive)
    private
        function GetByte(Index: Integer): Int64;
        procedure SetByte(Index: Integer; V: Int64);

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;

    public
        fBytes: TBytes;

        constructor Create; overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        property bytes[Index: integer]: Int64 read GetByte write SetByte; default;
        procedure SetCount(l: integer);
        procedure Add(b: Int64);

        function crc32: DWORD;

        function count: integer; override;
        function subseq(istart: integer; iend: integer = -1): TValue; override;
        procedure append(BV: TVBytes);
    end;


    { TVRecord }

    TVRecord = class (TVCompound)
    private
        unames: array of integer;
        slots: TObjectList;
        function fGetSlot(index: unicodestring): TValue;
        procedure fSetSlot(index: unicodestring; V: TValue);
        function flook(index: unicodestring): TValue;

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;
    public
        constructor Create(names: array of unicodestring); overload;

        constructor Create; overload;
        destructor Destroy; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        function GetSlot(nN: integer): TValue;
        procedure SetSlot(nN: integer; V: TValue);
        function LookSlot(nN: integer): TValue;

        property slot[nN: integer]:TValue read GetSlot write SetSlot;
        property Items[index: integer]: TValue read GetItem write SetItem; default;
        property look[index: integer]: TValue read LookItem;
        property look_name[n: unicodestring]: TValue read flook;
        function is_class(cl: TVRecord): boolean;
        procedure AddSlot(name: unicodestring; V: TValue); overload;
        procedure AddSlot(name: TVSymbol; V: TValue); overload;

        function count: integer; override;
        function name_n(n: integer): unicodestring;
        //TODO: сравнение записей должно быть независимым от порядка слотов
        function index_of(nN: integer): integer;
        function get_n_of(index: unicodestring): integer;
    end;



    { TVHashTable }
    //THashTableIndex = array of array of record h: DWORD; k: integer; v: integer end;
    THashTableIndex = array of record h: DWORD; k:integer; end;

    TVHashTable = class (TVCompound)
    private
        data: TVList;
        index: THashTableIndex;
        keys: TVList;

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;

        procedure Expand;
        function FindEmpty(h: DWORD): integer;
    public
        constructor Create;
        constructor CreateEmpty;
        destructor Destroy; override;
        function AsString: unicodestring; override;
        function Copy: TValue; override;
        function hash: DWORD; override;
        function equal({%H-}V: TValue): boolean; override;
        procedure print;

        procedure CopyOnWrite;

        function Get(key: TValue): TValue;
        function GetIndex(key: TValue): integer;
        function GetKeys: TVList;

        function Count: integer; override;
    end;

    { TVSymbolStack }

    TVSymbolStack = class (TValue)
        parent: TVSymbolStack;
        stack: array of TStackRecord;

        function index_of(name: unicodestring): integer; overload;
        function index_of(n: integer): integer; overload;

        constructor Create(parent: TVSymbolStack);
        destructor Destroy; override;
        function Copy: TValue; override;
        function equal(V: TValue): boolean; override;

        function Count: integer;
        function set_n(n: integer; V: TValue): boolean;

        procedure Print(n: integer = -1); overload;

        procedure new_var(name: unicodestring; V: TValue; c: boolean = false); overload;
        procedure new_var(symbol: TVSymbol; V: TValue; c: boolean = false); overload;
        procedure new_var(N: integer; V: TValue; c: boolean = false); overload;
        function find_var(symbol: TVSymbol): TValue; overload;
        function find_var(N: integer): TValue; overload;
        procedure set_var(symbol: TVSymbol; V: TValue);
        procedure clear_frame(n: integer);
        procedure bind_var(symbol, target: TVSymbol);
        function find_ref(symbol: TVSymbol): PVariable; overload;
        function find_ref(N: integer): PVariable; overload;
        function look_var(N: integer): PVariable;
        procedure new_ref(name: unicodestring; P: PVariable); overload;
        procedure new_ref(symbol: TVSymbol; P: PVariable); overload;

        function find_ref_or_nil(symbol: TVSymbol): PVariable;
    end;


    { TVSubprogram }

    TSubprogramParmetersMode = (spmReq, spmOpt, spmKey, spmRest, spmFlag);

    { TSubprogramSignature }

    TSubprogramSignature = record
        required_count: integer;
        mode: TSubprogramParmetersMode;
        p: array of record n,k: integer; end;
        procedure delete(n: integer);
        function AsString: unicodestring;
        //function Copy:TSubprogramSignature;
        //procedure Clear;
    end;


    TVSubprogram = class (TValue)
        nN: integer;
        //TODO: нужно пересмотреть дерево классов подпрограмм
        //поле signature используется только внутренними функциями
        //TVProcedure использует поле sign
        //поле stack не используется операторами и внутренними функциями
    end;

    { TVProcedureForwardDeclaration }

    TVProcedureForwardDeclaration = class (TValue)
        function Copy(): TValue; override;
    end;

    { TVProcedure }

    TVProcedure = class (TVSubprogram)
        sign1: TSubprogramSignature;
        stack: TVSymbolStack;
        rest: TVList;
        body: TVList;
        constructor Create;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;
    end;

    { TVMacro }

    TVMacro = class (TVProcedure)
        //макрос является обычной функцией за тем исключением, что
        //возвращаемый им результат выполняется в месте вызова
        //данный клас нужен, только для того, чтобы eval мог отличить макрос
        //от процедуры
        function AsString: unicodestring; override;
    end;

    TVMacroSymbol = class (TVProcedure)
    end;

    type TEvalProc = function (V: TValue): TValue of Object;
    type TCallProc = function (V: TVList): TValue of Object;
    type TInternalFunctionBody = function (const PL: TVList; call: TCallProc): TValue;

    { TVInternalSubprogram }

    TVInternalSubprogram = class (TVSubprogram)
    end;

    { TVInternalFunction }

    TVInternalFunction = class (TVInternalSubprogram)
    //TODO: нужно унифицировать интерфейсы операторов и внутренних функций,
        signature: TVList;
        body: TInternalFunctionBody;
        constructor Create(sign: TVList;
                            body: TInternalFunctionBody;
                            name: unicodestring = '');
        constructor CreateEmpty;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;
    end;

    { TVPredicate }

    type TTypePredicate = function (V: TValue): boolean;

    TVPredicate = class (TVInternalSubprogram)
        body: TTypePredicate;
        fAssert: boolean;
        constructor Create(name: unicodestring; _body: TTypePredicate; a: boolean); overload;
        constructor Create(_nN: integer; _body: TTypePredicate; a: boolean); overload;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;
    end;

    { TVOperator }

    type TOperatorEnum = (
            oeAND_THEN,
            oeAPPEND,      //mod
            oeASSEMBLE,
            oeBLOCK,
            oeBREAK,
            oeCASE,
            oeCOND,
            oeCONST,
            oeCONTINUE,
            oeDEBUG,
            oeDEFAULT,     //mod
            oeDELETE,      //mod
            oeELT,
            oeERROR,
            oeEXECUTE_FILE,
            oeFOR,
            oeGOTO,
            oeIF,
            oeIF_NIL,
            oeINSERT,      //mod
            oeKEY,
            oeLET,
            oeMACRO,
            oeMACRO_SYMBOL,
            oeOR_THEN,
            oePACKAGE,
            oePOP,          //mod
            oePROCEDURE,
            oePUSH,         //mod
            oeQUOTE,
            oeRETURN,
            oeSET,          //mod
            oeTRY,
            oeUSE,
            oeVAR,
            oeWHEN,
            oeWHILE,
            oeWITH);
    //and append block break case cond const continue default elt for goto if if-nil last let or pop procedure push quote set stack structure structure-as val var when while

    TVOperator = class (TVInternalSubprogram)
        op_enum: TOperatorEnum;

        constructor Create(name: unicodestring; en: TOperatorEnum); overload;
        constructor Create(_nN: integer; en: TOperatorEnum); overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;
    end;

   { TVStreamPointer }

    TVStreamPointer = class (TValue)
        body: TLStream;

        constructor Create(_body: TLStream);
        constructor CreateCopy(origin: TVStreamPointer);
        destructor Destroy; override;

        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        procedure close_stream;
    end;

    { TVZIPArchivePointer }

    TVZIPArchivePointer = class (TValue)
        Z: TZipArchive;

        constructor Create(_Z: TZipArchive);
        destructor Destroy; override;

        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        procedure close_zip;
    end;

    { TVProcessPointer }

    TVProcessPointer = class (TValue)
        P: TLProcess;

        constructor Create(_P: TLProcess);
        destructor Destroy; override;

        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
        function equal(V: TValue): boolean; override;

        procedure close_process;
    end;




procedure Assign(var v1, v2: TValue);
function equal(v1, v2: TValue): boolean; inline;

function op_null(V: TValue): boolean;

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
function RefVariable(P: PVariable): PVariable;
procedure ReleaseVariable(var P: PVariable);

var symbols: array of unicodestring;
    _ : TVSymbol;
    kwFLAG, kwKEY, kwOPTIONAL, kwREST: TVKeyword;


implementation

uses lisya_predicates;

var gensym_n: Int64 = -1;



    { TVariable }



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

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
begin
    New(result);
    result.ref_count:=1;
    result.constant := _constant;
    result.V := _V;
end;

function RefVariable(P: PVariable): PVariable;
begin
    if P<>nil then begin
        Inc(P.ref_count);
        result := P;
    end
    else
        result := nil;
end;

procedure ReleaseVariable(var P: PVariable);
{$IFDEF RECURSIVERELEASE} {$ELSE}var no_refs: boolean;{$ENDIF}
begin
    if P<>nil then begin
        {$IFDEF RECURSIVERELEASE}
        if P.ref_count>0
        then begin
            if tpProcedure(P.V) and ReleaseRecursiveProcedure(P)
            then
                P := nil
            else begin
                Dec(P.ref_count);
                if P.ref_count=0 then begin
                    P.V.Free;
                    Dispose(P);
                    P := nil;
                end;
            end;
        end;
        {$ELSE}
        Dec(P.ref_count);
        no_refs := P.ref_count<=0;
        if no_refs then begin
            P.V.Free;
            Dispose(P);
            P:= nil;
        end;
        {$ENDIF}
    end;
end;


procedure Assign(var v1, v2: TValue);
begin
  v1.Free;
  v1 := v2;
  FreeAndNil(v2);
end;

function equal(v1, v2: TValue): boolean;
begin
    result := v1.equal(v2);
end;

function op_null(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).count=0);
end;

{ TSubprogramSignature }

procedure TSubprogramSignature.delete(n: integer);
var i: integer;
begin
    for i := n to high(p)-1 do p[i]:=p[i+1];
    SetLength(p, Length(p)-1);
    if n<required_count then Dec(required_count);
end;

function TSubprogramSignature.AsString: unicodestring;
var i: integer;
begin
    result := '(';
    for i := 0 to required_count-1 do result := result+TVSymbol.symbol_uname(P[i].n)+' ';
    case mode of
        spmReq:;
        spmOpt: result := result+':OPTIONAL ';
        spmKey: result := result+':KEY ';
        spmFlag:result := result+':FLAG ';
        spmRest:result := result+':REST ';
    end;
    for i := required_count to high(p) do result := result+TVSymbol.symbol_uname(P[i].n)+' ';
    if result[Length(result)] = ' '
    then result[Length(result)] := ')'
    else result := result+')';
end;

{ TSubprogramSignature }

//function TSubprogramSignature.Copy: TSubprogramSignature;
//begin
//    result.required_count:=self.required_count;
//    result.mode:= self.mode;
//    result.p := system.copy(self.p);
//end;
//
//procedure TSubprogramSignature.Clear;
//var i: integer;
//begin
//    required_count := 0;
//    mode := spmReq;
//    for i := 0 to high(p) do p[i].V.Free;
//    SetLength(p, 0);
//end;

{ TVProcedureForwardDeclaration }

function TVProcedureForwardDeclaration.Copy: TValue;
begin
    result := TVProcedureForwardDeclaration.Create;
end;

{ TVProcessPointer }

constructor TVProcessPointer.Create(_P: TLProcess);
begin
    P := _P;
end;

destructor TVProcessPointer.Destroy;
begin
    P.Release;
    inherited Destroy;
end;

function TVProcessPointer.Copy: TValue;
begin
    result := TVProcessPointer.Create(P.Ref as TLProcess);
end;

function TVProcessPointer.AsString: unicodestring;
begin
    Result:= '<PROCESS '+P.description+'>';
end;

function TVProcessPointer.hash: DWORD;
begin
    Result:= 0;
end;

function TVProcessPointer.equal(V: TValue): boolean;
begin
    Result := (V is TVProcessPointer) and ((V as TVProcessPointer).P=P);
end;

procedure TVProcessPointer.close_process;
begin
    P.term;
end;


{ TVCompoundOfPrimitive }

function TVCompoundOfPrimitive.LookItem(index: integer): TValue;
begin
    result := nil;
    raise ELE.Create('look for item of '+self.ClassName, 'internal');
end;

{ TVInternal }

function TVInternal.Copy: TValue;
begin
    result := self.ClassType.Create as TValue;
end;

function TVInternal.equal(V: TValue): boolean;
begin
    result := false;
    raise ELE.Create('equal '+self.ClassName, 'internal');
end;


{ TValue }

function TValue.AsString: unicodestring;
begin
    result := '#<'+self.ClassName+'>';
end;

function TValue.hash: DWORD;
begin
    result := 0;
end;

function TValue.equal(V: TValue): boolean;
begin
    result := false;
end;


{ TVZIPArchivePointer }

constructor TVZIPArchivePointer.Create(_Z: TZipArchive);
begin
    Z := _Z;
end;

destructor TVZIPArchivePointer.Destroy;
begin
    Z.Release;
    inherited Destroy;
end;

function TVZIPArchivePointer.Copy: TValue;
begin
    result := TVZIPArchivePointer.Create(Z.Ref as TZipArchive);
end;

function TVZIPArchivePointer.AsString: unicodestring;
begin
    result := '#<ZIP '+Z.description+'>';
end;

function TVZIPArchivePointer.hash: DWORD;
begin
    result := 10009;
end;

function TVZIPArchivePointer.equal(V: TValue): boolean;
begin
    result := (V is TVZIPArchivePointer) and (Z = (V as TVZIPArchivePointer).Z);
end;

procedure TVZIPArchivePointer.close_zip;
begin
    Z.Close;
end;



{ TVTime }

function TVTime.equal(V: TValue): boolean;
begin
    result := (V.ClassType=self.ClassType) and (fDT=(V as TVTime).fDT);
end;

{ TVReal }

function TVReal.equal(V: TValue): boolean;
begin
    Result:= (V is TVReal) and (F=(V as TVReal).F);
end;

{ TVNumber }

function TVNumber.equal(V: TValue): boolean;
begin
    result := (V is TVNumber) and (C=(V as TVNumber).C);
end;

{ TVMacro }

function TVMacro.AsString: unicodestring;
begin
    if nN<=0
    then result := '#<MACRO '+sign1.AsString+'>'
    else result := '#<MACRO '+symbols[nN]+'>';
end;

{ TVHashTable }

function TVHashTable.GetItem(index: integer): TValue;
begin
    result := data[index];
end;

procedure TVHashTable.SetItem(index: integer; _V: TValue);
begin
    data[index] := _v;
end;

function TVHashTable.LookItem(index: integer): TValue;
begin
    result := data.look[index];
end;


function TVHashTable.GetIndex(key: TValue): integer;
var h: DWORD; i, li: integer;
begin
    Expand;
    h := key.hash;
    for i := h mod Length(index) to (h mod Length(index)) + Length(index) do begin
        li := i mod Length(index);
        if (index[li].h=h) and (index[li].k>=0) and key.equal(keys.look[index[li].k])
        then begin
            result := index[li].k;
            Exit;
        end
        else
            if index[li].k=-1 then begin
                keys.Add(key.Copy);
                data.Add(TVList.Create);
                index[li].h:=h;
                index[li].k:=data.high;
                result := data.high;
                Exit;
            end;
    end;
end;

function TVHashTable.GetKeys: TVList;
var i: integer;
begin
    result := TVList.Create;
    for i := 0 to Count-1 do
        if tpTrue(data.look[i]) then result.Add(keys[i]);
end;

procedure TVHashTable.Expand;
var old_index: THashTableIndex; i: integer;
begin
    if count<(Length(index) div 4) then Exit;

    old_index := System.Copy(index);
    SetLength(index, Length(old_index)*2);
    for i := 0 to high(index) do index[i].k:=-1;
    for i := 0 to high(old_index) do
        index[FindEmpty(old_index[i].h)] := old_index[i];
end;

function TVHashTable.FindEmpty(h: DWORD): integer;
var i, li: integer;
begin
    for i := h mod Length(index) to h mod Length(index) + Length(index) do begin
        li := i mod Length(index);
        if index[li].k=-1 then begin
            result := li;
            Exit;
        end;
    end;
    raise ELE.Create('filfull hash-table', 'internal');
end;


constructor TVHashTable.Create;
begin
    data := TVList.Create;
    keys := TVList.Create;
    SetLength(index, 2);
    index[0].k:=-1;
    index[1].k:=-1;
end;

constructor TVHashTable.CreateEmpty;
begin
    data := nil;
    keys := nil;
    SetLength(index, 0);
end;

destructor TVHashTable.Destroy;
begin
    keys.Free;
    data.Free;
    SetLength(index, 0);
    inherited Destroy;
end;

function TVHashTable.AsString: unicodestring;
begin
    result := '#<HASH-TABLE '+IntToStr(count)+'/'+IntToStr(Length(index))+'>';
end;

function TVHashTable.Copy: TValue;
begin
    result := TVHashTable.CreateEmpty;
    (result as TVHashTable).data := data.Copy as TVList;
    (result as TVHashTable).keys := keys.Copy as TVList;
    (result as TVHashTable).index := System.Copy(index);
end;

function TVHashTable.hash: DWORD;
begin
    result := 10008;
end;

function TVHashTable.equal(V: TValue): boolean;
begin
    result := false;
    raise ELE.Create('TVHashTable.eqial','not implemented');
end;

procedure TVHashTable.print;
var i: integer;
begin
    WriteLn('--------',AsString,'--------');
    for i:=0 to Length(index)-1 do WriteLn(index[i].h, '   ', index[i].k);
    WriteLn('keys');
    for i := 0 to keys.high do WriteLn('    ', i, '  ', keys.look[i].AsString);
    WriteLn('values');
    for i := 0 to data.high do WriteLn('    ', i, '  ', data.look[i].AsString);
    WriteLn('--------',AsString,'--------');
end;

procedure TVHashTable.CopyOnWrite;
begin
    data.CopyOnWrite;
    //ключи не копируются при модификации значения ХЭШ таблицы,
    //поскольку они изменяются только при добавлении новых значений
end;

//procedure TVHashTable.AddPair(key, value: TValue);
//var h: DWORD; i: integer;
//begin
//    h := key.hash;
//    keys.Add(key);
//    data.Add(value);
//    i := FindEmpty(h);
//    index[i].h := h;
//    index[i].k := data.high;
//end;


function TVHashTable.Get(key: TValue): TValue;
begin
    result := data[GetIndex(key)]
end;

function TVHashTable.Count: integer;
begin
    result := data.Count;
end;


{ TListBody }

constructor TListBody.Create(free_objects: boolean);
begin
    inherited Create(free_objects);
    ref_count := 1;
end;


{ TVComplex }

function TVComplex.C: COMPLEX;
begin
   result := fC;
end;

constructor TVComplex.Create(re, im: double);
begin
    fC.re := re;
    fC.im := im;
end;

constructor TVComplex.Create(_c: COMPLEX);
begin
    fC := _c;
end;

function TVComplex.Copy: TValue;
begin
    result := TVComplex.Create(fC.re, fC.im);
end;

function TVComplex.AsString: unicodestring;
begin
    result := FloatToStr(fC.re);
    if fC.im<0
    then result := result+'-i'+FloatToStr(-fC.im)
    else result := result+'+i'+FloatToStr(+fC.im);
end;

destructor TVComplex.Destroy;
begin
    inherited Destroy;
end;

function TVComplex.hash: DWORD;
var i: Int64;
begin
    if fC.im=0
    then begin
        i := round(fC.re);
        if i = fC.re
        then result := crc32(0, @i, SizeOf(i))
        else result := crc32(0, @fC.re, SizeOf(fC.re))
    end
    else result := crc32(0, @fC, SizeOf(COMPLEX));
end;

{ TVPredicate }

constructor TVPredicate.Create(name: unicodestring; _body: TTypePredicate; a: boolean);
begin
    nN := TVSymbol.symbol_n(name);
    body := _body;
    fAssert := a;
end;

constructor TVPredicate.Create(_nN: integer; _body: TTypePredicate; a: boolean);
begin
    nN := _nN;
    body := _body;
    fAssert := a;
end;


function TVPredicate.Copy: TValue;
begin
    result := TVPredicate.Create(nN, body, fAssert);
end;

function TVPredicate.AsString: unicodestring;
var a: unicodestring;
begin
    if fAssert then a := '!' else a := '?';
    result := '#<PREDICATE '+symbols[nN]+a+'>';
end;

function TVPredicate.hash: DWORD;
begin
    if fAssert
    then result := crc32(1, @body, SizeOf(body))
    else result := crc32(0, @body, SizeOf(body));
end;

function TVPredicate.equal(V: TValue): boolean;
begin
    result := (V is TVPredicate)
        and (@body=@((V as TVPredicate).body))
        and (fAssert=(V as TVPredicate).fAssert);
end;

{ TVReturn }

constructor TVReturn.Create(V: TValue);
begin
    value := V;
end;

destructor TVReturn.Destroy;
begin
    value.Free;
    inherited Destroy;
end;

function TVReturn.Copy: TValue;
begin
    result := TVReturn.Create(value.Copy);
end;

function TVReturn.AsString: unicodestring;
begin
    result := '#<RETURN '+value.AsString+'>';
end;

function TVReturn.hash: DWORD;
var i: DWORD;
begin
    i := 4;
    result := crc32(value.hash, @i, SizeOf(i));
end;


{ TVStreamPointer }

constructor TVStreamPointer.Create(_body: TLStream);
begin
    body := _body;
end;

constructor TVStreamPointer.CreateCopy(origin: TVStreamPointer);
begin
    body := origin.body.Ref as TLStream;
end;

destructor TVStreamPointer.Destroy;
begin
    body.Release;
    inherited Destroy;
end;

function TVStreamPointer.Copy: TValue;
begin
    result := TVStreamPointer.CreateCopy(self);
end;

function TVStreamPointer.AsString: unicodestring;
begin
    result := '#<STREAM-POINTER '+body.description+'>';
end;

function TVStreamPointer.hash: DWORD;
begin
    if body=nil
    then result := 10007
    else result := crc32(0, @body, SizeOf(body));
end;

function TVStreamPointer.equal(V: TValue): boolean;
begin
    result := (V is TVStreamPointer) and (body = (V as TVStreamPointer).body);
end;

procedure TVStreamPointer.close_stream;
begin
    body.close_stream;
end;


{ TVKeyword }

constructor TVKeyword.CreateCopy(origin: TVKeyword);
begin
    fN := origin.fN;
    fName := origin.fname;
end;

function TVKeyword.Copy: TValue;
begin
    result := TVKeyword.CreateCopy(self);
end;


{ TVSequence }

function TVSequence.high: integer;
begin
    result := self.Count-1;
end;

{ TVPrimitive }


function TVPrimitive.hash: DWORD;
begin
    Result := 10004;
end;


{ TVChainPointer }

constructor TVChainPointer.Create(P: PVariable);
begin
    V := P;
    SetLength(index, 0);
    primitive := TVPrimitive.Create;
end;

constructor TVChainPointer.Create(P: PVariable; indices: array of integer);
var i: integer;
begin
    V := P;
    SetLength(index, Length(indices));
    for i := 0 to High(indices) do index[i] := indices[i];
    primitive := TVPrimitive.Create;
end;

destructor TVChainPointer.Destroy;
begin
    ReleaseVariable(V);
    SetLength(index,0);
    primitive.Free;
    inherited;
end;

function TVChainPointer.Copy: TValue;
var i: integer;
begin
    result := TVChainPointer.Create(RefVariable(V));
    SetLength((result as TVChainPointer).index, Length(index));
    for i := 0 to high(index) do
        (result as TVChainPointer).index[i] := index[i];
end;

function TVChainPointer.AsString: unicodestring;
var i: integer;
begin
    result := '#<CHAIN-POINTER';
    for i := 0 to high(index) do result := result+' '+IntToStr(index[i]);
    result := result + '>';
end;

function TVChainPointer.hash: DWORD;
begin
    Result:= look.hash;
end;

function TVChainPointer.constant: boolean;
begin
    result := V.constant;
end;

function TVChainPointer.value: TValue;
var i: integer; tmp : TVCompound;
begin
    if Length(index)=0
    then result := V.V.Copy
    else begin
        tmp := V.V as TVCompound;
        for i := 0 to high(index)-1 do tmp := tmp.look[index[i]] as TVCompound;
        result := tmp[index[high(index)]];
    end;
end;

function TVChainPointer.look: TValue;
var i: integer;
begin
    if Length(index)=0
    then result := V.V
    else begin
        result := V.V;
        for i := 0 to high(index)-1 do
            result := (result as TVCompound).look[index[i]];
        //костыль
        if result is TVCompoundOfPrimitive
        then result := primitive
        else result := (result as TVCompound).look[index[high(index)]];
    end;
end;

procedure TVChainPointer.add_index(i: integer);
begin
    SetLength(index, Length(index)+1);
    index[high(index)] := i;
end;

procedure TVChainPointer.set_target(_V: TValue);
var tmp: TVCompound; i: integer;
begin
    if V.constant then raise ELE.Create('target is not variable');
    if Length(index)=0 then begin
        V.V.Free;
        V.V := _V;
    end
    else begin
        tmp := V.V as TVCompound;
        for i := 0 to high(index)-1 do tmp := tmp.look[index[i]] as TVCompound;
        tmp[index[high(index)]] := _V;
    end;
end;

procedure TVChainPointer.CopyOnWrite;
var obj: TValue; i: integer;
begin
    obj := self.V.V;
    if obj is TVList then (obj as TVList).CopyOnWrite;
    if obj is TVHashTable then (obj as TVHashTable).CopyOnWrite;
    for i := 0 to high(index)-1 do begin
        obj := (obj as TVCompound).look[index[i]];
        if (obj is TVList) then (obj as TVList).CopyOnWrite;
        if obj is TVHashTable then (obj as TVHashTable).CopyOnWrite;
    end;
end;


procedure TVChainPointer.set_last_index(i: integer);
begin
    index[high(index)] := i;
end;


{ TVTimeInterval }

constructor TVTimeInterval.Create(dt: TDateTime);
begin
    fDT := dt;
end;

function TVTimeInterval.Copy: TValue;
begin
    result := TVTimeInterval.Create(fDT);
end;

function TVTimeInterval.AsString: unicodestring;
var hour,minute,second, ms: WORD;
    function dd(i: integer): unicodestring;
    begin
        result := IntToStr(i);
        if Length(result)=1 then result := '0'+result;
    end;
    function ddd(i: integer): unicodestring;
    begin
        result := IntToStr(i);
        if Length(result)=2 then result := '0'+result;
        if Length(result)=1 then result := '00'+result;
    end;
begin
    DecodeTime(fDT, hour, minute, second, ms);
    result := IntToStr(round(fDT))+':'
        +dd(hour)+':'+dd(minute)+':'+dd(second)+'.'+ddd(ms);
    if fDT>=0
    then result := '#<'+result+'>'
    else result := '#< - '+result+'>'
end;

function TVTimeInterval.hash: DWORD;
begin
    result := crc32(1, @fDT, SizeOf(fDT));
end;

{ TVDateTime }

constructor TVDateTime.Create(dt: TDateTime);
begin
    fDT := dt;
end;

function TVDateTime.Copy: TValue;
begin
    result := TVDateTime.Create(fDT);
end;

function TVDateTime.AsString(): unicodestring;
begin
    result := AsSQLDateTime;
end;

function TVDateTime.hash: DWORD;
begin
    result := crc32(2, @fDT, SizeOf(fDT));
end;

function TVDateTime.AsSQLDateTime: unicodestring;
var year,month,day,hour,minute,second, ms: WORD;
    function dd(i: integer): unicodestring;
    begin
        result := IntToStr(i);
        if Length(result)=1 then result := '0'+result;
    end;
    function ddd(i: integer): unicodestring;
    begin
        result := IntToStr(i);
        if Length(result)=2 then result := '0'+result;
        if Length(result)=1 then result := '00'+result;
    end;
begin
    DecodeDate(fDT, year, month, day);
    DecodeTime(fDT, hour, minute, second, ms);
    result := ''''+IntToStr(Year)+'-'+dd(month)+'-'+dd(day)+
        ' '+dd(hour)+':'+dd(minute)+':'+dd(second)+'.'+ddd(ms)+'''';
end;

{ TVBytes }

function TVBytes.GetByte(Index: Integer): Int64;
begin
    result := fBytes[index];
end;

procedure TVBytes.SetByte(Index: Integer; V: Int64);
begin
    if (V<0) or (V>255) then raise ELE.Create('byte', 'out of range');
    fBytes[Index] := V;
end;

function TVBytes.GetItem(index: integer): TValue;
begin
    result := TVInteger.Create(fBytes[index]);
end;

procedure TVBytes.SetItem(index: integer; _V: TValue);
begin
    fBytes[index] := (_V as TVInteger).fI;
    _V.Free;
end;

constructor TVBytes.Create;
begin
    SetLength(fBytes,0);
end;

destructor TVBytes.Destroy;
begin
    SetLength(fBytes,0);
    inherited Destroy;
end;


function TVBytes.Copy: TValue;
var i: integer; res: TVBytes;
begin
    res := TVBytes.Create;
    Setlength(res.fBytes, Length(fBytes));
    for i := 0 to Length(fBytes)-1 do res.fBytes[i] := fBytes[i];
    result := res;
end;

function TVBytes.AsString: unicodestring;
var i: integer;
begin
    result := '#B(';
    for i := 0 to Length(fBytes)-1 do result := result+' '+IntToHex(fBytes[i],2);
    result := result + ')';
end;

function TVBytes.hash: DWORD;
begin
    result := crc32;
end;

function TVBytes.equal(V: TValue): boolean;
var i: integer; BV: TVBytes;
begin
    result := V is TVBytes;
    if not result then Exit;
    BV := V as TVBytes;

    result := Length(fBytes) = Length(BV.fBytes);
    if not result then Exit;

    for i := 0 to Length(fBytes)-1 do begin
        result := fBytes[i]=BV.fBytes[i];
        if not result then exit;
    end;
end;

procedure TVBytes.SetCount(l: integer);
begin
    SetLength(fBytes, l);
end;

procedure TVBytes.Add(b: Int64);
begin
    if (b<0) or (b>255) then raise ELE.Create('byte', 'out of range');
    SetLength(fBytes, Length(fBytes)+1);
    fBytes[Length(fBytes)-1] := b;
end;

function TVBytes.subseq(istart: integer; iend: integer): TValue;
var i, high_i: integer;
begin
    if iend<0 then high_i := length(fBytes)-1 else high_i := iend-1;
    result := TVBytes.Create;
    SetLength((result as TVBytes).fBytes, high_i-istart+1);
    for i := istart to high_i do
        (result as TVBytes).fBytes[i-istart] := fBytes[i];
end;

procedure TVBytes.append(BV: TVBytes);
var i, offset: integer;
begin
    offset := Length(fBytes);
    SetLength(fBytes, Length(fBytes)+Length(BV.fBytes));
    for i := 0 to Length(BV.fBytes)-1 do
        fBytes[i+offset] := BV.fBytes[i];

    BV.Free;
end;

function TVBytes.crc32: DWORD;
begin
    result := crc.crc32(0, @fBytes[0], Length(fBytes));
end;

function TVBytes.count: integer;
begin
    result := Length(fBytes);
end;


{ TVEndOfStream }

function TVEndOfStream.Copy: TValue;
begin
    result := TVEndOfStream.Create;
end;


{ TVSymbolStack }

function TVSymbolStack.index_of(name: unicodestring): integer;
begin
    result := index_of(TVSymbol.symbol_n(name));
end;

function TVSymbolStack.index_of(n: integer): integer;
begin
    for result := high(stack) downto 0 do
        if stack[result].N = n then
            if stack[result].V=nil
            then raise ELE.Create(symbols[n]+' - nil bound', 'internal')
            else exit;

    raise ELE.Create(symbols[n], 'symbol not bound');
end;

constructor TVSymbolStack.Create(parent: TVSymbolStack);
begin
    self.parent := parent;
    SetLength(stack,0);
end;

destructor TVSymbolStack.Destroy;
begin
    clear_frame(0);
    inherited Destroy;
end;


function TVSymbolStack.Copy: TValue;
var i: integer;
begin
    result := TVSymbolStack.Create(parent);
    SetLength((result as TVSymbolStack).stack, Length(stack));
    for i := 0 to high(stack) do begin
        (result as TVSymbolStack).stack[i].N := stack[i].N;
        (result as TVSymbolStack).stack[i].V := RefVariable(stack[i].V)
    end;
end;


function TVSymbolStack.equal(V: TValue): boolean;
var i: integer; ss: TVSymbolStack;
begin
    result := V is TVSymbolStack;
    if not result then exit;
    ss := V as TVSymbolStack;

    result := Length(stack)=Length(ss.stack);
    if not result then Exit;
    for i := 0 to high(stack) do begin
        result := (stack[i].N=ss.stack[i].N) and (stack[i].V=ss.stack[i].V);
        if not result then Exit;
    end;
end;

function TVSymbolStack.Count: integer;
begin
    result := Length(stack);
end;

function TVSymbolStack.set_n(n: integer; V: TValue): boolean;
begin
    if (n<0) or (n>high(stack))
    then raise ELE.Create('stack '+IntToStr(n), 'out of bounds');

    if stack[n].V.constant
    then raise ELE.Create('stack element '+IntToStr(n)+' is constant');
    stack[n].V.V.Free;
    stack[n].V.V := V;
    result := true;
end;


procedure TVSymbolStack.Print(n: integer);
var i, low_i: integer;
begin
    if n<0
    then low_i := 0
    else low_i := high(stack) - n;
    if low_i<0 then low_i := 0;
    WriteLn('-----------------------');
    for i := low_i to high(stack) do begin
        Write('  | ', symbols[stack[i].N]);
        if stack[i].V<> nil
        then WriteLn('  >(', stack[i].V.ref_count,')',
                        {%H-}Cardinal(stack[i].V),'>  ', stack[i].V.V.AsString)
        else WriteLn(' nil');
    end;
    WriteLn('-----------------------');

end;

procedure TVSymbolStack.new_var(name: unicodestring; V: TValue; c: boolean);
begin
    new_var(TVSymbol.symbol_n(name), V, c);
end;

procedure TVSymbolStack.new_var(symbol: TVSymbol; V: TValue; c: boolean);
begin
    new_var(symbol.N, V, c);
end;

procedure TVSymbolStack.new_var(N: integer; V: TValue; c: boolean);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].N := N;
    stack[high(stack)].V := NewVariable;
    stack[high(stack)].V.V := V;
    stack[high(stack)].V.constant := c;
end;

function TVSymbolStack.find_var(symbol: TVSymbol): TValue;
begin
   result := stack[index_of(symbol.N)].V.V.Copy;
end;

function TVSymbolStack.find_var(N: integer): TValue;
begin
    result := stack[index_of(N)].V.V.Copy;
end;

procedure TVSymbolStack.set_var(symbol: TVSymbol; V: TValue);
begin
    set_n(index_of(symbol.N), V);
end;

procedure TVSymbolStack.clear_frame(n: integer);
var i: integer;
begin
    Assert(n<=Length(stack), 'очистка выше стека');

    for i := high(stack) downto n do ReleaseVariable(stack[i].V);

    SetLength(stack, n);
end;


procedure TVSymbolStack.bind_var(symbol, target: TVSymbol);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].V := RefVariable(stack[index_of(target.n)].V);
    stack[high(stack)].N := symbol.N;
end;


function TVSymbolStack.find_ref(symbol: TVSymbol): PVariable;
begin
    result := RefVariable(stack[index_of(symbol.N)].V);
end;

function TVSymbolStack.find_ref(N: integer): PVariable;
begin
    result := RefVariable(stack[index_of(N)].V);
end;

function TVSymbolStack.look_var(N: integer): PVariable;
begin
    result := stack[index_of(N)].V;
end;

procedure TVSymbolStack.new_ref(name: unicodestring; P: PVariable);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].N := TVSymbol.symbol_n(name);
    stack[high(stack)].V := P;
end;

procedure TVSymbolStack.new_ref(symbol: TVSymbol; P: PVariable);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].N := symbol.N;
    stack[high(stack)].V := P;
end;

function TVSymbolStack.find_ref_or_nil(symbol: TVSymbol): PVariable;
var i: integer;
begin
    result := nil;
    for i := high(stack) downto 0 do
        if stack[i].N=symbol.N then begin
            result := RefVariable(stack[i].V);
            exit;
        end;
end;


{ TVRange }

constructor TVRange.Create(l, h: Int64);
begin
    low := l;
    high := h;
end;

function TVRange.Copy: TValue;
begin
    result := TVRange.Create(low,high);
end;

function TVRange.AsString: unicodestring;
begin
    result := IntToStr(low)+'..'+IntToStr(high);
end;

function TVRange.hash: DWORD;
begin
    result := crc32(0, @low, SizeOf(low));
    result := crc32(result, @high, SizeOf(high));
end;

function TVRange.equal(V: TValue): boolean;
begin
    result := (V is TVRange) and (low=(V as TVRange).low) and (high=(V as TVRange).high);
end;


{ TVRecord }

function TVRecord.fGetSlot(index: unicodestring): TValue;
begin
    result := (slots[index_of(TVSymbol.symbol_n(index))] as TValue).copy;
end;

procedure TVRecord.fSetSlot(index: unicodestring; V: TValue);
begin
    slots[index_of(TVSymbol.symbol_n(index))] := V;
end;

function TVRecord.flook(index: unicodestring): TValue;
begin
    //TODO: обращение к несуществующему слоту структуры вызывает необрабатываемую ошибку
    result := slots[index_of(TVSymbol.symbol_n(index))] as TValue;
end;

function TVRecord.GetItem(index: integer): TValue;
begin
    result := (slots[index] as TValue).Copy;
end;

procedure TVRecord.SetItem(index: integer; _V: TValue);
begin
    slots[index] := _V;
end;

function TVRecord.LookItem(index: integer): TValue;
begin
    result := slots[index] as TValue;
end;

function TVRecord.index_of(nN: integer): integer;
var i: integer;
begin
    for i := 0 to high(unames) do
        if unames[i]=nN then begin
            result := i;
            Exit;
        end;
    raise ELE.Create('slot '+TVSymbol.symbol_uname(nN)+' not found');
end;

constructor TVRecord.Create(names: array of unicodestring);
var i: integer;
begin
    SetLength(unames, Length(names));
    slots := TObjectList.create(true);
    slots.Capacity := Length(names);
    for i := low(names) to high(names) do begin
        unames[i] := TVSymbol.symbol_n(names[i]);
        slots.Add(TVList.Create);
    end;
end;

constructor TVRecord.Create;
begin
    SetLength(unames, 0);
    slots := TObjectList.Create(true);
end;

destructor TVRecord.Destroy;
begin
    SetLength(unames, 0);
    slots.Free;
    inherited Destroy;
end;

function TVRecord.Copy: TValue;
var i: integer;
begin
    result := TVRecord.Create;

    SetLength((result as TVRecord).unames, Length(unames));
    (result as TVRecord).slots.capacity := slots.count;
    for i := low(unames) to high(unames) do begin
        (result as TVRecord).unames[i] := unames[i];
        (result as TVRecord).slots.Add((slots[i] as TValue).copy);
    end;
end;

function TVRecord.AsString: unicodestring;
var i: integer;
begin
    result := '#R(';
    if slots.Count>0
    then result := result + TVSymbol.symbol_uname(unames[0]) + ' ' + (slots[0] as TValue).AsString();
    for i := 1 to slots.Count - 1 do
    result := result + ' ' + TVSymbol.symbol_uname(unames[i]) + ' ' + (slots[i] as TValue).AsString();
    result := result + ')';
end;

function TVRecord.hash: DWORD;
var i: integer;
begin
    result := 0;
    for i := 0 to high(unames) do
        result := result +
            crc32((slots[i] as TValue).hash,@unames[i], SizeOf(unames[i]))
            div Length(unames);
end;

function TVRecord.equal(V: TValue): boolean;
var i, j: integer; VR: TVRecord;
begin
    result := V is TVRecord;
    if not result then Exit;
    VR := V as TVRecord;

    result := count=VR.count;
    if not result then Exit;

    for i := 0 to high(unames) do begin
        result := false;
        for j := 0 to high(VR.unames) do
            if (unames[i]=VR.unames[j])
                and ((slots[i] as TValue).equal(VR.slots[j] as TValue))
            then result := true;
        if not result then Exit;
    end;

    result := true;
end;

function TVRecord.is_class(cl: TVRecord): boolean;
var i, j: integer; m: boolean;
begin
    result := false;
    for i := 0 to High(cl.unames) do begin
        m := false;
        for j := 0 to high(unames) do if cl.unames[i]=unames[j] then m := true;
        if not m then exit;
    end;
    result := true;
end;

procedure TVRecord.AddSlot(name: unicodestring; V: TValue);
begin
    SetLength(unames, Length(unames)+1);
    unames[high(unames)] := TVSymbol.symbol_n(name);
    slots.Add(V);
end;

procedure TVRecord.AddSlot(name: TVSymbol; V: TValue);
begin
    SetLength(unames, Length(unames)+1);
    unames[high(unames)] := name.N;
    slots.Add(V);
end;

function TVRecord.GetSlot(nN: integer): TValue;
begin
    result := (slots[index_of(nN)] as TValue).Copy;
end;

procedure TVRecord.SetSlot(nN: integer; V: TValue);
begin
    slots[index_of(nN)] := V;
end;

function TVRecord.LookSlot(nN: integer): TValue;
begin
    result := slots[index_of(nN)] as TValue;
end;

function TVRecord.count: integer;
begin
    result := Length(unames);
end;

function TVRecord.name_n(n: integer): unicodestring;
begin
    result := TVSymbol.symbol_uname(unames[n]);
end;

function TVRecord.get_n_of(index: unicodestring): integer;
begin
    result := index_of(TVSymbol.symbol_n(index));
end;


{ TVOperator }

constructor TVOperator.Create(name: unicodestring; en: TOperatorEnum);
begin
    self.nN := TVSymbol.symbol_n(name);
    self.op_enum := en;
end;

constructor TVOperator.Create(_nN: integer; en: TOperatorEnum);
begin
    self.nN := _nN;
    self.op_enum := en;
end;

destructor TVOperator.Destroy;
begin
    inherited Destroy;
end;

function TVOperator.Copy: TValue;
begin
    result := TVOperator.Create(nN, op_enum);
end;

function TVOperator.AsString: unicodestring;
begin
    result := '#<OPERATOR '+symbols[nN]+'>';//+signature.AsString()+'>';
end;

function TVOperator.hash: DWORD;
begin
    result := crc32(6, @op_enum, SizeOf(op_enum));
end;

function TVOperator.equal(V: TValue): boolean;
begin
    result := (V is TVOperator) and (op_enum=(V as TVOperator).op_enum);
end;

{ TVInternalFunction }

constructor TVInternalFunction.Create(sign: TVList;
                                    body: TInternalFunctionBody;
                                    name: unicodestring = '');
begin
    inherited Create;
    signature := sign;
    self.body := body;
    self.nN := TVSymbol.symbol_n(name);
end;

constructor TVInternalFunction.CreateEmpty;
begin
//
end;

destructor TVInternalFunction.Destroy;
begin
    //сигнатура не должна освобождаться, поскольку все экземпляры встроенной
    //функции используют общую сигнатуру, освобождаемую при завершении программы

    //signature.Free;
    inherited Destroy;
end;


function TVInternalFunction.Copy: TValue;
begin
    result := TVInternalFunction.CreateEmpty;
    with (result as TVInternalFunction) do begin
        signature := self.signature;
        body := self.body;
        nN := self.nN;
    end;
end;

function TVInternalFunction.AsString: unicodestring;
begin
    result := '#<INTERNAL '+symbols[nN]+'>';//+signature.AsString()+'>';
end;

function TVInternalFunction.hash: DWORD;
begin
    result := crc32(0, @body, SizeOf(body));
end;

function TVInternalFunction.equal(V: TValue): boolean;
begin
    result := (V is TVInternalFunction) and (@body=@((V as TVInternalFunction).body));
end;

{ TVContinue }


function TVContinue.hash: DWORD;
begin
    result := 10003;
end;

{ TVBreak }

function TVBreak.hash: DWORD;
begin
    result := 10002;
end;


{ TVProcedure }

constructor TVProcedure.Create;
begin
    inherited;
    body := nil;
    stack := nil;
    rest := nil;
    SetLength(sign1.p, 0);
    sign1.mode:=spmReq;
    sign1.required_count:=0;
end;

destructor TVProcedure.Destroy;
begin
    stack.Free;
    body.Free;
    rest.Free;
    SetLength(sign1.p, 0);
    inherited Destroy;
end;


function TVProcedure.Copy: TValue;
begin
    //TODO: копирование процедуры ненадёжно может приводить к утечкам

    result := self.ClassType.Create as TValue;

    (result as TVProcedure).body := body.Copy() as TVList;
    (result as TVProcedure).stack := stack.Copy as TVSymbolStack;
    (result as TVProcedure).sign1 := sign1;
    (result as TVProcedure).nN := nN;
    (result as TVProcedure).rest := rest.Copy as TVList;
end;

function TVProcedure.AsString: unicodestring;
begin
    if nN<=0
    then result := '#<PROCEDURE '+sign1.AsString+'>'
    else result := '#<PROCEDURE '+symbols[nN]+'>';
end;

function TVProcedure.hash: DWORD;
begin
    result := (body.hash div 2);//TODO: хэш от процедур вычисляется без учёта сигнатуры
end;

function TVProcedure.equal(V: TValue): boolean;
var proc: TVProcedure;
begin
    result := self.ClassType=V.ClassType;
    if not result then Exit;
    proc := V as TVProcedure;
    //TODO: равенство процедур вычисляется без учёта сигнатуры
    result := //sign.equal(proc.sign)
        //and
        body.equal(proc.body)
        and rest.equal(proc.rest)
        and stack.equal(proc.stack);
end;


{ TVGoto }


constructor TVGoto.Create(mark: TVSymbol);
begin
    if mark<>nil then N := mark.N;
end;

function TVGoto.Copy: TValue;
begin
    result := TVGoto.Create(nil);
    (result as TVGoto).N := N;
end;

function TVGoto.AsString: unicodestring;
begin
    result := '#<GOTO '+TVSymbol.symbol_uname(N)+'>';
end;

function TVGoto.hash: DWORD;
begin
    result := crc32(3, @n, SizeOf(n));
end;



{ TVT }

function TVT.Copy: TValue;
begin
    result := TVT.Create;
end;

function TVT.AsString: unicodestring;
begin
    result := 'T';
end;

function TVT.hash: DWORD;
begin
    result := 1000000001;
end;

function TVT.equal(V: TValue): boolean;
begin
    result := V.ClassType=TVT;
end;

{ TVFloat }

constructor TVFloat.Create(F: double);
begin
    fF := F;
end;

function TVFloat.Copy: TValue;
begin
    result := TVFloat.Create(self.F);
end;

function TVFloat.AsString: unicodestring;
begin
    result := FloatToStr(F);
end;

function TVFloat.hash: DWORD;
var i: Int64;
begin
    i := round(fF);
    if fF = i
    then crc32(0, @i, SizeOf(i))
    else result := crc32(0, @fF, SizeOf(fF));
end;

function TVFloat.F: double;
begin
    result := fF;
end;

function TVFloat.C: COMPLEX;
begin
    result.im := 0;
    result.re := fF;
end;


{ TVString }

function TVString.GetItem(index: integer): TValue;
begin
    result := TVString.Create(S[index+1]);
end;

procedure TVString.SetItem(index: integer; _V: TValue);
begin
    S[index+1] := (_V as TVString).S[1];
    _V.Free;
end;

constructor TVString.Create(S: unicodestring);
begin
    self.S := S;
end;

destructor TVString.Destroy;
begin
    self.S := '';
    inherited;
end;


function TVString.Copy: TValue;
begin result := TVString.Create(self.S); end;

function TVString.AsString: unicodestring;
var i :integer;
begin
    result := '"';
    for i:= 1 to Length(S) do
        case s[i] of
            '"', '\': result := result + '\' + s[i];
            else result := result + s[i];
        end;
    result := result + '"';
end;

function TVString.hash: DWORD;
begin
    result := crc32;
end;

function TVString.equal(V: TValue): boolean;
begin
    result := (V is TVString) and (S=(V as TVString).S);
end;

function TVString.Count: integer;
begin
    result := Length(S);
end;

function TVString.crc32: DWORD;
var i, cp: DWORD;
begin
    result := 10005;
    for i := 1 to Length(S) do begin
        cp := Ord(S[i]);
        result := crc.crc32(result, @cp, SizeOf(cp));
    end;
end;

function TVString.subseq(istart: integer; iend: integer = -1): TValue;
begin
    if iend<0 then iend := Length(S);
    result := TVString.Create(S[istart+1..iend]);
end;


{ TVInteger }

function    TVInteger.Copy: TValue;
begin
    result := TVInteger.Create(fI);
end;

function    TVInteger.AsString: unicodestring; begin result := IntToStr(fI); end;

function TVInteger.hash: DWORD;
begin
    result := crc32(0, @fI, SizeOf(fI));
end;

function TVInteger.equal(V: TValue): boolean;
begin
    Result := (V.ClassType=TVInteger) and (fI=(V as TVInteger).fI);
end;

function TVInteger.F: double;
begin
    result := fI;
end;

function TVInteger.C: COMPLEX;
begin
    result.im := 0;
    result.re := fI;
end;

constructor TVInteger.Create(I: Int64); begin fI := I; end;


  { TVSymbol }

function TVSymbol.Copy: TValue;
begin
    result := TVSymbol.CreateEmpty;
    (result as TVSymbol).fname := self.fname;
    //(result as TVSymbol).funame := self.funame;
    (result as TVSymbol).fN := self.fN;
    //(result as TVSymbol).fKeyword := self.fKeyword;
end;

function TVSymbol.AsString: unicodestring;
begin
    result := fname;
end;

function TVSymbol.hash: DWORD;
begin
    if n>=0
    then result := N
    else result := $FFFFFFFF+N;
end;

function TVSymbol.equal(V: TValue): boolean;
begin
    result := (V is TVSymbol) and (N=(V as TVSymbol).N);
end;

class function TVSymbol.symbol_n(n: unicodestring): integer;
var i: integer; uname: unicodestring;
begin
    uname := UnicodeUpperCase(n);
    result := -1;

    for i := high(symbols) downto 0 do
        if symbols[i] = uname then begin
            result := i;
            break;
        end;

    if result<0 then begin
        SetLength(symbols, length(symbols)+1);
        result := high(symbols);
        symbols[high(symbols)] := uname;
    end;

end;

class function TVSymbol.symbol_uname(nN: integer): unicodestring;
begin
    result := symbols[nN];
end;

constructor TVSymbol.Gensym;
begin
    fN := gensym_n;
    Dec(gensym_n);
    fName := '#G'+IntToStr(fN);
end;

constructor TVSymbol.CreateEmpty;
begin

end;

function TVSymbol.fGetUname: unicodestring;
begin
    if fN<0
    then result := fname
    else result := symbol_uname(fN);
end;

constructor TVSymbol.Create(S: unicodestring);
begin
    fname := S;
    fN := symbol_n(fname);
end;

destructor TVSymbol.Destroy;
begin
    inherited;
end;

{ TVList }

procedure TVList.Add(V: TValue);
begin
    CopyOnWrite;
    fL.Add(V);
end;

procedure TVList.Append(VL: TVList);
var
  i: integer;
begin
    CopyOnWrite;
    fL.Capacity := fL.Capacity + VL.fL.Capacity;

    if fL.OwnsObjects
    then begin
        for i := 0 to VL.fL.Count - 1 do fL.Add((VL.fL[i] as TValue).Copy);
        VL.Free;
    end
    else
        for i := 0 to VL.fL.Count - 1 do fL.Add(VL.fL[i] as TValue);

end;

function TVList.GetElementName(index: integer): unicodestring;
begin
    Assert(fL[index] is TVSymbol, 'Элемент '+IntToStr(index)+' не символ');
    result := (fL[index] as tvSymbol).name;
end;

function TVList.GetElementUName(index: integer): unicodestring;
begin
    Assert(fL[index] is TVSymbol, 'Элемент '+IntToStr(index)+' не символ');
    result := (fL[index] as tvSymbol).uname;
end;

function TVList.GetElementI(index: integer): Int64;
begin
    Assert(fL[index] is TVInteger, 'Элемент '+IntToStr(index)+' не целое');
    result := (fL[index] as tvInteger).fI;
end;

function TVList.GetElementF(index: integer): double;
begin
    Assert(fL[index] is TVReal, 'Элемент '+IntToStr(index)+' не число');
    result := (fL[index] as TVReal).F;
end;

function TVList.GetElementS(index: integer): unicodestring;
begin
    Assert(fL[index] is TVString, 'Элемент '+IntToStr(index)+' не строка');
    result := (fL[index] as tvString).S;
end;

function TVList.GetElementL(index: integer): TVList;
begin
    Assert(fL[index] is TVList, 'Элемент '+IntToStr(index)+' не список');
    result := (fL[index] as TVList);
end;

function TVList.GetElementC(index: integer): COMPLEX;
begin
    Assert(fL[index] is TVNumber, 'Элемент '+IntToStr(index)+' не комплексное число');
    result := (fL[index] as TVNumber).C;
end;

function TVList.LookElementSYM(index: integer): TVSymbol;
begin
    Assert(fL[index] is TVSymbol, 'Элемент '+IntToStr(index)+' не символ');
    result := fL[index] as TVSymbol;
end;

function TVList.Count: integer;
begin
    result := fL.Count;
end;

procedure TVList.SetCapacity(c: integer);
begin
    CopyOnWrite;
    self.fL.Capacity:=c;
end;

function TVList.extract(n: integer): TValue;
begin
    CopyOnWrite;
    result := fL.Items[n] as TValue;
    fL.Delete(n);
end;

procedure TVList.delete(n: integer);
begin
    CopyOnWrite;
    fL.Delete(n);
end;

procedure TVList.insert(n: integer; V: TValue);
begin
    CopyOnWrite;
    fl.Insert(n, V);
end;

function TVList.Copy: TValue;
begin
    Inc(fL.ref_count);
    result := TVList.Create(fL);
end;


function TVList.AsString: unicodestring;
var i: integer;
begin
    if count=0
    then result := 'NIL'
    else begin
        result := '(';
        for i := 0 to fL.Count-1 do
            result := result + (fL[i] as TValue).AsString() + ' ';
        result[length(result)]:=')';
    end;
end;

function TVList.hash: DWORD;
var h: DWORD; i: integer;
begin
    result := 10006;
    for i := 0 to high do begin
        h := (fL[i] as TValue).hash;
        result := crc32(result, @h, SizeOf(h));
    end;
end;

function TVList.equal(V: TValue): boolean;
var VL: TVList; i: integer;
begin
    result := V is TVList;
    if not result then Exit;

    VL := V as TVList;
    result := fL=VL.fL;
    if result then Exit;

    result := Count=VL.Count;
    if not result then Exit;

    for i := 0 to high do begin
        result := look[i].equal(VL.look[i]);
        if not result then Exit;
    end;
end;

constructor TVList.Create;
begin
  fL := TListBody.Create(true);
end;

constructor TVList.Create(VL: array of TValue; free_objects: boolean);
var i :integer;
begin
    fL := TListBody.Create(free_objects);
    fL.Capacity:=length(VL);
    for i:=0 to Length(VL)-1 do fL.Add(VL[i]);
end;

constructor TVList.Create(body: TListBody);
begin
    fL := body;
end;


destructor TVList.Destroy;
begin
    Dec(fl.ref_count);
    if fL.ref_count=0 then fL.Free;
    inherited;
end;

function TVList.GetItem(index: integer): TValue;
begin
    result := (fL[Index] as TValue).Copy;
end;

procedure TVList.SetItem(index: integer; _V: TValue);
begin
    CopyOnWrite;
    fL[Index] := _V;
end;

function TVList.LookItem(index: integer): TValue;
begin
    result := (fL[Index] as TValue);
end;

function TVList.subseq(istart: integer; iend: integer = -1): TValue;
var i: integer;
begin
    if iend<0 then iend := fL.Count;
    result := TVList.Create;
    (result as TVList).fL.Capacity := iend - istart;
    for i := istart to iend - 1 do
        (result as TVList).fL.Add((fL[i] as TValue).Copy);
end;


function TVList.CopyOnWrite: boolean;
var i: integer; fL_old: TListBody;
begin
    if fL.ref_count>1 then begin
        fL_old := fL;
        fL := TListBody.Create(true);
        fL.Capacity := fL_old.Count;
        for i := 0 to fL_old.Count-1 do fL.Add((fL_old[i] as TValue).Copy);
        Dec(fL_old.ref_count);
        if fL_old.ref_count<=0 then fL_old.Free;
        result := true;
    end
    else result := false;
end;

function TVList.POP: TValue;
begin
    CopyOnWrite;
    result := (fL.Last as TValue).Copy();
    fL.Delete(fL.Count-1);
end;

procedure TVList.Clear;
begin
    CopyOnWrite;
    fL.Clear;
end;

function TVList.ValueList: TValueList;
var i: integer;
begin
    setLength(result, fL.Count);
    for i:=0 to fL.Count-1 do result[i] := (fL[i] as TValue)
end;

function TVList.CdrValueList: TValueList;
var i: integer;
begin
    setLength(result, fL.Count-1);
    for i:=1 to fL.Count-1 do result[i-1] := (fL[i] as TValue)
end;

function TVList.CAR: TValue;
begin
    if fL.Count=0
    then result := TVList.Create
    else result := (fL[0] as TValue).Copy();
end;

function TVList.CDR: TVList;
var i: integer;
begin
    result := TVList.Create;
    result.fL.Capacity:= fL.Count - 1;
    for i:=1 to fL.Count-1 do result.Add((fL[i] as TValue).Copy());
end;


initialization
    _ := TVSymbol.Create('_');
    kwFLAG := TVKeyword.Create(':FLAG');
    kwKEY := TVKeyword.Create(':KEY');
    kwOPTIONAL := TVKeyword.Create(':OPTIONAL');
    kwREST := TVKeyword.Create(':REST');

finalization
    kwREST.Free;
    kwOPTIONAL.Free;
    kwKEY.Free;
    kwFLAG.Free;
    _.Free;
end.  //3477 3361 3324 3307 2938  2911 2988
