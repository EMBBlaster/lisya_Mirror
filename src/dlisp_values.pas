﻿unit dlisp_values;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$ASSERTIONS ON}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    SysUtils, Classes, Contnrs, ucomplex, crc,
    lisia_charset, zstream, mar;


const
    {$ifdef windows}
    dir_separator = '\';
    new_line: unicodestring = #13+#10;
    {$else windows}
    dir_separator = '/';
    new_line: unicodestring = #10;
    {$endif windows}


type

    { ELisyaError }

    ELisyaError              = class (Exception)
        EClass: unicodestring;
        EStack: unicodestring;
        constructor InvalidParameters;
        constructor Create(msg: unicodestring; ec: unicodestring=''; es: unicodestring='');
        constructor Malformed(msg: unicodestring);
        constructor Stream(msg: unicodestring);
        destructor Destroy; override;
    end;
    ELE                      = ELisyaError;
    EObjectNotCompound       = class (ELisyaError) end;
    EInvalidParameters       = class (ELisyaError) end;

    { TValue }

    TValue = class
        function Copy(): TValue; virtual; abstract;
        function AsString(): unicodestring; virtual; abstract;
        function hash: DWORD; virtual; abstract;
    end;
    TValueList = array of TValue;

    { TVEndOfStream }

    TVEndOfStream = class (TValue)
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

    /////  Stack /////////////////////

    { TVariable }

    TVariable = record
        V: TValue;
        constant: boolean;
    private
        ref_count: integer;
        critical_section: TRTLCriticalSection;
        procedure enter_critical_section;
        procedure leave_critical_section;
    end;
    PVariable = ^TVariable;


    TStackRecord = record
        name: unicodestring;
        V: PVariable;
        N: integer;
    end;


    { TVT }

    TVT = class (TValue)
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
    end;


    TVNumber = class (TValue)
        function C: COMPLEX; virtual; abstract;
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
    end;


    { TVInteger }

    TVInteger = class (TVReal)
        fI: Int64;
        constructor Create(I: Int64);
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


    { TVTime }

    TVTime = class (TValue)
        fDT: TDateTime;

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

        class function symbol_n(n: unicodestring): integer;
        class function symbol_uname(nN: integer): unicodestring;
    end;


    { TVKeyword }

    TVKeyword = class (TVSymbol)
        constructor CreateEmpty;
        function Copy: TValue; override;
    end;


    TVGo = class (TValue);


    { TVGoto }

    TVGoto = class (TVGo)
        uname: unicodestring;
        n: integer;
        constructor Create(name: unicodestring); overload;
        constructor Create(mark: TVSymbol); overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
    end;


    { TVBreak }

    TVBreak = class (TVGo)
        function Copy: TVBreak; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
    end;


    { TVContinue }

    TVContinue = class (TVGo)
        function Copy: TVContinue; override;
        function AsString: unicodestring; override;
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

    TVPrimitive = class (TValue)
        //класс заглушка, позволяющий функции look вернуть информацию о том,
        //что компонент является примитивным типом
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
    end;


    { TVChainPointer }

    TVChainPointer = class (TValue)
    private
        primitive: TVPrimitive;
        V: PVariable;
        index: array of integer;
    public
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
        procedure enter_critical_section;
        procedure leave_critical_section;
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


    { TVCompoundIndexed }

    TVCompoundIndexed = class (TVCompound)
        function high: integer;
        function subseq(istart: integer; iend: integer = -1): TValue; virtual; abstract;
        //function append(ss: TVCompoundIndexed): TValue; virtual; abstract;
    end;

    TVCompoundOfPrimitive = class (TVCompoundIndexed)

    end;


    { TVString }

    TVString = class (TVCompoundOfPrimitive)
    private
        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem({%H-}index: integer): TValue; override;
      public
        S: unicodestring;
        constructor Create(S: unicodestring);
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;


        function Count: integer; override;
        function crc32: DWORD;

        function subseq(istart: integer; iend: integer = -1): TValue; override;
    end;


    { TVList }

    { TListBody }

    TListBody = class (TObjectList)
        ref_count: integer;
        critical_section: TRTLCriticalSection;
        constructor Create(free_objects: boolean);
        destructor Destroy;
        procedure enter_critical_section;
        procedure leave_critical_section;
    end;

    TVList = class (TVCompoundIndexed)
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
        constructor Create(VL: array of TValue); overload;
        constructor Create(body: TListBody); overload;
        //constructor CreatePhantom;
        destructor Destroy; override;
        function Copy(): TValue; override;
        //function Phantom_Copy: TVList;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;

        function Count: integer; override;

        function subseq(istart: integer; iend: integer = -1): TValue; override;
        //function phantom_subseq(istart: integer; iend: integer = -1): TVList;

        function CopyOnWrite: boolean;
        procedure Add(V: TValue);
        procedure Append(VL: TVList);

        //procedure Add_phantom(V: TValue);

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
        function POP: TValue;
        procedure Clear;
        function ValueList: TValueList;
        function CdrValueList: TValueList;
        function CAR: TValue;
        function CDR: TVList;
        //function Phantom_CDR: TVList;
    end;


    { TVByteVector }

    TVByteVector = class (TVCompoundOfPrimitive)
    private
        function GetByte(Index: Integer): Int64;
        procedure SetByte(Index: Integer; V: Int64);

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem({%H-}index: integer): TValue; override;

    public
        fBytes: TBytes;

        constructor Create; overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;

        property bytes[Index: integer]: Int64 read GetByte write SetByte; default;
        procedure SetCount(l: integer);
        procedure Add(b: Int64);

        function equal_to(BV: TVByteVector): boolean;
        function crc32: DWORD;

        function count: integer; override;
        function subseq(istart: integer; iend: integer = -1): TValue; override;
        procedure append(BV: TVByteVector);
    end;


    { TVRecord }

    TVRecord = class (TVCompound)
    private
        //unames: TStringList;
        unames: array of integer;
        slots: TObjectList;
        function fGetSlot(index: unicodestring): TValue;
        procedure fSetSlot(index: unicodestring; V: TValue);
        function flook(index: unicodestring): TValue;

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;

        function index_of(nN: integer): integer;
    public
        constructor Create(names: array of unicodestring); overload;
        //constructor Create(names: TStringList); overload;
        constructor Create; overload;
        destructor Destroy; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;

        function GetSlot(nN: integer): TValue;
        procedure SetSlot(nN: integer; V: TValue);
        function LookSlot(nN: integer): TValue;

        //property slot[index: unicodestring]:TValue read fGetSlot write fSetSlot;
        property slot[nN: integer]:TValue read GetSlot write SetSlot;
        property Items[index: integer]: TValue read GetItem write SetItem; default;
        property look[index: integer]: TValue read LookItem;
        property look_name[n: unicodestring]: TValue read flook;
        function is_class(cl: TVRecord): boolean;
        procedure AddSlot(name: unicodestring; V: TValue); overload;
        //procedure AddSlot(nN: integer; V: TValue); overload;
        procedure AddSlot(name: TVSymbol; V: TValue); overload;




        function count: integer; override;
        function name_n(n: integer): unicodestring;
        //TODO: сравнение записей должно быть независимым от порядка слотов
        function get_n_of(index: unicodestring): integer;
    end;



    { TVHashTable }

    TVHashTable = class (TValue)
    private
        data: TVList;
        fCount: integer;
        procedure Expand;
    public
        constructor Create;
        destructor Destroy; override;
        function AsString: unicodestring; override;
        function Copy: TValue; override;
        function hash: DWORD; override;
        procedure print;


        procedure Add(key: TValue; V: TValue);
        function Get(key: TValue): TValue;
        procedure Clear;
    end;

    { TVSymbolStack }

    TVSymbolStack = class (TValue)
    //TODO: требуется упорядочить и защитить от многопоточности работу со стеком
        parent: TVSymbolStack;
        stack: array of TStackRecord;

        function index_of(name: unicodestring): integer; overload;
        function index_of(n: integer): integer; overload;

        constructor Create(parent: TVSymbolStack);
        destructor Destroy; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function Count: integer; //override;
        function set_n(n: integer; V: TValue): boolean; //override;
        //function get_n(n: integer): TValue; //override;

        procedure Print(n: integer = -1); overload;

        procedure new_var(name: unicodestring; V: TValue; c: boolean = false); overload;
        procedure new_var(symbol: TVSymbol; V: TValue; c: boolean = false); overload;
        procedure new_var(N: integer; V: TValue; c: boolean = false); overload;
        //function find_var(name: unicodestring): TValue; overload;
        function find_var(symbol: TVSymbol): TValue; overload;
        function find_var(N: integer): TValue; overload;
        //procedure set_var(name: unicodestring; V: TValue); overload;
        procedure set_var(symbol: TVSymbol; V: TValue); overload;
        procedure clear_frame(n: integer = -1);
        //procedure bind_var(name, target: unicodestring); overload;
        procedure bind_var(symbol, target: TVSymbol); overload;
        //function find_ref(name: unicodestring): PVariable; overload;
        function find_ref(symbol: TVSymbol): PVariable; overload;
        function find_ref(N: integer): PVariable; overload;
        function look_var(N: integer): PVariable;
        procedure new_ref(name: unicodestring; P: PVariable); overload;
        procedure new_ref(symbol: TVSymbol; P: PVariable); overload;

        //function find_ref_or_nil(name: unicodestring): PVariable; overload;
        function find_ref_or_nil(symbol: TVSymbol): PVariable; overload;
        function find_ref_in_frame_or_nil(name: unicodestring; n: integer): PVariable; overload;
        function find_ref_in_frame_or_nil(symbol: TVSymbol; n: integer): PVariable; overload;
        function find_ref_in_frame_or_nil(nN: integer; n: integer): PVariable; overload;
        procedure remove_unbound;
    end;


    { TVSubprogram }

    TSubprogramParmeterMode = (spmNec, spmOpt, spmKey, spmRest, spmFlag,
        spmCaptured);
    TSubprogramParameterDescription = record
        n: unicodestring;
        m: TSubprogramParmeterMode;
        r: boolean;
    end;
    TSubprogramSignature = array of TSubprogramParameterDescription;


    TVSubprogram = class (TValue)
        nN: integer;
        //TODO: нужно пересмотреть дерево классов подпрограмм
        //поле signature используется только внутренними функциями
        //TVProcedure использует поле fsignature
        //поле stack не используется операторами и внутренними функциями
    end;


    { TVProcedure }

    TVProcedure = class (TVSubprogram)
        //is_macro: boolean;
        //is_macro_symbol: boolean;
        evaluated: boolean;
        //fsignature: TSubprogramSignature;
        sign: TVList;
        stack_pointer: integer;
        stack: TVSymbolStack;
        rests: TVRecord;
        home_stack: TVSymbolStack;
        body: TVList;
        constructor Create;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;

        procedure Complement;
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
    end;

    { TVPredicate }

    type TTypePredicate = function (V: TValue): boolean;

    TVPredicate = class (TVInternalSubprogram)
        body: TTypePredicate;
        constructor Create(name: unicodestring; _body: TTypePredicate); overload;
        constructor Create(_nN: integer; _body: TTypePredicate); overload;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;
    end;

    { TVOperator }

    type TOperatorEnum = (
            oeAND,
            oeAPPEND,
            oeAPPLY,
            oeBLOCK,
            oeBREAK,
            oeCASE,
            oeCOND,
            oeCONST,
            oeCONTINUE,
            oeDEBUG,
            oeDEFAULT,
            oeELT,
            oeERROR,
            oeEXECUTE_FILE,
            oeFILTER,
            oeFOLD,
            oeFOR,
            oeGOTO,
            oeIF,
            oeIF_NIL,
            oeLAST,
            oeLET,
            oeMACRO,
            oeMACRO_SYMBOL,
            oeMAP,
            oeOR,
            oePACKAGE,
            oePOP,
            oePROCEDURE,
            oePUSH,
            oeQUOTE,
            oeRECORD,
            oeRECORD_AS,
            oeRETURN,
            oeSET,
            oeUSE,
            oeVAL,
            oeVAR,
            oeWHEN,
            oeWHILE,
            oeWITH);
    //and append block break case cond const continue default elt exception filter for goto if if-nil last let map or pop procedure push quote set stack structure structure-as val var when while

    TVOperator = class (TVInternalSubprogram)
        op_enum: TOperatorEnum;

        constructor Create(name: unicodestring; en: TOperatorEnum); overload;
        constructor Create(_nN: integer; en: TOperatorEnum); overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
        function hash: DWORD; override;
    end;


    TFileMode = (fmRead, fmWrite, fmAppend);
    TStreamEncoding = (seBOM, seUTF8, seCP1251, seCP1252, seUTF16BE, seUTF16LE, seUTF32BE,
                        seUTF32LE, seCP866, seKOI8R);


    { TVStream }

    TVStream = class (TValue)
        fstream: TStream;
        encoding: TStreamEncoding;
        mutex: TRTLCriticalSection;
        external_mutex: boolean;

        constructor Create; overload;
        constructor Create(m: TRTLCriticalSection); overload;
        destructor Destroy; override;
        function Copy: TValue; override;

        function hash: DWORD; override;

        function read_byte(out b: byte): boolean;
        function read_bytes(var bb: TBytes; count: integer = -1): boolean;
        function write_byte(b: byte): boolean;
        function write_bytes(bb: TBytes): boolean;

        procedure read_BOM; virtual;
        procedure write_BOM;

        function read_char(out ch: unicodechar): boolean;
        function write_char(ch: unicodechar): boolean;
    end;


    { TVFileStream }

    TVFileStream = class (TVStream)
        file_name: unicodestring;

        constructor Create(fn: unicodestring; mode: TFileMode;
            enc: TStreamEncoding = seUTF8);

        function AsString: unicodestring; override;
    end;


    { TVInflateStream }

    TVInflateStream = class (TVStream)
        target: PVariable;

        constructor Create(_target: PVariable; enc: TStreamEncoding = seUTF8;
            head: boolean = false);
        destructor Destroy; override;

        procedure read_BOM; override;
        function AsString: unicodestring; override;
    end;


    { TVDeflateStream }

    TVDeflateStream = class (TVStream)
        target: PVariable;

        constructor Create(_target: PVariable; enc: TStreamEncoding = seUTF8;
            head: boolean = false);
        destructor Destroy; override;

//        procedure read_BOM; override;
        function AsString: unicodestring; override;
    end;

    { TVMemoryStream }

    TVMemoryStream = class (TVStream)
        constructor Create; overload;
        constructor Create(const bb: TBytes); overload;
        constructor Create(const s: unicodestring); overload;

        function AsString: unicodestring;
    end;

    { TVStreamPointer }

    TVStreamPointer = class (TValue)
        body: PVariable;
        stream: TVStream;

        constructor Create(_body: PVariable);
        destructor Destroy; override;

        function Copy: TValue; override;
        function AsString: unicodestring; override;
        function hash: DWORD; override;

        procedure close_stream;
        procedure lock;
        procedure unlock;
    end;


procedure Assign(var v1, v2: TValue);

function op_null(V: TValue): boolean;

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
function RefVariable(P: PVariable): PVariable;
procedure ReleaseVariable(var P: PVariable);

var symbols: array of unicodestring;
    symbols_mutex: TRTLCriticalSection;
    console_mutex: TRTLCriticalSection;


implementation

uses lisya_ifh;

var gensym_n: Int64 = -1;



    { TVariable }

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
begin
    New(result);
    result.ref_count:=1;
    result.constant := _constant;
    result.V := _V;
    InitCriticalSection(result.critical_section);

   // WriteLn('NewVariable>> ', IntToHex(qword(result),8));
end;

function RefVariable(P: PVariable): PVariable;
begin
    if P<> nil then begin
        P.enter_critical_section;
        Inc(P.ref_count);
        P.leave_critical_section;
        result := P;
    end
    else
        result := nil;
end;

procedure ReleaseVariable(var P: PVariable);
var no_refs: boolean;
begin
    if P<>nil then begin
        P.enter_critical_section;
        Dec(P.ref_count);
        no_refs := P.ref_count<=0;
        P.leave_critical_section;
        if no_refs then begin
            P.V.Free;
            DoneCriticalSection(P.critical_section);
            Dispose(P);
            P:= nil;
        end;
    end;
end;


procedure Assign(var v1, v2: TValue);
begin
  v1.Free;
  v1 := v2;
  FreeAndNil(v2);
end;

function op_null(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).count=0);
end;

{ TVMacro }

function TVMacro.AsString: unicodestring;
var
    PL: unicodestring;
    i: integer;

    function m(pd: TSubprogramParmeterMode): unicodestring;
    begin
        case pd of
            spmNec: result := ':n';
            spmKey: result := ':k';
            spmOpt: result := ':o';
            spmRest: result := ':r';
        end;
    end;

begin
    PL := '';
    if nN<0
    then result := '#<MACRO'+sign.AsString+'>'
    else result := '#<MACRO '+symbols[nN]+'>';
end;

{ TVHashTable }

procedure TVHashTable.Expand;
var old_data: TVList; i, j, capacity: integer;
begin
    if fCount<(data.Count div 4) then Exit;

    old_data := data;
    data := TVList.Create;
    capacity := old_data.Count*2;
    data.SetCapacity(capacity);
    for i := 1 to capacity do data.Add(TVList.Create);

    for i := 0 to old_data.high do
        for j := 0 to old_data.L[i].high do begin
            data.L[old_data.L[i].L[j].I[0] mod capacity].Add(old_data.L[i][j]);
        end;
    old_data.Free;
end;


constructor TVHashTable.Create;
begin
    data := TVList.Create([TVList.Create]);
    fCount := 0;
end;

destructor TVHashTable.Destroy;
begin
    data.Free;
    inherited Destroy;
end;

function TVHashTable.AsString: unicodestring;
begin
    result := '#<HASH-TABLE '+IntToStr(fCount)+'/'+IntToStr(data.Count)+'>';
end;

function TVHashTable.Copy: TValue;
begin
    result := TVHashTable.Create;
    (result as TVHashTable).data := data.Copy as TVList;
    (result as TVHashTable).fCount := fCount;
end;

function TVHashTable.hash: DWORD;
begin
    result := 10008;
end;

procedure TVHashTable.print;
var i: integer;
begin
    WriteLn('--------',AsString,'--------');
    for i := 0 to data.high do
        if data.L[i].Count>0 then WriteLn(i,'| ',data.L[i].AsString);
    WriteLn('--------',AsString,'--------');
end;

procedure TVHashTable.Add(key: TValue; V: TValue);
var h: DWORD; index, i: integer; nest: TVList;
begin
    Expand;
    h := key.hash;

    data.CopyOnWrite;
    nest := data.L[h mod data.Count];
    nest.CopyOnWrite;

    for i := 0 to nest.high do begin
        if (nest.L[i].I[0]=h) and ifh_equal(nest.L[i].look[1], key) then begin
            nest.L[i][2] := V;
            exit;
        end;
    end;

    nest.Add(TVList.Create([TVInteger.Create(h), key.Copy, V]));
    Inc(fCount);
end;

function TVHashTable.Get(key: TValue): TValue;
var h: DWORD; i: integer; nest: TVList;
begin
    h := key.hash;

    nest := data.L[h mod data.Count];

    //WriteLn(nest.AsString);
    for i := 0 to nest.high do begin
        if (nest.L[i].I[0]=h) and ifh_equal(nest.L[i].look[1], key) then begin
            result := nest.L[i][2];
            exit;
        end;
    end;
    result := TVList.Create;
end;

procedure TVHashTable.Clear;
begin

end;

{ TVariable }

procedure TVariable.enter_critical_section;
begin
    EnterCriticalSection(critical_section);
end;

procedure TVariable.leave_critical_section;
begin
    LeaveCriticalSection(critical_section);
end;

{ TListBody }

constructor TListBody.Create(free_objects: boolean);
begin
    inherited Create(free_objects);
    ref_count := 1;
    InitCriticalSection(critical_section);
end;

destructor TListBody.Destroy;
begin
    DoneCriticalSection(critical_section);
end;

procedure TListBody.enter_critical_section;
begin
    EnterCriticalSection(critical_section);
end;

procedure TListBody.leave_critical_section;
begin
    leaveCriticalSection(critical_section);
end;


{ TVDeflateStream }

constructor TVDeflateStream.Create(_target: PVariable; enc: TStreamEncoding;
    head: boolean);
begin
    encoding := enc;
    target := _target;
    //WriteLn('inflate>> ',target.V.AsString());
    fStream := TCompressionStream.create(clDefault,
        (target.V as TVStream).fstream, not head);
    inherited Create((target.V as TVStream).mutex);
end;

destructor TVDeflateStream.Destroy;
begin
    ReleaseVariable(target);
    inherited Destroy;
end;

function TVDeflateStream.AsString: unicodestring;
begin
    result := '#<DEFLATE-STREAM '+target.V.AsString+'>';
end;

{ TVMemoryStream }

constructor TVMemoryStream.Create;
begin
    encoding := seUTF8;
    fstream := TMemoryStream.Create;
    inherited Create;
end;

constructor TVMemoryStream.Create(const bb: TBytes);
var i: integer;
begin
    encoding := seUTF8;
    fstream := TMemoryStream.Create;
    for i := 0 to high(bb) do fstream.WriteByte(bb[i]);
    inherited Create;
end;

constructor TVMemoryStream.Create(const s: unicodestring);
var i: integer;
begin
    encoding := seUTF8;
    fstream := TMemoryStream.Create;
    for i := 1 to Length(s) do self.write_char(s[i]);
    inherited Create;
end;

function TVMemoryStream.AsString: unicodestring;
begin
    result := '<#MEMORY STREAM>'
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

constructor TVPredicate.Create(name: unicodestring; _body: TTypePredicate);
begin
    nN := TVSymbol.symbol_n(name);
    body := _body;
end;

constructor TVPredicate.Create(_nN: integer; _body: TTypePredicate);
begin
    nN := _nN;
    body := _body;
end;

function TVPredicate.Copy: TValue;
begin
    result := TVPredicate.Create(nN, body);
end;

function TVPredicate.AsString: unicodestring;
begin
    result := '#<PREDICATE '+symbols[nN]+'>';
end;

function TVPredicate.hash: DWORD;
begin
    result := crc32(0, @body, SizeOf(body));
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

constructor TVStreamPointer.Create(_body: PVariable);
begin
    body := _body;
    stream := body.V as TVStream;
end;

destructor TVStreamPointer.Destroy;
begin
    ReleaseVariable(body);
    inherited Destroy;
end;

function TVStreamPointer.Copy: TValue;
begin
    result := TVStreamPointer.Create(RefVariable(body));
end;

function TVStreamPointer.AsString: unicodestring;
begin
    result := '#<STREAM-POINTER '+body.V.AsString+'>';
end;

function TVStreamPointer.hash: DWORD;
begin
    if body=nil
    then result := 10007
    else result := crc32(0, @stream, SizeOf(stream));
end;

procedure TVStreamPointer.close_stream;
begin
    FreeAndNil(stream.fstream);
    stream := nil;
end;

procedure TVStreamPointer.lock;
begin
    EnterCriticalSection((body.V as TVStream).mutex);
end;

procedure TVStreamPointer.unlock;
begin
    LeaveCriticalSection((body.V as TVStream).mutex);
end;

{ TVInflateStream }

constructor TVInflateStream.Create(_target: PVariable; enc: TStreamEncoding = seUTF8;
    head: boolean = false);
begin
    if enc = seBOM then read_BOM else encoding := enc;
    target := _target;
    //WriteLn('inflate>> ',target.V.AsString());
    fStream := TDecompressionStream.create((target.V as TVStream).fstream, not head);
    inherited Create((target.V as TVStream).mutex);
end;

destructor TVInflateStream.Destroy;
begin
    ReleaseVariable(target);
    inherited Destroy;
end;

procedure TVInflateStream.read_BOM;
begin
    raise ELE.Create('can not read BOM from decompression stream',
                                                        'invalid parameters');
end;

function TVInflateStream.AsString: unicodestring;
begin
    result := '#<INFLATE-STREAM '+target.V.AsString+'>';
end;

{ TVFileStream }

constructor TVFileStream.Create(fn: unicodestring; mode: TFileMode;
    enc: TStreamEncoding = seUTF8);
begin
    inherited Create;
    file_name := fn;
    case mode of
        fmRead: begin
            fStream := TFileStream.Create(fn, fmOpenRead);
        end;
        fmWrite: begin
            fStream := TFileStream.Create(fn, fmCreate);
        end;
        fmAppend: begin
            if FileExists(fn)
            then fStream := TFileStream.Create(fn, fmOpenReadWrite)
            else fStream := TFileStream.Create(fn, fmCreate);
            fStream.Seek(fStream.Size,0);
        end;
    end;
    if enc = seBOM then read_BOM else encoding := enc;
end;


function TVFileStream.AsString: unicodestring;
begin
    result := '#<FILE-STREAM '+file_name+'>';
end;

{ TVStream }

constructor TVStream.Create;
begin
    InitCriticalSection(mutex);
    external_mutex := false;
end;

constructor TVStream.Create(m: TRTLCriticalSection);
begin
    mutex := m;
    external_mutex := true;
end;

destructor TVStream.Destroy;
begin
    fstream.Free;
    if not external_mutex then DoneCriticalSection(mutex);
    inherited Destroy;
end;

function TVStream.Copy: TValue;
begin
    result := nil;
    raise ELE.Create('копирование потока', 'internal');
end;

function TVStream.hash: DWORD;
begin
    result := crc32(0, @fStream, SizeOf(fStream));
end;

function TVStream.read_byte(out b: byte): boolean;
begin
    Assert(fstream<>nil, 'operation on closed stream');
try
    //EnterCriticalSection(mutex);
try
    b := fstream.ReadByte;
    result := true;
except
    on E:EStreamError do result := false;
end;
finally
    //LeaveCriticalSection(mutex);
end;
end;

function TVStream.read_bytes(var bb: TBytes; count: integer): boolean;
var i: integer;
begin try
    Assert(fstream<>nil, 'operation on closed stream');
    //EnterCriticalSection(mutex);
    if count>0 then begin
        SetLength(bb, count);
        for i := 0 to count-1 do bb[i] := fStream.ReadByte;
        //fStream.ReadBuffer(bb, count);
        //TODO: по загадочным причинам readbuffer вызывает разрушение памяти при освобождении буфера
        result := true;
    end
    else begin
        //WriteLn('size>> ', fStream.Size, '   ', fStream.Position);
        SetLength(bb, fStream.Size - fStream.Position);
        for i := 0 to fStream.Size - fStream.Position - 1 do bb[i] := fStream.ReadByte;
        //fStream.ReadBuffer(bb, fStream.Size - fStream.Position);
        result := true;
    end;
finally
    //LeaveCriticalSection(mutex);
end;
end;

function TVStream.write_byte(b: byte): boolean;
begin
    Assert(fstream<>nil, 'operation on closed stream');
    try
       // EnterCriticalSection(mutex);
        fstream.WriteByte(b);
    finally
      //  LeaveCriticalSection(mutex);
    end;
    result := true;
end;

function TVStream.write_bytes(bb: TBytes): boolean;
var i: integer;
begin
    Assert(fstream<>nil, 'operation on closed stream');
//    WriteLn('write_bytes>> ', Length(bb));
    try
       // EnterCriticalSection(mutex);
        for i := 0 to high(bb) do fStream.WriteByte(bb[i]);
    finally
       // LeaveCriticalSection(mutex);
    end;
    //TODO: writeBuffer глючит так же как readbuffer
    //fStream.WriteBuffer(bb, Length(bb));
    result := true;
end;

procedure TVStream.read_BOM;
    function b: byte; begin result := fstream.ReadByte; end;
var p: integer;
begin
    Assert(fstream<>nil, 'operation on closed stream');
    p := (fstream as TFileStream).Position;
    //UTF-8
    if (b=$EF) and (b=$BB) and (b=$BF) then begin
        encoding := seUTF8;
        exit;
    end else fstream.Seek(p,0);
    //UTF16BE
    if (b=$FE) and (b=$FF) then begin
        encoding := seUTF16BE;
        exit;
    end else fstream.Seek(p,0);
    //UTF32BE
    if (b=$00) and (b=$00) and (b=$FE) and (b=$FF) then begin
        encoding := seUTF32BE;
        exit;
    end else fstream.Seek(p,0);
    //UTF32LE
    if (b=$FF) and (b=$FE) and (b=$00) and (b=$00) then begin
        encoding := seUTF32LE;
        exit;
    end else fstream.Seek(p,0);
    //UTF16LE
    if (b=$FF) and (b=$FE) then begin
        encoding := seUTF16LE;
        exit;
    end else fstream.Seek(p,0);

    encoding := seCP1251;
end;

procedure TVStream.write_BOM;
    procedure b(b: byte); begin write_byte(b); end;
begin
    Assert(fstream<>nil, 'operation on closed stream');
    case encoding of
        seUTF8:    begin b($EF); b($BB); b($BF); end;
        seUTF16BE: begin b($FE); b($FF); end;
        seUTF16LE: begin b($FF); b($FE); end;
        seUTF32BE: begin b($00); b($00); b($FE); b($FF); end;
        seUTF32LE: begin b($FF); b($FE); b($00); b($00); end;
    end;
end;

function TVStream.read_char(out ch: unicodechar): boolean;
var b1, b2, b3, b4: byte;
    function read8bit(const enc: TCodePage): boolean;
    begin
        result := read_byte(b1);
        ch := enc[b1];
    end;
begin
    Assert(fstream<>nil, 'operation on closed stream');
    case encoding of
        seUTF8: begin
            //TODO: read_char кошмарик
            result := read_byte(b1);
            if result and ((b1 shr 6)=3) then result := read_byte(b2{%H-});
            if result and ((b1 shr 5)=7) then result := read_byte(b3{%H-});
            if result and ((b1 shr 4)=15) then result := read_byte(b4{%H-});

            ch := unicodechar(b1);
            if (b1 shr 6)=3 then ch := unicodechar(64*(b1 and 31)
                                                    + (b2 and 63));
            if (b1 shr 5)=7 then ch := unicodechar(64*64*(b1 and 15)
                                                    + 64*(b2 and 63)
                                                    + (b3 and 63));
            if (b1 shr 4)=15 then ch :=unicodechar(64*64*64*(b1 and 7)
                                                    + 64*64*(b2 and 63)
                                                    + 64*(b3 and 64)
                                                    + (b4 and 64));
        end;
        seUTF16LE: begin
            result := read_byte(b1) and read_byte(b2);
            ch := unicodechar(b1+b2*256);
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF16BE: begin
            result := read_byte(b1) and read_byte(b2);
            ch := unicodechar(256*b1+b2);
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF32LE: begin
            result := read_byte(b1) and read_byte(b2) and read_byte(b3)
                        and read_byte(b4);
            ch := unicodechar(b1+b2*256+b3*256*256+b4*256*256*256);
        end;
        seUTF32BE: begin
            result := read_byte(b1) and read_byte(b2) and read_byte(b3)
                        and read_byte(b4);
            ch := unicodechar(256*256*256*b1+256*256*b2+256*b3+b4);
        end;
        seCP1251: result := read8bit(cp1251_cp);
        seCP1252: result := read8bit(cp1252_cp);
        seCP866:  result := read8bit(cp866_cp);
        seKOI8R:  result := read8bit(KOI8_R_cp);
        else begin result := read_byte(b1); ch := unicodechar(b1); end;
    end
end;

function TVStream.write_char(ch: unicodechar): boolean;
var cp: integer;
    function write8bit(const enc: TCodePage): boolean;
    var i: byte;
    begin
        for i := 0 to 255 do
            if enc[i]=ch then begin
                result := write_byte(i);
                exit;
            end;
        result := write_byte(ord('?'));
    end;
begin
    Assert(fstream<>nil, 'operation on closed stream');
    cp := ord(ch);
    case encoding of
        seUTF8: case cp of
                0..127: result := write_byte(cp);
                128..2047: result := write_byte((cp shr 6) or 192)
                                and write_byte((cp and 63) or 128);
                2048..65535: result := write_byte((cp shr 12) or 224)
                                and write_byte(((cp shr 6) and 63) or 128)
                                and write_byte((cp and 63) or 128);
                65536..2097152: result := write_byte((cp shr 18) or 240)
                                and write_byte(((cp shr 12) and 63) or 128)
                                and write_byte(((cp shr 6) and 63) or 128)
                                and write_byte((cp and 63) or 128);
                else result := false;
            end;
        seUTF16LE: result := write_byte(cp and $FF) and write_byte(cp shr 8);
        seUTF16BE: result := write_byte(cp shr 8) and write_byte(cp and $FF);
        seUTF32LE: result := write_byte(cp and $FF)
                            and write_byte((cp shr 8) and $FF)
                            and write_byte((cp shr 16) and $FF)
                            and write_byte((cp shr 24) and $FF);
        seUTF32BE: result := write_byte((cp shr 24) and $FF)
                            and write_byte((cp shr 16) and $FF)
                            and write_byte((cp shr 8) and $FF)
                            and write_byte(cp and $FF);
        seCP1251: result := write8bit(cp1251_cp);
        seCP1252: result := write8bit(cp1252_cp);
        seCP866:  result := write8bit(cp866_cp);
        seKOI8R: result := write8bit(KOI8_R_cp);
        else raise ELE.Create('неизвестная кодировка','invalid parameters');
    end
end;

{ TVKeyword }

constructor TVKeyword.CreateEmpty;
begin

end;

function TVKeyword.Copy: TValue;
begin
    result := TVKeyword.CreateEmpty;
    (result as TVKeyword).fN := self.fN;
    (result as TVKeyword).fname := self.fname;
end;


{ TVCompoundIndexed }

function TVCompoundIndexed.high: integer;
begin
    result := self.Count-1;
end;

{ TVPrimitive }

function TVPrimitive.Copy: TValue;
begin
    raise ELE.Create('primitive copy');
    result := nil;
end;

function TVPrimitive.AsString: unicodestring;
begin
    result := '#<PRIMITIVE>'
end;

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
    if (obj is TVList) then (obj as TVList).CopyOnWrite;
    for i := 0 to high(index)-1 do begin
        obj := (obj as TVCompound).look[index[i]];
        if (obj is TVList) then (obj as TVList).CopyOnWrite;
    end;
end;

procedure TVChainPointer.enter_critical_section;
begin
    V.enter_critical_section;
end;

procedure TVChainPointer.leave_critical_section;
begin
    V.leave_critical_section;
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

{ TVByteVector }

function TVByteVector.GetByte(Index: Integer): Int64;
begin
    result := fBytes[index];
end;

procedure TVByteVector.SetByte(Index: Integer; V: Int64);
begin
    if (V<0) or (V>255) then raise ELE.Create('byte', 'out of range');
    fBytes[Index] := V;
end;

function TVByteVector.GetItem(index: integer): TValue;
begin
    result := TVInteger.Create(fBytes[index]);
end;

procedure TVByteVector.SetItem(index: integer; _V: TValue);
begin
    fBytes[index] := (_V as TVInteger).fI;
    _V.Free;
end;

function TVByteVector.LookItem(index: integer): TValue;
begin
    raise ELE.Create('TVByteVector.LookItem');
    result := nil;
end;

constructor TVByteVector.Create;
begin
    SetLength(fBytes,0);
end;

destructor TVByteVector.Destroy;
begin
    SetLength(fBytes,0);
    inherited Destroy;
end;


function TVByteVector.Copy: TValue;
var i: integer; res: TVByteVector;
begin
    res := TVByteVector.Create;
    Setlength(res.fBytes, Length(fBytes));
    for i := 0 to Length(fBytes)-1 do res.fBytes[i] := fBytes[i];
    result := res;
end;

function TVByteVector.AsString: unicodestring;
var i: integer;
begin
    result := '#B(';
    for i := 0 to Length(fBytes)-1 do result := result+' '+IntToHex(fBytes[i],2);
    result := result + ')';
end;

function TVByteVector.hash: DWORD;
begin
    result := crc32;
end;

procedure TVByteVector.SetCount(l: integer);
begin
    SetLength(fBytes, l);
end;

procedure TVByteVector.Add(b: Int64);
begin
    if (b<0) or (b>255) then raise ELE.Create('byte', 'out of range');
    SetLength(fBytes, Length(fBytes)+1);
    fBytes[Length(fBytes)-1] := b;
end;

function TVByteVector.subseq(istart: integer; iend: integer): TValue;
var i, high_i: integer;
begin
    if iend<0 then high_i := length(fBytes)-1 else high_i := iend-1;
    result := TVByteVector.Create;
    SetLength((result as TVByteVector).fBytes, high_i-istart+1);
    for i := istart to high_i do
        (result as TVByteVector).fBytes[i-istart] := fBytes[i];
end;

procedure TVByteVector.append(BV: TVByteVector);
var i, offset: integer;
begin
    offset := Length(fBytes);
    SetLength(fBytes, Length(fBytes)+Length(BV.fBytes));
    for i := 0 to Length(BV.fBytes)-1 do
        fBytes[i+offset] := BV.fBytes[i];

    BV.Free;
end;

//function TVByteVector.High: integer;
//begin
//    result := length(fBytes)-1;
//end;

function TVByteVector.equal_to(BV: TVByteVector): boolean;
var i: integer;
begin
    result := Length(fBytes) = Length(BV.fBytes);
    if result then
        for i := 0 to Length(fBytes)-1 do
            if fBytes[i]<>BV.fBytes[i] then begin
                result := false;
                Exit;
            end;
end;

function TVByteVector.crc32: DWORD;
begin
    result := crc.crc32(0, @fBytes[0], Length(fBytes));
end;

function TVByteVector.count: integer;
begin
    result := Length(fBytes);
end;


{ TVEndOfStream }

function TVEndOfStream.Copy: TValue;
begin
    result := TVEndOfStream.Create;
end;

function TVEndOfStream.AsString: unicodestring;
begin
    result := '#<END OF STREAM>';
end;

{ ELisyaError }

constructor ELisyaError.InvalidParameters;
begin
    inherited Create('invalid parameters');
//    fmessage := 'invalid parameters';
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

{ TVSymbolStack }

function TVSymbolStack.index_of(name: unicodestring): integer;
var uname: unicodestring;
begin
    uname := UpperCaseU(name);
    for result := high(stack) downto 0 do begin
        assert(stack[result].V<>nil,
            stack[result].name+' несвязанный элемент в стэке');
        if stack[result].name = uname then exit;
    end;

    raise ELE.Create(name, 'symbol not bound');
end;

function TVSymbolStack.index_of(n: integer): integer;
begin
    for result := high(stack) downto 0 do begin
        assert(stack[result].V<>nil,
            stack[result].name+' несвязанный элемент в стэке');
        if stack[result].N = n then exit;
    end;


    raise ELE.Create(symbols[n]+' ('+IntToStr(n)+')', 'symbol not bound');
end;

constructor TVSymbolStack.Create(parent: TVSymbolStack);
begin
    self.parent := parent;
    SetLength(stack,0);
    //WriteLn('--------------------> stack +1');
end;

destructor TVSymbolStack.Destroy;
var i: integer;
begin
    //WriteLn('--------------------> stack -1');
    clear_frame(0);
    inherited Destroy;
end;


function TVSymbolStack.Copy: TValue;
var i: integer;
begin
    result := TVSymbolStack.Create(parent);
    SetLength((result as TVSymbolStack).stack, Length(stack));
    for i := 0 to high(stack) do begin
        (result as TVSymbolStack).stack[i].name := stack[i].name;
        (result as TVSymbolStack).stack[i].N := stack[i].N;
        (result as TVSymbolStack).stack[i].V := RefVariable(stack[i].V)
    end;
end;

function TVSymbolStack.AsString: unicodestring;
begin
    result := '#<STACK '+IntToStr(Length(stack))+'>';
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
    stack[n].V.enter_critical_section;
    stack[n].V.V.Free;
    stack[n].V.V := V;
    stack[n].V.leave_critical_section;
    result := true;
end;

//function TVSymbolStack.get_n(n: integer): TValue;
//begin
//    if (n<0) or (n>high(stack))
//    then raise EOutOfBounds.Create('stack '+IntToStr(n));
//
//    result := stack[n].V.V.Copy;
//end;

procedure TVSymbolStack.Print(n: integer);
var i, low_i: integer;
begin
    if n<0
    then low_i := 0
    else low_i := high(stack) - n;
    if low_i<0 then low_i := 0;
    WriteLn('-----------------------');
    for i := low_i to high(stack) do begin
        Write('  | ', stack[i].name);
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
    stack[high(stack)].name := symbols[N];
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

//procedure TVSymbolStack.set_var(name: unicodestring; V: TValue);
//begin
//    set_n(index_of(name), V);
//end;

procedure TVSymbolStack.set_var(symbol: TVSymbol; V: TValue);
begin
    set_n(index_of(symbol.N), V);
end;

procedure TVSymbolStack.clear_frame(n: integer);
var i, _n: integer;
begin
    Assert((n<=high(stack)) or (n=0), 'очистка выше стека');
    if n<0
    then begin
        for i := high(stack) downto 0 do
        if stack[i].name[1]=' ' then begin
            _n := i;
            break;
        end;
    end
    else _n := n;

    //print(_n);
    for i := high(stack) downto _n do
        if (stack[i].V<>nil) and (stack[i].V.ref_count>1) and (stack[i].V.V is TVProcedure)
        then begin
            stack[i].V.enter_critical_section;
            (stack[i].V.V as TVProcedure).Complement;
            stack[i].V.leave_critical_section;
        end;

    for i := high(stack) downto _n do ReleaseVariable(stack[i].V);

    SetLength(stack, _n);
end;

//procedure TVSymbolStack.bind_var(name, target: unicodestring);
//begin
//    SetLength(stack, Length(stack)+1);
//    stack[high(stack)].V := RefVariable(stack[index_of(target)].V);
//    stack[high(stack)].name := UpperCaseU(name);
//end;

procedure TVSymbolStack.bind_var(symbol, target: TVSymbol);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].V := RefVariable(stack[index_of(target.n)].V);
    stack[high(stack)].name := symbol.uname;
    stack[high(stack)].N := symbol.N;
end;

//function TVSymbolStack.find_ref(name: unicodestring): PVariable;
//begin
//    result := RefVariable(stack[index_of(name)].V);
//end;

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
    stack[high(stack)].name := name;
    stack[high(stack)].N := TVSymbol.symbol_n(name);
    stack[high(stack)].V := P;
end;

procedure TVSymbolStack.new_ref(symbol: TVSymbol; P: PVariable);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].name := symbol.uname;
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

function TVSymbolStack.find_ref_in_frame_or_nil(name: unicodestring; n: integer
    ): PVariable;
begin
    result := find_ref_in_frame_or_nil(TVSymbol.symbol_n(name), n);
end;

function TVSymbolStack.find_ref_in_frame_or_nil(symbol: TVSymbol; n: integer
    ): PVariable;
begin
    result := find_ref_in_frame_or_nil(symbol.N, n);
end;

function TVSymbolStack.find_ref_in_frame_or_nil(nN: integer; n: integer
    ): PVariable;
var i: integer;
begin
    for i := n to high(stack) do begin
        if stack[i].N = nN then begin
            result := RefVariable(stack[i].V);
            exit;
        end;
        if stack[i].name[1]=' ' then break;
    end;
    result := nil;
end;

procedure TVSymbolStack.remove_unbound;
var i,j: integer;
begin
    i := high(stack);
    while i>=0 do begin
        if stack[i].V=nil then begin
            for j := i+1 to high(stack) do stack[j-1] := stack[j];
            SetLength(stack, Length(stack)-1);
            if i>high(stack) then dec(i);
        end
        else dec(i);
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


{ TVRecord }

function TVRecord.fGetSlot(index: unicodestring): TValue;
begin
    //result := (slots[unames.IndexOf(index)] as TValue).copy;
    result := (slots[index_of(TVSymbol.symbol_n(index))] as TValue).copy;
end;

procedure TVRecord.fSetSlot(index: unicodestring; V: TValue);
begin
    //slots[unames.IndexOf(index)] := V;
    slots[index_of(TVSymbol.symbol_n(index))] := V;
end;

function TVRecord.flook(index: unicodestring): TValue;
begin
    //TODO: обращение к несуществующему слоту структуры вызывает необрабатываемую ошибку
    //result := slots[unames.IndexOf(index)] as TValue;
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
//    unames := TStringList.Create;
    SetLength(unames, Length(names));
    //unames.Capacity := Length(names);
    slots := TObjectList.create(true);
    slots.Capacity := Length(names);
    for i := low(names) to high(names) do begin
        //unames.Add(UpperCaseU(names[i]));
        unames[i] := TVSymbol.symbol_n(names[i]);
        slots.Add(TVList.Create);
    end;
    //unames.Sort;
end;

//constructor TVRecord.Create(names: TStringList);
//var i: integer;
//begin
//    unames := names;
//    slots := TObjectList.create(true);
//    slots.Capacity := names.Count;
//    for i := 0 to unames.Count-1 do begin
//        unames[i] := UpperCaseU(unames[i]);
//        slots.Add(TVList.Create);
//    end;
//    //unames.Sort;
//end;

constructor TVRecord.Create;
begin
    //unames := TStringList.Create;
    SetLength(unames, 0);
    slots := TObjectList.Create(true);
end;

destructor TVRecord.Destroy;
begin
//    unames.Free;
    SetLength(unames, 0);
    slots.Free;
    inherited Destroy;
end;

function TVRecord.Copy: TValue;
var i: integer;
begin
    //WriteLn('>>', self.AsString);
    result := TVRecord.Create;
    //(result as TVRecord).unames.capacity := unames.count;
    SetLength((result as TVRecord).unames, Length(unames));
    (result as TVRecord).slots.capacity := slots.count;
    for i := low(unames) to high(unames) do begin
        //(result as TVRecord).unames.Add(unames[i]);
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

//procedure TVRecord.AddSlot(nN: integer; V: TValue);
//begin
//    SetLength(unames, Length(unames)+1);
//    unames[high(unames)] := nN;
//    slots.Add(V);
//end;

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

procedure TVRecord.SetSlot(nN: integer; V: tValue);
begin
    slots[index_of(nN)] := V;
end;

function TVRecord.LookSlot(nN: integer): TValue;
begin
    result := slots[index_of(nN)] as TValue;
end;

function TVRecord.count: integer;
begin
    //result := unames.Count;
    result := Length(unames);
end;

function TVRecord.name_n(n: integer): unicodestring;
begin
    //result := unames[n];
    result := TVSymbol.symbol_uname(unames[n]);
end;

function TVRecord.get_n_of(index: unicodestring): integer;
begin
    //result := unames.IndexOf(index);
    result := index_of(TVSymbol.symbol_n(index));
end;


{ TVOperator }

constructor TVOperator.Create(name: unicodestring; en: TOperatorEnum);
begin
    //self.name := UpperCaseU(name);
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
    //сигнатура копируется по ссылке, поскольку операторы не могут быть
    //изменены или полностью уничтожены
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

{ TVContinue }

function TVContinue.Copy: TVContinue;
begin
    result := TVContinue.Create;
end;

function TVContinue.AsString: unicodestring;
begin
    result := '#<CONTINUE>';
end;

function TVContinue.hash: DWORD;
begin
    result := 10003;
end;

{ TVBreak }

function TVBreak.Copy: TVBreak;
begin
    result := TVBreak.Create;
end;

function TVBreak.AsString: unicodestring;
begin
    result := '#<BREAK>';
end;

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
    home_stack := nil;

    sign := nil;
    rests := nil;
    evaluated := false;
    stack_pointer := -1;
end;

destructor TVProcedure.Destroy;
var i: integer;
begin
//    for i := 0 to high(fsignature) do fsignature[i].n := '';
//    SetLength(fsignature,0);

    stack.Free;
    body.Free;
    sign.Free;
    rests.Free;
    inherited Destroy;
end;


function TVProcedure.Copy: TValue;
var i: integer;
begin
    //TODO: копирование процедуры ненадёжно может приводить к утечкам
    //self.Complement;

    result := self.ClassType.Create as TValue;

    (result as TVProcedure).body := body.Copy() as TVList;
    (result as TVProcedure).stack := stack.Copy as TVSymbolStack;

    (result as TVProcedure).home_stack := home_stack;

    (result as TVProcedure).evaluated := evaluated;

    (result as TVProcedure).sign := sign.Copy as TVList;

    (result as TVProcedure).stack_pointer := stack_pointer;


    (result as TVProcedure).nN := nN;
    (result as TVProcedure).rests := rests.Copy as TVRecord;
end;

function TVProcedure.AsString: unicodestring;
begin
    if nN<=0
    then result := '#<PROCEDURE '+sign.AsString+'>'
    else result := '#<PROCEDURE '+symbols[nN]+'>';
end;

function TVProcedure.hash: DWORD;
begin
    result := (sign.hash div 2) + (body.hash div 2);
end;

procedure TVProcedure.Complement;
var i: integer;
begin
    if home_stack<>nil
    then try
        for i := 0 to stack.Count-1 do
            if stack.stack[i].V=nil
            then stack.stack[i].V :=
                    home_stack.find_ref_in_frame_or_nil(stack.stack[i].N,
                                                    stack_pointer);

        stack.remove_unbound;
        evaluated:=true;
        home_stack := nil;
    finally
    end;
end;

{ TVGoto }

constructor TVGoto.Create(name: unicodestring);
begin
    n := -1;
    self.uname := UpperCaseU(name);
end;

constructor TVGoto.Create(mark: TVSymbol);
begin
    n := -1;
    self.uname:=mark.uname;
end;

destructor TVGoto.Destroy;
begin
    uname :='';
end;

function TVGoto.Copy: TValue;
begin
    result := TVGoto.Create(uname);
    (result as TVGoto).n := n;
end;

function TVGoto.AsString: unicodestring;
begin
    result := '#<GOTO '+IntTostr(n)+' '+uname+'>';
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
    result := 10001;
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

function TVString.LookItem(index: integer): TValue;
begin
    raise ELE.Create('TVString.LookItem');
    result := nil;
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

class function TVSymbol.symbol_n(n: unicodestring): integer;
var i: integer; uname: unicodestring;
begin
    uname := UpperCaseU(n);
    result := -1;
    EnterCriticalSection(symbols_mutex);
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
    LeaveCriticalSection(symbols_mutex);
end;

class function TVSymbol.symbol_uname(nN: integer): unicodestring;
begin
    EnterCriticalSection(symbols_mutex);
    result := symbols[nN];
    LeaveCriticalSection(symbols_mutex);
end;

constructor TVSymbol.Gensym;
var negN: Int64;
begin
    EnterCriticalSection(symbols_mutex);
    fN := gensym_n;
    Dec(gensym_n);
    LeaveCriticalSection(symbols_mutex);
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
    if false //VL.fL.ref_count=1
    then begin
        VL.fL.OwnsObjects:=false;
        for i := 0 to VL.fL.Count - 1 do fL.Add(VL.fL[i]);
    end
    else
        for i := 0 to VL.fL.Count - 1 do fL.Add((VL.fL[i] as TValue).Copy);

    VL.Free;
end;

//procedure TVList.Add_phantom(V: TValue);
//begin
//    CopyOnWrite;
//    if V is TVList
//    then fl.Add((V as TVList).Phantom_Copy)
//    else fL.Add(V.Copy)
//end;

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
    //writeln(self.asstring);
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
    //WriteLn('count>>');
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

function TVList.Copy: TValue;
var i: integer;
begin
    fL.enter_critical_section;
    Inc(fL.ref_count);
    fL.leave_critical_section;
    result := TVList.Create(fL);
    //(result as TVList).fL.Capacity := fL.Capacity;
    //for i := 0 to fL.Count - 1 do
    //   (result as TVList).fL.Add((fL[i] as TValue).Copy);
end;

//function TVList.Phantom_Copy: TVList;
//var i: integer;
//begin
//  result := TVList.CreatePhantom;
//  result.fL.Capacity := fL.Capacity;
//  for i := 0 to fL.Count - 1 do
//    result.fL.Add(fL[i]);
//end;

function TVList.AsString: unicodestring;
var i: integer;
begin  // try
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

constructor TVList.Create;
begin
  fL := TListBody.Create(true);
end;

constructor TVList.Create(VL: array of TValue);
var i :integer;
begin
    fL := TListBody.Create(true);
    fL.Capacity:=length(VL);
    for i:=0 to length(VL)-1 do fL.Add(VL[i]);
end;

constructor TVList.Create(body: TListBody);
begin
    fL := body;
end;

//constructor TVList.CreatePhantom;
//begin
//    fL := TListBody.create(false);
//end;

destructor TVList.Destroy;
var no_refs: boolean;
begin
    fL.enter_critical_section;
    Dec(fl.ref_count);
    no_refs := fL.ref_count=0;
    fL.leave_critical_section;
    if no_refs then fL.Free;
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

//function TVList.phantom_subseq(istart: integer; iend: integer): TVList;
//var i: integer;
//begin
//    if iend<0 then iend := fL.Count;
//    result := TVList.CreatePhantom;
//    result.fL.Capacity := iend - istart;
//    for i := istart to iend - 1 do
//        result.fL.Add(fL[i]);
//end;

function TVList.CopyOnWrite: boolean;
var i: integer; fL_old: TListBody; no_refs: boolean;
begin
    if fL.ref_count>1 then begin
        fL_old := fL;
        fL := TListBody.Create(true);
        fL.Capacity := fL_old.Count;
        for i := 0 to fL_old.Count-1 do fL.Add((fL_old[i] as TValue).Copy);
        fL_old.enter_critical_section;
        Dec(fL_old.ref_count);
        no_refs := fL_old.ref_count<=0;
        fL_old.leave_critical_section;
        if no_refs then fL_old.Free;
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

//function TVList.Phantom_CDR: TVList;
//var i: integer;
//begin
//    result := TVList.CreatePhantom;
//    result.fL.Capacity:= fL.Count - 1;
//    for i:=1 to fL.Count-1 do result.fL.Add(fL[i]);
//end;
initialization
    InitCriticalSection(symbols_mutex);
    InitCriticalSection(console_mutex);
finalization
    DoneCriticalSection(symbols_mutex);
    DoneCriticalSection(console_mutex);
end.