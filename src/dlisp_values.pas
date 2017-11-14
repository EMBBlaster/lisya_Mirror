unit dlisp_values;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$ASSERTIONS ON}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    SysUtils, Classes, Contnrs, lisia_charset, zstream, mar;


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
        constructor InvalidParameters;
        constructor Create(msg: unicodestring; ec: unicodestring='');
        constructor Malformed(msg: unicodestring);
        constructor Stream(msg: unicodestring);
        destructor Destroy; override;
    end;
    ELE                      = ELisyaError;
    ESymbolNotBound          = class (ELisyaError) end;
    EObjectNotCompound       = class (ELisyaError) end;
    EInvalidParameters       = class (ELisyaError) end;
    EOutOfBounds             = class (ELisyaError) end;

    { TValue }

    TValue = class
        function Copy(): TValue; virtual; abstract;
        function AsString(): unicodestring; virtual; abstract;
    end;
    TValueList = array of TValue;

    { TVEndOfStream }

    TVEndOfStream = class (TValue)
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

    /////  Stack /////////////////////

    TVariable = record
        V: TValue;
        constant: boolean;
    private
        ref_count: integer;
    end;
    PVariable = ^TVariable;


    TStackRecord = record
        name: unicodestring;
        V: PVariable;
    end;


    { TVT }

    TVT = class (TValue)
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;


    { TVNumber }

    TVNumber = class (TValue)
        function F: double; virtual; abstract;
    end;


    { TVInteger }

    TVInteger = class (TVNumber)
        fI: Int64;
        constructor Create(I: Int64);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;

        function F: double; override;
    end;


  { TVRange }

  TVRange = class (TValue)
    low, high: Int64;
    constructor Create(l,h: Int64);
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;
  end;

  { TVFloat }

  TVFloat = class (TVNumber)
    fF: double;
    constructor Create(F: double);
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;

    function F: double; override;
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
    end;

    { TVDateTime }

    TVDateTime = class (TVTime)
        constructor Create(dt: TDateTime);
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;

        function AsSQLDateTime: unicodestring;
    end;




    { TVSymbol }

    TVSymbol = class (TValue)
    private
        fname: unicodestring;
        funame: unicodestring;
    public
        property name: unicodestring read fname;
        property uname: unicodestring read funame;
        constructor Create(S: unicodestring);
        constructor CreateEmpty;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
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
    end;


    { TVBreak }

    TVBreak = class (TVGo)
        function Copy: TVBreak; override;
        function AsString: unicodestring; override;
    end;


    { TVContinue }

    TVContinue = class (TVGo)
        function Copy: TVContinue; override;
        function AsString: unicodestring; override;
    end;


    { TVPrimitive }

    TVPrimitive = class (TValue)
        //класс заглушка, позволяющий функции look вернуть информацию о том,
        //что компонент является примитивным типом
        function Copy: TValue; override;
        function AsString: unicodestring; override;
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
        destructor Destroy;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function constant: boolean;
        function value: TValue;
        function look: TValue;
        procedure add_index(i: integer);
        procedure set_target(_V: TValue);
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
        function append(ss: TVCompoundIndexed): TValue; virtual; abstract;
    end;

    TVCompoundOfPrimitive = class (TVCompoundIndexed)

    end;


    { TVString }

    TVString = class (TVCompoundOfPrimitive)
    private
        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;
      public
        S: unicodestring;
        constructor Create(S: unicodestring);
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;

        function Count: integer; override;

        function subseq(istart: integer; iend: integer = -1): TValue; override;
    end;


    { TVList }

    TVList = class (TVCompoundIndexed)
    private
        fL: TObjectList;
        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;

        function GetElementName(index: integer): unicodestring;
        function GetElementUName(index: integer): unicodestring;
        function GetElementI(index: integer): Int64;
        function GetElementF(index: integer): double;
        function GetElementS(index: integer): unicodestring;
        function GetElementL(index: integer): TVList;
    public
        constructor Create; overload;
        constructor Create(VL: array of TValue); overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;

        function Count: integer; override;

        function subseq(istart: integer; iend: integer = -1): TValue; override;


        procedure Add(V: TValue);
        procedure Append(VL: TVList);

        property name[index: integer]: unicodestring read GetElementName;
        property uname[index: integer]: unicodestring read GetElementUName;
        property I[index: integer]: Int64 read GetElementI;
        property F[index: integer]: double read GetElementF;
        property S[index: integer]: unicodestring read GetElementS;
        property L[index: integer]: TVList read GetElementL;

        procedure SetCapacity(c: integer);

        function POP: TValue;
        procedure Clear;
        function ValueList: TValueList;
        function CdrValueList: TValueList;
        function CAR: TValue;
        function CDR: TVList;
    end;


    { TVByteVector }

    TVByteVector = class (TVCompoundOfPrimitive)
    private
        function GetByte(Index: Integer): Int64;
        procedure SetByte(Index: Integer; V: Int64);

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;

    public
        fBytes: array of byte;

        constructor Create; overload;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;

        property bytes[Index: integer]: Int64 read GetByte write SetByte; default;
        procedure SetCount(l: integer);
        procedure Add(b: Int64);

        function equal_to(BV: TVByteVector): boolean;

        function count: integer; override;
        function subseq(istart: integer; iend: integer = -1): TValue; override;
        procedure append(BV: TVByteVector);
    end;


    { TVRecord }

    TVRecord = class (TVCompound)
    private
        unames: TStringList;
        slots: TObjectList;
        function fGetSlot(index: unicodestring): TValue;
        procedure fSetSlot(index: unicodestring; V: TValue);
        function flook(index: unicodestring): TValue;

        function GetItem(index: integer): TValue; override;
        procedure SetItem(index: integer; _V: TValue); override;
        function LookItem(index: integer): TValue; override;

    public
        constructor Create(names: array of unicodestring); overload;
        constructor Create(names: TStringList); overload;
        constructor Create; overload;
        destructor Destroy; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        property slot[index: unicodestring]:TValue read fGetSlot write fSetSlot;
        property look_name[n: unicodestring]: TValue read flook;
        function is_class(cl: TVRecord): boolean;
        function AddSlot(name: unicodestring; V: TValue): boolean;

        function count: integer; override;
        function name_n(n: integer): unicodestring;

        function get_n_of(index: unicodestring): integer;
    end;


    { TVSymbolStack }

    TVSymbolStack = class (TValue)
    //TODO: требуется упорядочить и защитить от многопоточности работу со стеком
        parent: TVSymbolStack;
        stack: array of TStackRecord;

        function index_of(name: unicodestring): integer;

        constructor Create(parent: TVSymbolStack);
        destructor Destroy; override;
        procedure Print;  overload;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function Count: integer; //override;
        function set_n(n: integer; V: TValue): boolean; //override;
        function get_n(n: integer): TValue; //override;

        procedure Print(n: integer); overload;

        procedure new_var(name: unicodestring; V: TValue; c: boolean = false);
        function find_var(name: unicodestring): TValue;
        procedure set_var(name: unicodestring; V: TValue);
        procedure clear_frame(n: integer = -1);
        procedure bind_var(name, target: unicodestring);
        function find_ref(name: unicodestring): PVariable;
        function find_ref_in_frame(name: unicodestring; n: integer): PVariable;
        procedure new_ref(name: unicodestring; P: PVariable);

        function find_ref_or_nil(name: unicodestring): PVariable;
        function find_ref_in_frame_or_nil(name: unicodestring; n: integer): PVariable;
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
        name: unicodestring;
        //TODO: нужно пересмотреть дерево классов подпрограмм
        //поле signature используется только внутренними функциями
        //TVProcedure использует поле fsignature
        //поле stack не используется операторами и внутренними функциями
    end;


    { TVProcedure }

    TVProcedure = class (TVSubprogram)
        is_macro: boolean;
        evaluated: boolean;
        fsignature: TSubprogramSignature;
        stack_pointer: integer;
        stack: TVSymbolStack;
        body: TVList;
        constructor Create;
        constructor CreateEmpty;
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

    TVMacro = class (TVProcedure)
        //макрос является обычной функцией за тем исключением, что
        //возвращаемый им результат выполняется в месте вызова
        //данный клас нужен, только для того, чтобы eval мог отличить макрос
        //от процедуры

        //неправда это всё надо переделать
    end;

    type TEvalProc = function (V: TValue): TValue of Object;
    type TInternalFunctionBody = function (const PL: TVList;
                                            ep: TEvalProc = nil): TValue;

    { TVInternalSubprogram }

    TVInternalSubprogram = class (TVSubprogram)
        signature: TVList;
        constructor Create;
        destructor Destroy; override;
    end;

    { TVInternalFunction }

    TVInternalFunction = class (TVInternalSubprogram)
    //TODO: нужно унифицировать интерфейсы операторов и внутренних функций,
    // а то они используют разные способы хранения сигнатур
    // операторы по ссылке, а внутренние функции по значению
    //при этом используют одно итоже поле signature родительского класса
    //возможно операторам вообще не нужна сигнатура, поскольку большинством
    //операторов она не используется при вызове (только как справка)

        body: TInternalFunctionBody;
        constructor Create(sign: TVList;
                            body: TInternalFunctionBody;
                            name: unicodestring = '');
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

    { TVOperator }

    type TOperatorEnum = (
            oeAND,
            oeAPPEND,
            oeBLOCK,
            oeBREAK,
            oeCASE,
            oeCOND,
            oeCONST,
            oeCONTINUE,
            oeDEBUG,
            oeDEFAULT,
            oeELT,
            oeFILTER,
            oeFOR,
            oeGOTO,
            oeIF,
            oeIF_NIL,
            oeLAST,
            oeLET,
            oeMACRO,
            oeMAP,
            oeOR,
            oePACKAGE,
            oePOP,
            oePROCEDURE,
            oePUSH,
            oeQUOTE,
            oeSET,
            oeSTRUCTURE,
            oeSTRUCTURE_AS,
            oeVAL,
            oeVAR,
            oeWHEN,
            oeWHILE);
    //and append block break case cond const continue default elt exception filter for goto if if-nil last let map or pop procedure push quote set stack structure structure-as val var when while

    TVOperator = class (TVInternalSubprogram)
        name: unicodestring;
        op_enum: TOperatorEnum;
        param_is_check: boolean;

        constructor Create(name: unicodestring; en: TOperatorEnum; sign: TVList);
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;


    TFileMode = (fmRead, fmWrite, fmAppend);
    TStreamEncoding = (seBOM, seUTF8, seCP1251, seCP1252, seUTF16BE, seUTF16LE, seUTF32BE,
                        seUTF32LE);

    { TVStreamBody }
    TCompressionMode = (cmNone, cmDeflate);
    TStreamDirection = (sdIn, sdOut);

    TVStreamBody = class (TValue)
        fstream: TStream;
        fZstream: TStream;
        stream_type: (stFile);
        stream_direction: TStreamDirection;
        file_name: unicodestring;
        encoding: TStreamEncoding;
        constructor Create(fn: unicodestring; mode: TFileMode);
        destructor Destroy; override;

        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function read_byte(var b: byte): boolean;
        function write_byte(b: byte): boolean;
    end;

    { TVStreamPointer }

    TVStreamPointer = class (TValue)
        body: PVariable;
        //encoding: TStreamEncoding;
        constructor Create(fn: unicodestring;
                            mode: TFileMode;
                            _encoding: TStreamEncoding); overload;
        constructor Create; overload;
        destructor Destroy; override;

        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function read_byte(var b: byte): boolean;
        function write_byte(b: byte): boolean;
        function read_char(var ch: unicodechar): boolean;
        function write_char(ch: unicodechar): boolean;
        function write_BOM: boolean;
        procedure close_stream;

        function Set_compression_mode(mode: TCompressionMode): boolean;
        function set_position(p: Int64): boolean;
        function get_position(var p: Int64): boolean;
        function stream_length: Int64;
    end;



procedure Assign(var v1, v2: TValue);

function op_null(V: TValue): boolean;

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
function RefVariable(P: PVariable): PVariable;
procedure ReleaseVariable(var P: PVariable);

implementation


    { TVariable }

function NewVariable(_V: TValue = nil; _constant: boolean = false): PVariable;
begin
    New(result);
    result.ref_count:=1;
    result.constant := _constant;
    result.V := _V;

   // WriteLn('NewVariable>> ', IntToHex(qword(result),8));
end;

function RefVariable(P: PVariable): PVariable;
begin

   // WriteLn('RefVariable>> ', IntToHex(qword(p),8));
    if P<> nil then begin
        Inc(P.ref_count);
        result := P;
    end
    else
        result := nil;

end;

procedure ReleaseVariable(var P: PVariable);
begin
    //WriteLn('ReleaseVariable>> ', IntToHex(qword(p),8));
    if P<>nil then begin
      //  WriteLn('ref_count = ',P.ref_count);
        Dec(P.ref_count);
        if P.ref_count<=0 then begin
            P.V.Free;
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


{ TVCompoundIndexed }

function TVCompoundIndexed.high: integer;
begin
    result := self.Count-1;
end;

{ TVPrimitive }

function TVPrimitive.Copy: TValue;
begin
    raise ELE.Create('primitive copy');
end;

function TVPrimitive.AsString: unicodestring;
begin
    result := '#<PRIMITIVE>'
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
    DecodeTime(fDT, hour, minute, second, ms);
    result := IntToStr(round(fDT))+':'
        +dd(hour)+':'+dd(minute)+':'+dd(second)+'.'+ddd(ms);
    if fDT>=0
    then result := '#<'+result+'>'
    else result := '#< - '+result+'>'
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
var year,month,day,hour,minute,second, ms: WORD;
    function dd(i: integer): unicodestring;
    begin
        result := IntToStr(i);
        if Length(result)=1 then result := '0'+result;
    end;
begin
    //DecodeDate(fDT, year, month, day);
    //DecodeTime(fDT, hour, minute, second, ms);
    //result := '#<'+
    //    IntToStr(Year)+'-'+dd(month)+'-'+dd(day)+
        //' '+dd(hour)+':'+dd(minute)+':'+dd(second)+'>';
    result := AsSQLDateTime;
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

function TVByteVector.count: integer;
begin
    result := Length(fBytes);
end;

{ TVInternalSubprogram }

constructor TVInternalSubprogram.Create;
begin
    name := '';
    signature := nil;
end;

destructor TVInternalSubprogram.Destroy;
begin
    signature.Free;
    inherited Destroy;
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

constructor ELisyaError.Create(msg: unicodestring; ec: unicodestring);
begin
    inherited Create(msg);
    EClass := ec;
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

constructor TVSymbolStack.Create(parent: TVSymbolStack);
begin
    self.parent := parent;
    SetLength(stack,0);
end;

destructor TVSymbolStack.Destroy;
var i: integer;
begin
    for i := 0 to high(stack) do ReleaseVariable(stack[i].V);
    inherited Destroy;
end;

procedure TVSymbolStack.Print;
var i: integer;
begin
    WriteLn('-----------------------');
    for i := 0 to high(stack) do begin
        Write('  | ', stack[i].name);
        if stack[i].V<> nil
        then WriteLn('  >(', stack[i].V.ref_count,')',
                        IntToHex(Cardinal(stack[i].V),9),'>  ', stack[i].V.V.AsString)
        else WriteLn(' nil');
    end;
    WriteLn('-----------------------');
end;

function TVSymbolStack.Copy: TValue;
var i: integer;
begin
   // print;
    result := TVSymbolStack.Create(nil);
    for i := 0 to high(stack) do begin
        (result as TVSymbolStack).new_ref(stack[i].name,
                                            RefVariable(stack[i].V));
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
    then raise EOutOfBounds.Create('stack '+IntToStr(n));

    if stack[n].V.constant
    then raise ELisyaError.Create('stack element '+IntToStr(n)+' is constant');
    stack[n].V.V.Free;
    stack[n].V.V := V;
    result := true;
end;

function TVSymbolStack.get_n(n: integer): TValue;
begin
    if (n<0) or (n>high(stack))
    then raise EOutOfBounds.Create('stack '+IntToStr(n));

    result := stack[n].V.V.Copy;
end;

procedure TVSymbolStack.Print(n: integer);
var i: integer;
begin
    WriteLn('-----------------------');
    for i := n to high(stack) do begin
        Write('  | ', stack[i].name);
        if stack[i].V<> nil
        then WriteLn('  >(', stack[i].V.ref_count,')',
                        Cardinal(stack[i].V),'>  ', stack[i].V.V.AsString)
        else WriteLn(' nil');
    end;
    WriteLn('-----------------------');

end;

procedure TVSymbolStack.new_var(name: unicodestring; V: TValue; c: boolean);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].name := UpperCaseU(name);
    stack[high(stack)].V := NewVariable;
    stack[high(stack)].V.V := V;
    stack[high(stack)].V.constant := c;
end;

function TVSymbolStack.find_var(name: unicodestring): TValue;
begin
    //WriteLn(name);
    //print(78);
    result := stack[index_of(name)].V.V.Copy;
end;

procedure TVSymbolStack.set_var(name: unicodestring; V: TValue);
begin
    set_n(index_of(name), V);
end;

procedure TVSymbolStack.clear_frame(n: integer);
var i, _n: integer;
begin
    if n<0
    then begin
        for i := high(stack) downto 0 do
        if stack[i].name[1]=' ' then begin
            _n := i;
            break;
        end;
    end
    else _n := n;

    for i := high(stack) downto _n do ReleaseVariable(stack[i].V);
    SetLength(stack, _n);
end;

procedure TVSymbolStack.bind_var(name, target: unicodestring);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].V := RefVariable(stack[index_of(target)].V);
    stack[high(stack)].name := UpperCaseU(name);
end;

function TVSymbolStack.find_ref(name: unicodestring): PVariable;
begin
    result := RefVariable(stack[index_of(name)].V);
end;

function TVSymbolStack.find_ref_in_frame(name: unicodestring; n: integer
    ): PVariable;
var i: integer; uname: unicodestring;
begin
    uname := UpperCaseU(name);
    //print(78);
    for i := n to high(stack) do begin
        if stack[i].name = uname then begin
            result := RefVariable(stack[i].V);
            exit;
        end;
        if stack[i].name[1]=' '
        then raise ESymbolNotBound.Create('recursive '+name);
    end;
    raise ESymbolNotBound.Create('recursive '+name);
end;

procedure TVSymbolStack.new_ref(name: unicodestring; P: PVariable);
begin
    SetLength(stack, Length(stack)+1);
    stack[high(stack)].name := name;
    stack[high(stack)].V := P;
end;

function TVSymbolStack.find_ref_or_nil(name: unicodestring): PVariable;
var i: integer;
begin
    result := nil;
    for i := high(stack) downto 0 do
        if stack[i].name=name then begin
            result := RefVariable(stack[i].V);
            exit;
        end;
end;

function TVSymbolStack.find_ref_in_frame_or_nil(name: unicodestring; n: integer
    ): PVariable;
var i: integer; uname: unicodestring;
begin
    uname := UpperCaseU(name);
    for i := n to high(stack) do begin
        if stack[i].name = uname then begin
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


{ TVRecord }

function TVRecord.fGetSlot(index: unicodestring): TValue;
begin
    result := (slots[unames.IndexOf(index)] as TValue).copy;
end;

procedure TVRecord.fSetSlot(index: unicodestring; V: TValue);
begin
    slots[unames.IndexOf(index)] := V;
end;

function TVRecord.flook(index: unicodestring): TValue;
begin
    //TODO: обращение к несуществующему слоту структуры вызывает необрабатываемую ошибку
    result := slots[unames.IndexOf(index)] as TValue;
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

constructor TVRecord.Create(names: array of unicodestring);
var i: integer;
begin
    unames := TStringList.Create;
    unames.Capacity := Length(names);
    slots := TObjectList.create(true);
    slots.Capacity := Length(names);
    for i := low(names) to high(names) do begin
        unames.Add(UpperCaseU(names[i]));
        slots.Add(TVList.Create);
    end;
    //unames.Sort;
end;

constructor TVRecord.Create(names: TStringList);
var i: integer;
begin
    unames := names;
    slots := TObjectList.create(true);
    slots.Capacity := names.Count;
    for i := 0 to unames.Count-1 do begin
        unames[i] := UpperCaseU(unames[i]);
        slots.Add(TVList.Create);
    end;
    //unames.Sort;
end;

constructor TVRecord.Create;
begin
    unames := TStringList.Create;
    slots := TObjectList.Create(true);
end;

destructor TVRecord.Destroy;
begin
    unames.Free;
    slots.Free;
    inherited Destroy;
end;

function TVRecord.Copy: TValue;
var i: integer;
begin
    //WriteLn('>>', self.AsString);
    result := TVRecord.Create;
    (result as TVRecord).unames.capacity := unames.count;
    (result as TVRecord).slots.capacity := unames.count;
    for i := 0 to unames.Count - 1 do begin
        (result as TVRecord).unames.Add(unames[i]);
        (result as TVRecord).slots.Add((slots[i] as TValue).copy);
    end;
end;

function TVRecord.AsString: unicodestring;
var i: integer;
begin
    result := '#S(';
    if slots.Count>0
    then result := result + unames[0] + ' ' + (slots[0] as TValue).AsString();
    for i := 1 to slots.Count - 1 do
    result := result + ' ' + unames[i] + ' ' + (slots[i] as TValue).AsString();
    result := result + ')';
end;

function TVRecord.is_class(cl: TVRecord): boolean;
var i: integer;
begin
    result := false;
    for i := 0 to cl.unames.Count-1 do
        if unames.IndexOf(cl.unames[i])<0 then exit;
    result := true;
end;

function TVRecord.AddSlot(name: unicodestring; V: TValue): boolean;
begin
    unames.Add(UpperCaseU(name));
    slots.Add(V);
end;

function TVRecord.count: integer;
begin
    result := unames.Count;
end;

function TVRecord.name_n(n: integer): unicodestring;
begin
    result := unames[n];
end;

function TVRecord.get_n_of(index: unicodestring): integer;
begin
    result := unames.IndexOf(index);

end;


{ TVStreamPointer }

constructor TVStreamPointer.Create(fn: unicodestring;
                                    mode: TFileMode;
                                    _encoding: TStreamEncoding);
    function b: byte;
    begin
        result := (body.v as TVStreamBody).fstream.ReadByte;
    end;
begin
    body := NewVariable;
    body.V := TVStreamBody.Create(fn, mode);
    (body.V as TVStreamBody).encoding :=_encoding;

    if _encoding=seBOM
    then begin
        //UTF-8
        if (b=$EF) and (b=$BB) and (b=$BF) then begin
            (body.V as TVStreamBody).encoding := seUTF8;
            exit;
        end else (body.v as TVStreamBody).fstream.Seek(0,0);
        //UTF16BE
        if (b=$FE) and (b=$FF) then begin
            (body.V as TVStreamBody).encoding := seUTF16BE;
            exit;
        end else (body.v as TVStreamBody).fstream.Seek(0,0);
        //UTF32BE
        if (b=$00) and (b=$00) and (b=$FE) and (b=$FF) then begin
            (body.V as TVStreamBody).encoding := seUTF32BE;
            exit;
        end else (body.v as TVStreamBody).fstream.Seek(0,0);
        //UTF32LE
        if (b=$FF) and (b=$FE) and (b=$00) and (b=$00) then begin
            (body.V as TVStreamBody).encoding := seUTF32LE;
            exit;
        end else (body.v as TVStreamBody).fstream.Seek(0,0);
        //UTF16LE
        if (b=$FF) and (b=$FE) then begin
            (body.V as TVStreamBody).encoding := seUTF16LE;
            exit;
        end else (body.v as TVStreamBody).fstream.Seek(0,0);

        (body.V as TVStreamBody).encoding := seCP1251;
    end;
end;

constructor TVStreamPointer.Create;
begin
    //
end;

destructor TVStreamPointer.Destroy;
begin
    ReleaseVariable(body);
    inherited Destroy;
end;


function TVStreamPointer.Copy: TValue;
begin
    result := TVStreamPointer.Create;
    (result as TVStreamPointer).body := RefVariable(body);
end;

function TVStreamPointer.AsString: unicodestring;
begin
    if body.V<>nil
    then result := '#<STREAM POINTER '+body.V.AsString()+' >'
    else result := '#<STREAM POINTER closed>';
end;

function TVStreamPointer.read_byte(var b: byte): boolean;
begin
try
    if body.V<>nil
    then result := (body.v as TVStreamBody).read_byte(b)
    else result := false;
except
    on EStreamError do result := false;
end;
end;

function TVStreamPointer.write_byte(b: byte): boolean;
begin
try
    if body.V<>nil
    then result := (body.v as TVStreamBody).write_byte(b)
    else result := false;
except
    on EStreamError do result := false;
end;
end;

function TVStreamPointer.read_char(var ch: unicodechar): boolean;
var b1, b2, b3, b4: byte;
    function read8bit(const enc: TCodePage): boolean;
    begin
        result := read_byte(b1);
        ch := enc[b1];
    end;
begin
    case (body.V as TVStreamBody).encoding of
        seUTF8: begin
            //TODO: read_char кошмарик
            result := read_byte(b1);
            if result and ((b1 shr 6)=3) then result := read_byte(b2);
            if result and ((b1 shr 5)=7) then result := read_byte(b3);
            if result and ((b1 shr 4)=15) then result := read_byte(b4);

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
        else begin result := read_byte(b1); ch := unicodechar(b1); end;
    end
end;

function TVStreamPointer.write_char(ch: unicodechar): boolean;
var b1, b2, b3, b4: byte; cp: integer;
begin
    case (body.V as TVStreamBody).encoding of
        seUTF8: begin
            cp := ord(ch);
            case cp of
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
        end;
        else result := false;
    end
end;

function TVStreamPointer.write_BOM: boolean;
    function wb(b: byte): boolean; begin result := write_byte(b); end;
begin
    case (body.V as TVStreamBody).encoding of
        seUTF8:    result := wb($EF) and wb($BB) and wb($BF);
        seUTF16LE: result := wb($FF) and wb($FE);
        seUTF16BE: result := wb($FE) and wb($FF);
        seUTF32LE: result := wb($FF) and wb($FE) and wb($00) and wb($00);
        seUTF32BE: result := wb($00) and wb($00) and wb($FE) and wb($FF);
        else result := true;
    end
end;

procedure TVStreamPointer.close_stream;
begin
    FreeAndNil(body.V);
end;

function TVStreamPointer.Set_compression_mode(mode: TCompressionMode): boolean;
begin
    if body.V=nil then begin result := false; exit; end;
    with (body.V as TVStreamBody) do begin
        FreeAndNil(fZstream);
        case stream_direction of
            sdIn: case mode of
                cmDeflate: fZstream := TDecompressionStream.create(
                                            fstream, true);
            end;
            sdOut: case mode of
                cmDeflate: fZstream := TCompressionStream.create(clDefault,
                                            fstream, true);
            end;
        end;
    end;
    result := true;
end;

function TVStreamPointer.set_position(p: Int64): boolean;
begin
    if body.V=nil then begin result := false; exit; end;
    with (body.V as TVStreamBody) do begin
        FreeAndNil(fZstream);
        fStream.Position := p;
    end;
    result := true;
end;

function TVStreamPointer.get_position(var p: Int64): boolean;
begin
    if body.V=nil then begin result := false; exit; end;
    with (body.V as TVStreamBody) do begin
        p := fStream.Position;
    end;
    result := true;
end;

function TVStreamPointer.stream_length: Int64;
begin
    (body.V as TVStreamBody).fstream.Size;
end;

{ TVStreamBody }

constructor TVStreamBody.Create(fn: unicodestring; mode: TFileMode);
begin
    stream_type := stFile;

    file_name := fn;
    case mode of
        fmRead: begin
            fStream := TFileStream.Create(fn, fmOpenRead);
            stream_direction := sdIn;
        end;
        fmWrite: begin
            fStream := TFileStream.Create(fn, fmCreate);
            stream_direction := sdOut;
        end;
        fmAppend: begin
            if FileExists(fn)
            then fStream := TFileStream.Create(fn, fmOpenReadWrite)
            else fStream := TFileStream.Create(fn, fmCreate);
            fStream.Seek(fStream.Size,0);
            stream_direction := sdOut;
        end;
    end;
    fZstream := nil;
end;

destructor TVStreamBody.Destroy;
begin
    FreeAndNil(fZstream);
    FreeAndNil(fStream);

    file_name := '';
    inherited Destroy;
end;

function TVStreamBody.Copy: TValue;
begin
    result := nil;
    raise Exception.Create('копирование потока');
end;

function TVStreamBody.AsString: unicodestring;
begin
    result := '#<STREAM '+file_name+' >';
end;

function TVStreamBody.read_byte(var b: byte): boolean;
begin
try
    result := true;
    if fZstream<>nil
    then b := fZstream.ReadByte
    else b := fstream.ReadByte;
except
    on EStreamError do result := false;
end;
end;

function TVStreamBody.write_byte(b: byte): boolean;
begin
try
    result := true;
    if fZstream<>nil
    then fZstream.Write(b,1)
    else fstream.Write(b,1);
except
    on EStreamError do result := false;
end;
end;

{ TVOperator }

constructor TVOperator.Create(name: unicodestring; en: TOperatorEnum; sign: TVList);
begin
    self.name := UpperCaseU(name);
    self.op_enum := en;
    self.signature := sign;
end;

destructor TVOperator.Destroy;
begin
    //унаследованный деструктор не должен вызываться поскольку он
    //освобождает сигнатуру, но все экземпляры оператора используют общий
    //экземпляр сигнатуры, хранимый в массиве ops

    //inherited Destroy;
end;

function TVOperator.Copy: TValue;
begin
    //сигнатура копируется по ссылке, поскольку операторы не могут быть
    //изменены или полностью уничтожены
    result := TVOperator.Create(name, op_enum, signature);
end;

function TVOperator.AsString: unicodestring;
begin
    result := '#<OPERATOR '+name+' '+signature.AsString()+'>';
end;

{ TVInternalFunction }

constructor TVInternalFunction.Create(sign: TVList;
                                    body: TInternalFunctionBody;
                                    name: unicodestring = '');
begin
    inherited Create;
    signature := sign;
    self.body := body;
    self.name := name;
end;

destructor TVInternalFunction.Destroy;
begin
    inherited Destroy;
end;


function TVInternalFunction.Copy: TValue;
begin
    result := TVInternalFunction.Create(signature.Copy as TVList, body, name);
end;

function TVInternalFunction.AsString: unicodestring;
begin
    result := '#<INTERNAL '+name+' '+signature.AsString()+'>';
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

{ TVBreak }

function TVBreak.Copy: TVBreak;
begin
    result := TVBreak.Create;
end;

function TVBreak.AsString: unicodestring;
begin
    result := '#<BREAK>';
end;


{ TVProcedure }

var pc: integer=0;

constructor TVProcedure.Create;
begin
    inherited;
    body:= TVList.Create;
    stack := TVSymbolStack.Create(nil);

    fsignature := nil;
    evaluated := false;
    stack_pointer := -1;
    is_macro := false;
end;

constructor TVProcedure.CreateEmpty;
begin
    inherited;
    body := nil;
    stack := nil;

    fsignature := nil;
    evaluated := false;
    stack_pointer := -1;
    is_macro := false;
end;

destructor TVProcedure.Destroy;
var i: integer;
begin
    for i := 0 to high(fsignature) do fsignature[i].n := '';
    SetLength(fsignature,0);

    stack.Free;
    body.Free;
    inherited Destroy;
end;


function TVProcedure.Copy: TValue;
var i: integer;
begin
    //TODO: копирование процедуры ненадёжно может приводить к утечкам
    result := TVProcedure.CreateEmpty;
    (result as TVProcedure).body := body.Copy() as TVList;
    (result as TVProcedure).stack := stack.Copy as TVSymbolStack;

    (result as TVProcedure).evaluated := evaluated;
    setLength((result as TVProcedure).fsignature, Length(fsignature));
    for i := 0 to high(fsignature) do
        (result as TVProcedure).fsignature[i] := fsignature[i];

    (result as TVProcedure).stack_pointer := stack_pointer;

    (result as tVProcedure).is_macro:=is_macro;
end;

function TVProcedure.AsString: unicodestring;
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
//    pl := parameters_list.AsString;
    PL := '';
    for i := 0 to Length(fsignature)-1 do
        PL := PL+' '+fsignature[i].n+m(fsignature[i].m);
    if name=''
    then result := '#<PROCEDURE'+pl+'>'
    else result := '#<PROCEDURE '+name+'>';
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
    mark.Free;
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


{ TVT }

function TVT.Copy: TValue;
begin
    result := TVT.Create;
end;

function TVT.AsString: unicodestring;
begin
    result := 'T';
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

function TVFloat.F: double;
begin
    result := fF;
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

function TVString.Count: integer;
begin
    result := Length(S);
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

function TVInteger.F: double;
begin
    result := fI;
end;

constructor TVInteger.Create(I: Int64); begin fI := I; end;


  { TVSymbol }

function TVSymbol.Copy: TValue;
begin
    result := TVSymbol.CreateEmpty;
    (result as TVSymbol).fname := self.fname;
    (result as TVSymbol).funame := self.funame;
end;

function TVSymbol.AsString: unicodestring;
begin
    result := fname;
end;

constructor TVSymbol.CreateEmpty;
begin

end;

constructor TVSymbol.Create(S: unicodestring);
begin
    fname := S;
    funame := UpperCaseU(S);
end;

destructor TVSymbol.Destroy;
begin
    inherited;
end;

{ TVList }

procedure TVList.Add(V: TValue);
begin
  fL.Add(V);
end;

procedure TVList.Append(VL: TVList);
var
  i: integer;
begin
  fL.Capacity := fL.Capacity + VL.fL.Capacity;
  for i := 0 to VL.fL.Count - 1 do begin
    fL.Add((VL.fL[i] as TValue).Copy);
  end;
  VL.Free;
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
    Assert(fL[index] is TVNumber, 'Элемент '+IntToStr(index)+' не число');
    result := (fL[index] as tvNumber).F;
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

function TVList.Count: integer;
begin
    result := fL.Count;
end;

procedure TVList.SetCapacity(c: integer);
begin
    self.fL.Capacity:=c;
end;

function TVList.Copy: TValue;
var i: integer;
begin
  result := TVList.Create;
  (result as TVList).fL.Capacity := fL.Capacity;
  for i := 0 to fL.Count - 1 do
    (result as TVList).fL.Add((fL[i] as TValue).Copy);
end;

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

var lc: integer = 0;

constructor TVList.Create;
begin
  fL := TObjectList.Create(true);
end;

constructor TVList.Create(VL: array of TValue);
var i :integer;
begin
    fL := TObjectList.Create(true);
    fL.Capacity:=length(VL);
    for i:=0 to length(VL)-1 do fL.Add(VL[i]);
end;

destructor TVList.Destroy;
begin
  fL.Free;
  inherited;
end;

function TVList.GetItem(index: integer): TValue;
begin
    result := (fL[Index] as TValue).Copy;
end;

procedure TVList.SetItem(index: integer; _V: TValue);
begin
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

function TVList.POP: TValue;
begin
    result := (fL.Last as TValue).Copy();
    fL.Delete(fL.Count-1);
end;

procedure TVList.Clear;
begin
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

end.