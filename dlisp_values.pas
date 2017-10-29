﻿unit dlisp_values;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$ASSERTIONS ON}

interface

uses
  cwstring,
  SysUtils, Classes, Contnrs, zlib, lisia_charset, zstream, mar;


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
        destructor Destroy; override;
    end;
    ELE                      = ELisyaError;
    ESymbolNotBound          = class (ELisyaError) end;
    EObjectNotCompound       = class (ELisyaError) end;
    EInvalidParameters       = class (ELisyaError) end;
    EOutOfBounds             = class (ELisyaError) end;

    { TValue }

    TValue = class
        procedure Print; virtual; abstract;
        function Copy(): TValue; virtual; abstract;
        function AsString(): unicodestring; virtual; abstract;
    end;
    TValueList = array of TValue;

    { TVEndOfStream }

    TVEndOfStream = class (TValue)
        procedure Print; override;
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

    { TSymbolStack }

    TSymbolStack = class
        //TODO: требуется упорядочить и защитить от многопоточности работу со стеком
        stack: array of TStackRecord;

        constructor Create(parent: TSymbolStack);
        destructor Destroy; override;

        procedure new_var(name: unicodestring; V: TValue; c: boolean = false);
        function find_var(name: unicodestring; var V: TValue): boolean;
        function set_var(name: unicodestring; V: TValue): boolean;
        function bind_var(name, target: unicodestring): boolean;
        function find_ref(name: unicodestring; var P: PVariable): boolean;
        function find_ref_in_frame
            (name: unicodestring; n: integer; var P: PVariable): boolean;
        procedure new_ref(name: unicodestring; P: PVariable);
        function find_unbound(var name: unicodestring): boolean;
        procedure print_stack;
        procedure clear_frame;
        function count: integer;
    end;



  { TVT }

  TVT = class (TValue)
    procedure Print; override;
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
    procedure Print; override;
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;

    function F: double; override;
  end;

  { TVRange }

  TVRange = class (TValue)
    low, high: Int64;
    constructor Create(l,h: Int64);
    procedure Print; override;
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;
  end;

  { TVFloat }

  TVFloat = class (TVNumber)
    fF: double;
    constructor Create(F: double);
    procedure Print; override;
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;

    function F: double; override;
  end;


    { TVTime }

    TVTime = class (TValue)
        fDT: TDateTime;
        procedure Print; override;
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
    end;




  { TVSymbol }

  TVSymbol = class (TValue)
    private
      fname: unicodestring;
    public
      property name: unicodestring read fname;
      function uname: unicodestring;
      constructor Create(S: unicodestring);
      destructor Destroy; override;
      procedure Print; override;
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
    procedure Print; override;
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;
  end;

  { TVBreak }

  TVBreak = class (TVGo)
    procedure Print; override;
    function Copy: TVBreak; override;
    function AsString: unicodestring; override;
  end;

  { TVContinue }

  TVContinue = class (TVGo)
    procedure Print; override;
    function Copy: TVContinue; override;
    function AsString: unicodestring; override;
  end;


  TVCompound = class (TValue)
    function Count: integer; virtual; abstract;
    function set_n(n: integer; V: TValue): boolean; virtual; abstract;
    function get_n(n: integer): TValue; virtual; abstract;
    function look_n(n: integer): TValue; virtual; abstract;
  end;

  TVCompoundOfPrimitive = class (TVCompound)

  end;

  { TVPointer }

  TVPointer = class (TValue)
    compound: TVCompound;
    index: integer;

    constructor Create(c: TVCompound; i: integer);
    procedure Print; override;
    function Copy: TValue; override;
    function AsString: unicodestring; override;

    function look: TValue;
    function value: TValue;
  end;

  { TVString }

  TVString = class (TVCompoundOfPrimitive)
    S: unicodestring;
    constructor Create(S: unicodestring);
    destructor Destroy; override;
    procedure Print; override;
    function Copy(): TValue; override;
    function AsString(): unicodestring; override;

    function Count: integer; override;
    function set_n(n: integer; V: TValue): boolean; override;
    function get_n(n: integer): TValue; override;
    function look_n(n: integer): TValue; override;
  end;

  { TVList }

  TVList = class (TVCompound)
    private
      fL: TObjectList;
      function GetItems(Index: Integer): TValue;
      procedure SetItems(Index: Integer; V: TValue);
      function flook(index: integer): TValue;
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
      procedure Print; override;
      function Copy(): TValue; override;
      function AsString(): unicodestring; override;

      procedure Add(V: TValue);
      procedure Append(VL: TVList);
      property Items[Index: Integer]: TValue read GetItems write SetItems; default;
      property look[index: integer]: TValue read flook;
      property name[index: integer]: unicodestring read GetElementName;
      property uname[index: integer]: unicodestring read GetElementUName;
      property I[index: integer]: Int64 read GetElementI;
      property F[index: integer]: double read GetElementF;
      property S[index: integer]: unicodestring read GetElementS;
      property L[index: integer]: TVList read GetElementL;
      function Count: integer; override;
      function High: integer;
      procedure SetCapacity(c: integer);
      function Subseq(istart, iend: integer): TVList;
      function POP: TValue;
      procedure Clear;
      function ValueList: TValueList;
      function CdrValueList: TValueList;
      function CAR: TValue;
      function CDR: TVList;

      function set_n(n: integer; V: TValue): boolean; override;
      function get_n(n: integer): TValue; override;
      function look_n(n: integer): TValue; override;
  end;


    { TVByteVector }

    TVByteVector = class (TVCompoundOfPrimitive)
        private
            fBytes: array of byte;
            function GetByte(Index: Integer): Int64;
            procedure SetByte(Index: Integer; V: Int64);
        public
            constructor Create; overload;
            destructor Destroy; override;
            procedure Print; override;
            function Copy(): TValue; override;
            function AsString(): unicodestring; override;

            property bytes[Index: integer]: Int64 read GetByte write SetByte; default;
            procedure SetCount(l: integer);
            procedure Add(b: Int64);
            function SubSeq(istart, iend: integer): TVByteVector;
            function High: integer;
            function equal_to(BV: TVByteVector): boolean;

            function Count: integer; override;
            function set_n(n: integer; V: TValue): boolean; override;
            function get_n(n: integer): TValue; override;
            function look_n(n: integer): TValue; override;
    end;


  { TVClass }

  TVClass = class (TValue)
    uname: unicodestring;
    unames: TStringList;
    constructor Create(name: unicodestring; names: array of unicodestring); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Print; override;
    function Copy: TValue; override;
    function AsString: unicodestring; override;
  end;


  { TVStructure }

  TVStructure = class (TVCompound)
    private
        unames: TStringList;
        slots: TObjectList;
        function fGetSlot(index: unicodestring): TValue;
        procedure fSetSlot(index: unicodestring; V: TValue);
        function flook(index: unicodestring): TValue;
    public
        constructor Create(names: array of unicodestring); overload;
        constructor Create(names: TStringList); overload;
        constructor Create; overload;
        destructor Destroy; override;
        procedure Print; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        property slot[index: unicodestring]:TValue read fGetSlot write fSetSlot; default;
        property look[n: unicodestring]: TValue read flook;
        function is_class(cl: TVStructure): boolean;
        function SetSlot(index: unicodestring; V: TValue): boolean;
        function GetSlot(index: unicodestring; var V: TValue): boolean;
        function AddSlot(name: unicodestring; V: TValue): boolean;

        function count: integer; override;
        function name_n(n: integer): unicodestring;

        function set_n(n: integer; V: TValue): boolean; override;
        function get_n(n: integer): TValue; override;
        function look_n(n: integer): TValue; override;
        function get_n_of(index: unicodestring): integer;
  end;

    { TVSymbolStack }

    TVSymbolStack = class (TVCompound)
    //TODO: требуется упорядочить и защитить от многопоточности работу со стеком
        parent: TVSymbolStack;
        stack: array of TStackRecord;

        function index_of(name: unicodestring): integer;

        constructor Create(parent: TVSymbolStack);
        destructor Destroy; override;
        procedure Print; override; overload;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function Count: integer; override;
        function set_n(n: integer; V: TValue): boolean; override;
        function get_n(n: integer): TValue; override;

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


type
    TErrorClass = (ecOk, ecError, ecEoS, ecSyntax, ecConversion,
        ecSymbolNotBound, ecInvalidParameters, ecMalformed,
        ecOutOfBounds, ecAssertion, ecFileNotFound, ecStreamError,
        ecXML, ecObjectNotCompound);
    const ErrorClassName: array[TErrorClass] of unicodestring = (
        'Ok',
        'Error',
        'end of stream',
        'Syntax',
        'Conversion',
        'symbol not bound',
        'invalid parameters',
        'malformed',
        'out of bounds',
        'assertion',
        'file not found',
        'stream error',
        'malformed XML',
        'object not compound');

type

    TVError = class (TValue)
        e: TErrorClass;
        msg: unicodestring;
        constructor Create(e: TErrorClass; msg: unicodestring);
        destructor Destroy; override;
        procedure Print; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

    TSubprogramParmeterMode = (spmNec, spmOpt, spmKey, spmRest, spmFlag,
        spmCaptured);
    TSubprogramParameterDescription = record
        n: unicodestring;
        m: TSubprogramParmeterMode;
        r: boolean;
    end;
    TSubprogramSignature = array of TSubprogramParameterDescription;

    { TVSubprogram }

    TVSubprogram = class (TValue)
        //TODO: нужно пересмотреть дерево классов подпрограмм
        //поле signature используется только внутренними функциями
        //TVProcedure использует поле fsignature
        //поле stack не используется операторами и внутренними функциями
    end;


    { TVProcedure }

    TVProcedure = class (TVSubprogram)
        evaluated: boolean;
        fsignature: TSubprogramSignature;
        stack_pointer: integer;
        stack: TVSymbolStack;
        body: TVList;
        constructor Create;
        constructor CreateEmpty;
        destructor Destroy; override;
        procedure Print; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

    type TEvalProc = function (V: TValue): TValue of Object;
    type TInternalFunctionBody = function (const PL: TVList;
                                            ep: TEvalProc = nil): TValue;

    { TVInternalSubprogram }

    TVInternalSubprogram = class (TVSubprogram)
        name: unicodestring;
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
        name: unicodestring;
        constructor Create(sign: TVList;
                            body: TInternalFunctionBody;
                            name: unicodestring = '');
        destructor Destroy; override;
        procedure Print; override;
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
            oeDEFAULT,
            oeELT,
            oeFILTER,
            oeFOR,
            oeGOTO,
            oeIF,
            oeIF_NIL,
            oeLAST,
            oeLET,
            oeMAP,
            oeOR,
            oePOP,
            oePROCEDURE,
            oePUSH,
            oeQUOTE,
            oeSET,
            oeSTACK,
            oeSTRUCTURE,
            oeSTRUCTURE_AS,
            oeVAL,
            oeVAR,
            oeWHEN,
            oeWHILE);

    TVOperator = class (TVInternalSubprogram)
        name: unicodestring;
        op_enum: TOperatorEnum;
        param_is_check: boolean;

        constructor Create(name: unicodestring; en: TOperatorEnum; sign: TVList);
        destructor Destroy; override;
        procedure Print; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;
    end;

  //  TVAccess = class (TValue)

//type
//  TVFunction = class (TValue)
//    private
//      L: TObjectList;
//    public
//      constructor Create(body: TVList);
//      destructor Destroy; override;
      
//  end;

//    TStreamType = (stFile);

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

        procedure Print; override;
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

        procedure Print; override;
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
    end;

    { TVStreamDefault }

    TVStreamDefault = class (TValue)
        constructor Create;
        destructor Destroy; override;

        procedure Print; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        function read_byte(var b: byte): boolean;
        function write_byte(b: byte): boolean;
        function read_char(var ch: unicodechar): boolean;
        function write_char(ch: unicodechar): boolean;
    end;


    TVStream = class (TValue)
        procedure Print(V: TValue); virtual; abstract;
        function Read(): TValue; virtual; abstract;
        procedure Close; virtual; abstract;
    end;



    { TVStreamTextFile }

    TVStreamTextFile = class (TVStream)
    private
        fActive: boolean;

    public
        f: TFileStream;
        constructor Create(fn: unicodestring; mode: TFileMode);
        destructor Destroy; override;
        function Copy(): TValue; override;
        function AsString(): unicodestring; override;

        procedure Print(V: TValue); override;
        function Read(): TValue; override;
        procedure Close(); override;
    end;


type

    TPredicate = class
        function test(V: TValue): boolean; virtual; abstract;
    end;

    { is_any }

    is_any = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_number }

    is_number = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_integer }

    is_integer = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_float }

    is_float = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_string }

    is_string = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_symbol }

    is_symbol = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_list }

    is_list = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_atom }

    is_atom = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_NIL }

    is_NIL = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_keyword }

    is_keyword = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;

    { is_selfevaluating }

    is_selfevaluating = class (TPredicate)
        function test(V: TValue): boolean; override;
    end;


    Tforward = class
        f: integer;
    end;

procedure PrintValue(V: TObject);

procedure Assign(var v1, v2: TValue);

function op_null(V: TValue): boolean;

function op_is_error_class(V: TValue; e: TErrorClass): boolean;

function NewVariable(): PVariable;
function RefVariable(P: PVariable): PVariable;
procedure ReleaseVariable(var P: PVariable);

implementation

uses
    dlisp_read;

    { TVariable }

function NewVariable(): PVariable;
begin
    New(result);
    result.ref_count:=1;
    result.constant := false;
    result.V := nil;

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


procedure PrintValue(V: TObject);
begin
  (V as TValue).Print;
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

function op_is_error_class(V: TValue; e: TErrorClass): boolean;
begin
    result := (V is TVError) and ((V as TVError).e=e);
end;

{ TVTime }

procedure TVTime.Print;
begin
    WriteLn(asString);
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
    DecodeDate(fDT, year, month, day);
    DecodeTime(fDT, hour, minute, second, ms);
    result := '#<'+
        IntToStr(Year)+'-'+dd(month)+'-'+dd(day)+
        ' '+dd(hour)+':'+dd(minute)+':'+dd(second)+'>';
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

constructor TVByteVector.Create;
begin
    SetLength(fBytes,0);
end;

destructor TVByteVector.Destroy;
begin
    SetLength(fBytes,0);
    inherited Destroy;
end;

procedure TVByteVector.Print;
begin
    WriteLn(asstring);
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

function TVByteVector.SubSeq(istart, iend: integer): TVByteVector;
var i, high_i: integer;
begin
    if iend<0 then high_i := length(fBytes)-1 else high_i := iend-1;
    result := TVByteVector.Create;
    SetLength(result.fBytes, high_i-istart+1);
    for i := istart to high_i do result.fBytes[i-istart] := fBytes[i];
end;

function TVByteVector.High: integer;
begin
    result := length(fBytes)-1;
end;

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

function TVByteVector.Count: integer;
begin
    result := Length(fBytes);
end;

function TVByteVector.set_n(n: integer; V: TValue): boolean;
begin
    if (V is TVinteger) then begin
        if ((V as TVInteger).fI<0) or ((V as TVInteger).fI>255)
        then raise ELE.Create('byte', 'out of range');
        fBytes[n] := (V as TVInteger).fI;
        V.Free;
    end
    else raise ELE.InvalidParameters;
end;

function TVByteVector.get_n(n: integer): TValue;
begin
    result := TVInteger.Create(fBytes[n]);
end;

function TVByteVector.look_n(n: integer): TValue;
begin
    raise ELE.Create('не на что смотреть в векторе байт');
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

procedure TVEndOfStream.Print;
begin
    WriteLn(AsString);
end;

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

{ TVPointer }

constructor TVPointer.Create(c: TVCompound; i: integer);
begin
    compound := c;
    index := i;
end;

procedure TVPointer.Print;
begin
    WriteLn(AsString);
end;

function TVPointer.Copy: TValue;
begin
    result := TVPointer.Create(compound, index);
end;

function TVPointer.AsString: unicodestring;
var s: unicodestring;
begin
    try s := self.look.AsString; except s := 'XXXX'; end;
//    result := '#<POINTER '+IntToStr(index)+' of '+compound.AsString+'>';
    result := '#<POINTER to '+IntToStr(index)+' ('+S+') in '
                +compound.AsString+'>';
end;

function TVPointer.look: TValue;
begin
  //  WriteLn('link compound ',compound.ClassName);
    if compound is TVList
    then result := (compound as TVList).look[index]
    else
        if compound is TVStructure
        then result := (compound as TVStructure).look_n(index)
        else
            if compound is TVSymbolStack
            then result := (compound as TVSymbolStack).stack[index].V.V
            else
                if compound is TVCompoundOfPrimitive
                then result := TVError.Create(ecError,
                    compound.ClassName+' не содержит объектов')
                else
                    raise ELisyaError.Create('неизвестный составной тип');
end;

function TVPointer.value: TValue;
begin
    if compound is TVCompoundOfPrimitive
    then result := compound.get_n(index)
    else result := look.Copy;
end;

{ TVRange }

constructor TVRange.Create(l, h: Int64);
begin
    low := l;
    high := h;
end;

procedure TVRange.Print;
begin
    WriteLn(AsString);
end;

function TVRange.Copy: TValue;
begin
    result := TVRange.Create(low,high);
end;

function TVRange.AsString: unicodestring;
begin
    result := IntToStr(low)+'..'+IntToStr(high);
end;

{ TVClass }

constructor TVClass.Create(name: unicodestring; names: array of unicodestring);
var i: integer;
begin
    uname := UpperCaseU(Name);
    unames := TStringList.Create;
    for i := low(names) to high(names) do unames.Add(UpperCaseU(names[i]));
    unames.Sort;
end;

constructor TVClass.Create;
begin
    uname := '';
    unames := TStringList.Create;
end;

destructor TVClass.Destroy;
begin
    unames.free;
    inherited Destroy;
end;

procedure TVClass.Print;
begin
    WriteLn(self.AsString);
end;

function TVClass.Copy: TValue;
var i: integer;
begin
    result := TVClass.Create;
    (result as TVClass).uname := uname;
    (result as TVClass).unames.capacity := unames.Count;
    for i := 0 to unames.Count - 1 do (result as TVClass).unames.Add(unames[i]);
end;

function TVClass.AsString: unicodestring;
var i: integer;
begin
    //result := '#<CLASS '+uname+;
    //for i := 0 to unames.Count-1 do result := result + ' ' + unames[i];
    //result := result + '>';
end;

{ TVStructure }

function TVStructure.fGetSlot(index: unicodestring): TValue;
begin
    result := (slots[unames.IndexOf(index)] as TValue).copy;
end;

procedure TVStructure.fSetSlot(index: unicodestring; V: TValue);
begin
    slots[unames.IndexOf(index)] := V;
end;

function TVStructure.flook(index: unicodestring): TValue;
begin
    //TODO: обращение к несуществующему слоту структуры вызывает необрабатываемую ошибку
    result := slots[unames.IndexOf(index)] as TValue;
end;

constructor TVStructure.Create(names: array of unicodestring);
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

constructor TVStructure.Create(names: TStringList);
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

constructor TVStructure.Create;
begin
    unames := TStringList.Create;
    slots := TObjectList.Create(true);
end;

destructor TVStructure.Destroy;
begin
    unames.Free;
    slots.Free;
    inherited Destroy;
end;

procedure TVStructure.Print;
begin
    WriteLn(AsString);
end;

function TVStructure.Copy: TValue;
var i: integer;
begin
    //WriteLn('>>', self.AsString);
    result := TVStructure.Create;
    (result as TVStructure).unames.capacity := unames.count;
    (result as TVStructure).slots.capacity := unames.count;
    for i := 0 to unames.Count - 1 do begin
        (result as TVStructure).unames.Add(unames[i]);
        (result as TVStructure).slots.Add((slots[i] as TValue).copy);
    end;
end;

function TVStructure.AsString: unicodestring;
var i: integer;
begin
    result := '#S(';
    if slots.Count>0
    then result := result + unames[0] + ' ' + (slots[0] as TValue).AsString();
    for i := 1 to slots.Count - 1 do
    result := result + ' ' + unames[i] + ' ' + (slots[i] as TValue).AsString();
    result := result + ')';
end;

function TVStructure.is_class(cl: TVStructure): boolean;
var i: integer;
begin
    result := false;
    for i := 0 to cl.unames.Count-1 do
        if unames.IndexOf(cl.unames[i])<0 then exit;
    result := true;
end;

function TVStructure.SetSlot(index: unicodestring; V: TValue): boolean;
var i: integer;
begin
    i := unames.IndexOf(index);
    if i<0 then raise EInvalidParameters.Create('слот '+index+' отсутствует');
    slots[i] := V;
    result := true;
end;

function TVStructure.GetSlot(index: unicodestring; var V: TValue): boolean;
var i: integer;
begin
    i := unames.IndexOf(index);
    if i<0 then begin result := false; exit; end;
    V := (slots[i] as TValue).Copy();
    result := true;
end;

function TVStructure.AddSlot(name: unicodestring; V: TValue): boolean;
begin
    unames.Add(UpperCaseU(name));
    slots.Add(V);
end;

function TVStructure.count: integer;
begin
    result := unames.Count;
end;

function TVStructure.name_n(n: integer): unicodestring;
begin
    result := unames[n];
end;

function TVStructure.look_n(n: integer): TValue;
begin
    result := slots[n] as TValue;
end;

function TVStructure.set_n(n: integer; V: TValue): boolean;
begin
    Assert((n<slots.Count) and (n>=0),
        'Запись поля структуры по номеру за пределы диапазона');
    slots[n] := V;
    result := true;
end;

function TVStructure.get_n(n: integer): TValue;
begin
    result := (slots[n] as TValue).Copy();
end;

function TVStructure.get_n_of(index: unicodestring): integer;
begin
    result := unames.IndexOf(index);

end;


{ TVStreamDefault }

constructor TVStreamDefault.Create;
begin
    //
end;

destructor TVStreamDefault.Destroy;
begin
    inherited Destroy;
end;

procedure TVStreamDefault.Print;
begin
    WriteLn('#<STREAM STD_OUT>');
end;

function TVStreamDefault.Copy: TValue;
begin
    result := self;
end;

function TVStreamDefault.AsString: unicodestring;
begin
    result := '#<STREAM STD_OUT>';
end;

function TVStreamDefault.read_byte(var b: byte): boolean;
begin
    System.Read(b);
    result := true;
end;

function TVStreamDefault.write_byte(b: byte): boolean;
begin
    System.Write(b);
    result := true;
end;

function TVStreamDefault.read_char(var ch: unicodechar): boolean;
begin
    System.Read(ch);
    result := true;
end;

function TVStreamDefault.write_char(ch: unicodechar): boolean;
begin
    System.Write(ch);
    result := true;
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

procedure TVStreamPointer.Print;
begin
    if body.V<>nil
    then WriteLn('#<STREAM POINTER '+body.V.AsString()+' >')
    else WriteLn('#<STREAM POINTER closed>');
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
            //TODO: суррогатные пары не поддерживаются
        end;
        seUTF32BE: begin
            result := read_byte(b1) and read_byte(b2) and read_byte(b3)
                        and read_byte(b4);
            ch := unicodechar(256*256*256*b1+256*256*b2+256*b3+b4);
            //TODO: суррогатные пары не поддерживаются
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

procedure TVStreamBody.Print;
begin
    WriteLn('#<STREAM '+file_name+' >');
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

procedure TVOperator.Print;
begin
    writeLn('#<OPERATOR '+name+' '+signature.AsString()+'>');
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

procedure TVInternalFunction.Print;
begin
    WriteLn('#<INTERNAL '+name+' >');
end;

function TVInternalFunction.Copy: TValue;
begin
    result := TVInternalFunction.Create(signature.Copy as TVList, body, name);
end;

function TVInternalFunction.AsString: unicodestring;
begin
    result := '#<INTERNAL '+name+' '+signature.AsString()+'>';
end;

{ TVSubprogram }

//constructor TVSubprogram.Create;
//begin
//    signature := TVList.Create;
//    stack := TVSymbolStack.Create(nil);
//end;

//destructor TVSubprogram.Destroy;
//begin
//    signature.Free;
//    stack.Free;
//    inherited Destroy;
//end;


{ TVContinue }

procedure TVContinue.Print;
begin
     write('#<CONTINUE>');
end;

function TVContinue.Copy: TVContinue;
begin
    result := TVContinue.Create;
end;

function TVContinue.AsString: unicodestring;
begin
    result := '#<CONTINUE>';
end;

{ TVBreak }

procedure TVBreak.Print;
begin
    write('#<BREAK>');
end;

function TVBreak.Copy: TVBreak;
begin
    result := TVBreak.Create;
end;

function TVBreak.AsString: unicodestring;
begin
    result := '#<BREAK>';
end;


{ TSymbolStack }

constructor TSymbolStack.Create(parent: TSymbolStack);
var i: integer;
begin
    stack := nil;
    if parent<> nil
    then begin
        SetLength(stack, Length(parent.stack));
        for i := 0 to high(parent.stack) do
        begin
            stack[i].name := parent.stack[i].name;
            stack[i].V := RefVariable(parent.stack[i].V);
        end;
    end;
end;

destructor TSymbolStack.Destroy;
var i: integer;
begin
    for i := 0 to high(stack) do
    begin
        ReleaseVariable(stack[i].V);
        stack[i].name := '';
    end;
    SetLength(stack,0);
    inherited;
end;

procedure TSymbolStack.new_var(name: unicodestring; V: TValue; c: boolean = false);
var this: integer;
begin
    this := Length(stack);
    SetLength(stack, this+1);
    stack[this].name := UpperCaseU(name);
    stack[this].V := NewVariable;
    stack[this].V.V := V;
    stack[this].V.constant:=c;
end;

function TSymbolStack.find_var(name: unicodestring; var V: TValue): boolean;
var i: integer; uname: unicodestring;
begin
    result := false;
    uname := UpperCaseU(name);
    for i:= Length(stack)-1 downto 0 do
        if (uname=stack[i].name) and (stack[i].V<> nil)
        then begin
            V := stack[i].V.V.Copy();
            result := true;
            break;
        end;
end;

function TSymbolStack.set_var(name: unicodestring; V: TValue): boolean;
var i: integer; uname: unicodestring;
label lend;
begin
    result := false;
    uname := UpperCaseU(name);
    for i:= Length(stack)-1 downto 0 do
        if uname=stack[i].name
        then begin
            if stack[i].V<>nil
            then stack[i].V.V.Free
            else stack[i].V:=NewVariable;
            if not stack[i].V.constant
            then begin
                stack[i].V.V := V;
                result := true;
            end;
            break;
        end;
    if not result then V.Free;
end;

function TSymbolStack.bind_var(name, target: unicodestring): boolean;
var this, i: integer; utarget: unicodestring;
begin
    result := false;
    this := Length(stack);
    SetLength(stack, this+1);
    stack[this].name := name;
    utarget := UpperCaseU(target);
    for i:= this-1 downto 0 do
        if utarget=stack[i].name
        then begin
            stack[this].V := RefVariable(stack[i].V);
            result := true;
            break;
        end;
end;

function TSymbolStack.find_ref(name: unicodestring; var P: PVariable): boolean;
var i: integer; uname: unicodestring;
begin
    P := nil;
    result := false;
    uname := UpperCaseU(name);
    for i:= Length(stack)-1 downto 0 do
        if uname=stack[i].name
        then begin
            P := RefVariable(stack[i].V);
            result := true;
            Exit;
        end;
    raise ESymbolNotBound.Create(name);
end;

function TSymbolStack.find_ref_in_frame(name: unicodestring;
                                        n: integer;
                                        var P: PVariable): boolean;
    var i: integer; uname: unicodestring;
begin
    P := nil;
    result := false;
    uname := UpperCaseU(name);
    for i:= n to high(stack) do begin
        if stack[i].name[1]=' ' then break;
        if uname=stack[i].name
        then begin
            P := RefVariable(stack[i].V);
            result := true;
            break;
        end;
    end;
end;

procedure TSymbolStack.new_ref(name: unicodestring; P: PVariable);
var this: integer;
begin
    this := Length(stack);
    SetLength(stack, this+1);
    stack[this].name := UpperCaseU(name);
    stack[this].V := P;
end;

function TSymbolStack.find_unbound(var name: unicodestring): boolean;
var i: integer; uname: unicodestring;
begin
    result := false;
    for i:= Length(stack)-1 downto 0 do
        if stack[i].V=nil
        then begin
            name := stack[i].name;
            break;
        end;
end;


procedure TSymbolStack.print_stack;
var i: integer;
begin
    WriteLn('-------------');
    for i := 76 to Length(stack)-1 do begin
        Write('  | ', stack[i].name);
        if stack[i].V<> nil
        then WriteLn('  >(', stack[i].V.ref_count,')>  ', stack[i].V.V.AsString)
        else WriteLn(' nil');
    end;
    WriteLn('-------------');
end;


procedure TSymbolStack.clear_frame;
var sp: integer;
begin
    //print_stack;
    sp := High(stack);
    repeat
        //print_stack;
        if (stack[sp].name[1]=' ')
        then begin
            releaseVariable(stack[sp].V);
            SetLength(stack, sp);
            break;
        end;
        releaseVariable(stack[sp].V);
        SetLength(stack, sp);

        Dec(sp);
    until sp<0;
end;

function TSymbolStack.count: integer;
begin
    result := Length(stack);
end;






{ TVStreamTextFile }

constructor TVStreamTextFile.Create(fn: unicodestring; mode: TFileMode);
var tf: file;
begin
    //Writeln('create text file');
    case mode of
        fmRead: f := TFileStream.Create(fn, fmOpenRead);
        fmWrite: begin
            AssignFile(tf, fn);
            Rewrite(tf);
            CloseFile(tf);
            f := TFileStream.Create(fn, fmOpenWrite);
        end;
        fmAppend: begin
            f := TFileStream.Create(fn, fmOpenReadWrite);
            f.Seek(f.Size,0);
        end;
    end;
    fActive := true;
end;

destructor TVStreamTextFile.Destroy;
begin
    FreeAndNil(F);
    //CloseFile(F);
    inherited Destroy;
end;

function TVStreamTextFile.Copy: TValue;
begin
    Assert(false, 'Копирование потока');
    result := TVError.Create(ecError, '');
end;

function TVStreamTextFile.AsString: unicodestring;
begin
    if F=nil
    then result := '#<TEXTFILE closed>'
    else result := '#<TEXTFILE opened>';
end;

procedure TVStreamTextFile.Print(V: TValue);
begin
    dlisp_read.print_s(V, f);
end;

function TVStreamTextFile.Read: TValue;
begin
   // result := dlisp_read.read_s(F);
   //TODO: удалить TVStreamTextFile
end;

procedure TVStreamTextFile.Close;
begin
    FreeAndNil(F);
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
end;

constructor TVProcedure.CreateEmpty;
begin
    inherited;
    body := nil;
    stack := nil;

    fsignature := nil;
    evaluated := false;
    stack_pointer := -1;
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

procedure TVProcedure.Print;
var i: integer;
begin
    WriteLn('#<PROCEDURE');
    body.Print;
    WriteLn(' PROCEDURE>');
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
    for i := 0 to Length(fsignature)-1 do
        PL := PL + fsignature[i].n+m(fsignature[i].m)+'|';
    result := '#<PROCEDURE |'+pl+' '+
        body.AsString()[1..(50-Length(PL))]+'...>';
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

procedure TVGoto.Print;
begin
    WriteLn('#<GOTO '+IntTostr(n)+uname+'>');
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

{ is_selfevaluating }

function is_selfevaluating.test(V: TValue): boolean;
begin
    result := (V is TVNumber)
        or (V is TVString)
        or ((V is TVSymbol) and ((V as TVSymbol).name[1]=':'))
        or ((V is TVList) and ((V as TVList).count=0));
end;

{ is_keyword }

function is_keyword.test(V: TValue): boolean;
begin
    result := (V is TVSymbol) and ((V as TVSymbol).name[1]=':');
end;

{ is_NIL }

function is_NIL.test(V: TValue): boolean;
begin
    result := (V is TVList) and ((V as TVList).Count=0);
end;

{ is_atom }

function is_atom.test(V: TValue): boolean;
begin
    result := (not (V is TVList)) or ((V as TVList).Count=0);
end;

{ is_list }

function is_list.test(V: TValue): boolean;
begin
    result := V is TVList;
end;

{ is_symbol }

function is_symbol.test(V: TValue): boolean;
begin
    result := V is TVSymbol;
end;

{ is_string }

function is_string.test(V: TValue): boolean;
begin
    result := V is TVString;
end;

{ is_float }

function is_float.test(V: TValue): boolean;
begin
    result := (V is TVFloat);
end;

{ is_integer }

function is_integer.test(V: TValue): boolean;
begin
    result := V is TVInteger;
end;

{ is_number }

function is_number.test(V: TValue): boolean;
begin
    result := (V is TVInteger) or (V is TVFloat);
end;

{ is_any }

function is_any.test(V: TValue): boolean;
begin
    result := true;
end;

{ TVT }

procedure TVT.Print;
begin
    WriteLn('T');
end;

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

procedure TVFloat.Print;
begin
    WriteLn(F);
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


{ TVError }

constructor TVError.Create(e: TErrorClass; msg: unicodestring);
begin self.e:=e; self.msg:=msg; end;

destructor TVError.Destroy;
begin msg := ''; inherited Destroy; end;

procedure TVError.Print;
begin
    writeLn('#<ERROR ', ErrorClassName[e], '> ', msg);
end;

function TVError.Copy: TValue;
begin result := TVError.Create(e, msg); end;

function TVError.AsString: unicodestring;
begin
    result := '#<ERROR ' + ErrorClassName[e] + ': ' + msg + ' >';
end;


{ TVString }

constructor TVString.Create(S: unicodestring);
begin self.S := S; end;

destructor TVString.Destroy;
begin self.S := ''; inherited; end;

procedure TVString.Print;
begin
    WriteLn(AsString);
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

function TVString.set_n(n: integer; V: TValue): boolean;
begin
    //TODO: нужно вставить проверки при замене буквы в TVString
    S[n+1] := (V as TVString).S[1];
    result := true;
end;

function TVString.get_n(n: integer): TValue;
begin
    //TODO: нужно вставить проверки при извлечении буквы в TVString
    result := TVString.Create(S[n+1]);
end;

function TVString.look_n(n: integer): TValue;
begin
    assert(false, 'TVString.look_n');
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

procedure   TVInteger.Print; begin WriteLn(fI); end;


  { TVSymbol }

function TVSymbol.Copy: TValue;
begin result := TVSymbol.Create(self.fname); end;

function TVSymbol.AsString: unicodestring;
begin
    result := fname;
end;

function TVSymbol.uname: unicodestring;
begin
    result := UpperCaseU(fname);
end;

constructor TVSymbol.Create(S: unicodestring);
begin
   // if s[1..5]='-----' then
   // WriteLn('create ',s);
    fname := S;
end;

destructor TVSymbol.Destroy;
begin
   // if fname[1..5]='-----' then
  //  WriteLn('destroy ', fname);
    fname := '';
    inherited;
end;

procedure TVSymbol.Print;
begin writeLn(fname); end;


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

function TVList.flook(index: integer): TValue;
begin
    result := (fL[Index] as TValue);
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
begin result := fL.Count; end;

function TVList.High: integer;
begin
    result := fl.Count-1;
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

//except
//    result := 'kgjg';
//end;
end;

var lc: integer = 0;

constructor TVList.Create;
begin
  fL := TObjectList.Create(true);
  //  Inc(lc);
  //WriteLn('cr', lc);
end;

constructor TVList.Create(VL: array of TValue);
var i :integer;
begin
//Inc(lc);
 // WriteLn('cr', lc);
    fL := TObjectList.Create(true);
    fL.Capacity:=length(VL);
    for i:=0 to length(VL)-1 do fL.Add(VL[i]);
end;

destructor TVList.Destroy;
begin
   // dec(lc);
  //  WriteLn('ds', lc);
  fL.Free;
  inherited;
end;

function TVList.GetItems(Index: Integer): TValue;
begin
  result := (fL[Index] as TValue).Copy();
end;

procedure TVList.Print;
var i: integer;
begin
    if fL.Count=0
    then WriteLn('NIL')
    else
        begin
            WriteLn('(');
            for i := 0 to fL.Count-1 do (fL[i] as TValue).Print;
            WriteLn(')');
        end;
end;

procedure TVList.SetItems(Index: Integer; V: TValue);
begin
  fL[Index] := V;
end;

function TVList.Subseq(istart, iend: integer): TVList;
var i: integer;
begin
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

function TVList.set_n(n: integer; V: TValue): boolean;
begin
    SetItems(n, v);
end;

function TVList.get_n(n: integer): TValue;
begin
    result := GetItems(n);
end;

function TVList.look_n(n: integer): TValue;
begin
    result := flook(n);
end;

end.