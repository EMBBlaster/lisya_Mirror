unit lisya_thread;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring, ctypes,
    {$ENDIF}
    Classes, SysUtils, dlisp_eval, dlisp_values, lisya_ifh, lisya_gc
    , lisya_exceptions
    , lisya_predicates
    , mar;

type

    TAction = procedure of object;

    { TEvaluationThread }

    TEvaluationThread = class (TThread)
    private
        fflow: dlisp_eval.TEvaluationFlow;

        fExpression: TVList;
        fProc: TVSubprogram;
        fParams: TVList;
        fN: integer;

        fComplitedEvent: pRTLEvent;
        eclass, emessage, estack: unicodestring;
        fError: boolean;
        fAction: TAction;
        procedure SetProc(P: TVSubprogram);
        procedure fMap;
        procedure fReject;
        procedure fFilter;
    protected
        procedure Execute; override;
    public
        fResult: TValue;
        property Proc: TVSubprogram write SetProc;
        constructor Create;
        destructor Destroy; override;
        procedure eval(PL: TVList); overload;
        procedure RunMap;
        procedure RunReject;
        procedure RunFilter;
        procedure WaitEnd;
        function WaitResult: TValue;
    end;

    { TLThreadBody }

    TLThreadBody = class (TThread)
    private
        fflow: TEvaluationFlow;
        fexpr: TVList;
        fresult: TValue;
        eclass, emessage, estack: unicodestring;
        fError: boolean;
    protected
        procedure Execute; override;
    public
        constructor Create(expr: TVList);
        destructor Destroy; override;
        function WaitResult: TValue;
    end;

    TLThread = class (TCountingObject)
    private
        thread: TLThreadBody;
        fname: unicodestring;
    public
        constructor Create(expr: TVList);
        destructor Destroy; override;
        function description: unicodestring; override;
        function WaitResult: TValue;
    end;

    TVThread = TVPointer<TLThread>;



function ifh_map_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;
function ifh_reject_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;
function ifh_filter_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;

var threads_pool: array of TEvaluationThread;

{$IFDEF MULTITHREADING}
var th: boolean = false;
{$ELSE}
var th: boolean = true;
{$ENDIF}

procedure set_threads_count(n: integer);
function CPU_Count: integer;

function tpThread(V: TValue): boolean;

implementation


var task_cs: TRTLCriticalSection;
    complited_event: pRTLEvent;
    task_queue: array of TValue;
    task_i: integer = 0;

{$IFDEF LINUX}
function sysconf(i:cint):clong;cdecl;external name 'sysconf';
{$ENDIF}

function CPU_Count: integer;
begin
    {$IFDEF LINUX}
        result := sysconf(83);  // вероятно это константа _SC_NPROCESSORS_CONF или _SC_NPROCESSORS_ONLN
    {$ELSE}
        result := GetCPUCount;
    {$ENDIF}
    if result <1 then result := 1;
end;

function tpThread(V: TValue): boolean;
begin
    result := V is TVThread;
end;


procedure set_threads_count(n: integer);
var i, n_old: integer;
begin
    if n<Length(threads_pool) then begin
        for i := n to high(threads_pool) do threads_pool[i].Free;
        SetLength(threads_pool, n);
    end;

    if n>Length(threads_pool) then begin
        n_old := Length(threads_pool);
        SetLength(threads_pool, n);
        for i := n_old to high(threads_pool) do
            threads_pool[i] := TEvaluationThread.Create;
    end;
end;

function ifh_map_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;
var i,j: integer;
begin
    //TODO: возможна утечка содержимого очереди заданий при возникновении исключения
    if (not th) and (length(threads_pool)>1) then try
        th := true;
        result := nil;
        for i := 0 to high(threads_pool) do threads_pool[i].Proc := P;
        SetLength(task_queue, PL.L[0].Count);
        task_i := 0;
        for i := 0 to high(task_queue) do begin
            task_queue[i] := TVList.Create();
            for j := 0 to PL.high do
                (task_queue[i] as TVList).Add(separate(PL.L[j].look[i]));
        end;
        for i := 0 to high(threads_pool) do threads_pool[i].RunMap;
        for i := 0 to high(threads_pool) do threads_pool[i].WaitEnd;
        result := TVList.Create;
        result.SetCapacity(Length(task_queue));
        for i := 0 to high(task_queue) do result.Add(task_queue[i]);
        SetLength(task_queue, 0);
        for i := 0 to high(threads_pool) do threads_pool[i].fProc := nil;
    finally
        th := false;
    end
    else result := ifh_map(call, P, PL);
end;

function ifh_reject_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;
var i: integer;
begin
    //TODO: возможна утечка содержимого очереди заданий при возникновении исключения
    if (not th) and (length(threads_pool)>1) then try
        th := true;
        result := nil;
        for i := 0 to high(threads_pool) do threads_pool[i].Proc := P;
        SetLength(task_queue, PL.Count);
        task_i := 0;
        for i := 0 to high(task_queue) do task_queue[i] := separate(PL.look[i]);
        for i := 0 to high(threads_pool) do threads_pool[i].RunReject;
        for i := 0 to high(threads_pool) do threads_pool[i].WaitEnd;
        result := TVList.Create;
        result.SetCapacity(Length(task_queue));
        for i := 0 to high(task_queue) do
            if task_queue[i]<>nil then result.Add(task_queue[i]);
        SetLength(task_queue, 0);
        for i := 0 to high(threads_pool) do FreeAndNil(threads_pool[i].fProc);
    finally
        th := false;
    end
    else result := ifh_filter(PL, call, tpNIL) as TVList;
end;

function ifh_filter_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;
var i: integer;
begin
    //TODO: возможна утечка содержимого очереди заданий при возникновении исключения
    if (not th) and (length(threads_pool)>1) then try
        th := true;
        result := nil;
        for i := 0 to high(threads_pool) do threads_pool[i].Proc := P;
        SetLength(task_queue, PL.Count);
        task_i := 0;
        for i := 0 to high(task_queue) do task_queue[i] := separate(PL.look[i]);
        for i := 0 to high(threads_pool) do threads_pool[i].RunFilter;
        for i := 0 to high(threads_pool) do threads_pool[i].WaitEnd;
        result := TVList.Create;
        result.SetCapacity(Length(task_queue));
        for i := 0 to high(task_queue) do
            if task_queue[i]<>nil then result.Add(task_queue[i]);
        SetLength(task_queue, 0);
        for i := 0 to high(threads_pool) do FreeAndNil(threads_pool[i].fProc);
    finally
        th := false;
    end
    else result := ifh_filter(PL, call, tpTrue) as TVList;
end;

function  NextTask(out tn: integer): boolean;
begin
    EnterCriticalSection(task_cs);
    result := task_i<Length(task_queue);
    tn := task_i;
    Inc(task_i);
    LeaveCriticalSection(task_cs);
end;

{ TLThread }

constructor TLThread.Create(expr: TVList);
begin
    inherited Create;
    fname := expr.AsString;
    thread := TLThreadBody.Create(expr);
end;

destructor TLThread.Destroy;
begin
    thread.FreeOnTerminate:=true;
//    thread.Destroy;
    inherited Destroy;
end;

function TLThread.description;
begin
    if thread.Finished
    then result := 'THREAD FINISHED '+fname
    else result := 'THREAD '+fname
end;

function TLThread.WaitResult: TValue;
begin
    result := thread.WaitResult;
end;

{ TLThreadBody }

procedure TLThreadBody.Execute;
begin
    try
        fresult := fflow.eval(fexpr);
    except
        on E:ELE do begin
            fError := true;
            eclass := E.EClass;
            emessage := E.Message;
            estack := E.EStack;
        end;
        on E:Exception do begin
            fError := true;
            eclass := '!'+E.ClassName+'!';
            emessage := E.Message;
            estack := 'thread';
        end;
    end;
end;

constructor TLThreadBody.Create(expr: TVList);
begin
    fexpr := separate(expr) as TVList;
    fflow := TEvaluationFlow.CreatePure;
    fResult := nil;
    fError := false;
    FreeOnTerminate := false;
    inherited Create(false);
end;

destructor TLThreadBody.Destroy;
begin
    WaitFor;
    fflow.Free;
    fResult.Free;
    fExpr.Free;
    inherited;
end;

function TLThreadBody.WaitResult: TValue;
begin
    result := nil;
    self.WaitFor;
    if fError then raise ELE.Create(emessage,eclass,estack);
    result := fResult.Copy;
end;



{ TEvaluationThread }

procedure TEvaluationThread.SetProc(P: TVSubprogram);
begin
    fProc.Free;
    fProc := separate(P) as TVSubprogram;
end;

procedure TEvaluationThread.fMap;
var PL: TVList;
begin
    fExpression := TVList.Create([fProc],false);
    PL := task_queue[fN] as TVList;
    fExpression.Append(PL);
    task_queue[fN] := fFlow.call(fExpression);
    FreeAndNil(fExpression);
    PL.Free;
end;

procedure TEvaluationThread.fReject;
var tmp: TValue;
begin try
    fExpression := TVList.Create([fProc], false);
    fExpression.Add(task_queue[fN]);
    tmp := fFlow.call(fExpression);
    if tpTrue(tmp) then FreeAndNil(task_queue[fN]);
    FreeAndNil(fExpression);
finally
    tmp.Free;
end;end;

procedure TEvaluationThread.fFilter;
var tmp: TValue;
begin try
    fExpression := TVList.Create([fProc], false);
    fExpression.Add(task_queue[fN]);
    tmp := fFlow.call(fExpression);
    if tpNIL(tmp) then FreeAndNil(task_queue[fN]);
    FreeAndNil(fExpression);
finally
    tmp.Free;
end;end;

procedure TEvaluationThread.Execute;
begin
    while not self.Terminated do begin
        try
            fError := false;

            while NextTask(fN) do fAction;
        except
            on E:ELE do begin
                fError := true;
                eclass := E.EClass;
                emessage := E.Message;
                estack := E.EStack;
            end;
            on E:Exception do begin
                fError := true;
                eclass := '!'+E.ClassName+'!';
                emessage := E.Message;
                estack := 'thread';
            end;
        end;
        RtlEventSetEvent(fComplitedEvent);
        self.Suspended := true;
    end;
end;

constructor TEvaluationThread.Create;
begin
    self.fExpression := nil;
    fProc := nil;
    fParams := nil;
    self.fflow := TEvaluationFlow.CreatePure;
    self.fResult := nil;
    self.FreeOnTerminate := false;
    fComplitedEvent := RTLEventCreate;
    inherited Create(true);
end;


destructor TEvaluationThread.Destroy;
begin
    fflow.Free;
    fResult.Free;
    fExpression.Free;
    fProc.Free;
    fParams.Free;
    RTLeventdestroy(fComplitedEvent);
    self.Terminate;
    self.Suspended := false;
    inherited;
end;


procedure TEvaluationThread.eval(PL: TVList);
begin
    fExpression := separate(PL) as TVList;
    RTLEventResetEvent(fComplitedEvent);
    self.Start;
end;

procedure TEvaluationThread.RunMap;
begin
    RTLEventResetEvent(fComplitedEvent);
    fAction := fMap;
    Start;
end;

procedure TEvaluationThread.RunReject;
begin
    RTLEventResetEvent(fComplitedEvent);
    fAction := fReject;
    Start;
end;

procedure TEvaluationThread.RunFilter;
begin
    RTLEventResetEvent(fComplitedEvent);
    fAction := fFilter;
    Start;
end;

procedure TEvaluationThread.WaitEnd;
begin
    RtlEventWaitFor(fComplitedEvent);
    if fError then raise ELE.Create(emessage, eclass, estack);
end;


function TEvaluationThread.WaitResult: TValue;
begin
    RtlEventWaitFor(fComplitedEvent);

    result := fResult;
    fResult := nil;
    if fError then raise ELE.Create(emessage, eclass, estack);
end;


initialization
    {$IFDEF MULTITHREADING}
    set_threads_count(CPU_count);
    {$ENDIF}
    InitCriticalSection(task_cs);
    complited_event := RTLEventCreate;

finalization
    set_threads_count(0);
    DoneCriticalSection(task_cs);
    RTLeventdestroy(complited_event);
end.

