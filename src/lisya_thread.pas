﻿unit lisya_thread;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring, ctypes,
    {$ENDIF}
    Classes, SysUtils, dlisp_eval, dlisp_values, lisya_ifh, lisya_gc, lisya_exceptions;

type

    TMTFunction = function (call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;

    { TEvaluationThread }

    TEvaluationThread = class (TThread)
    private
        fflow: dlisp_eval.TEvaluationFlow;

        fExpression: TVList;
        fProc: TVSubprogram;
        fParams: TVList;

        fComplitedEvent: pRTLEvent;
        eclass, emessage, estack: unicodestring;
        fError: boolean;
        procedure SetProc(P: TVSubprogram);
    protected
        procedure Execute; override;
    public
        fResult: TValue;
        property Proc: TVSubprogram write SetProc;
        constructor Create;
        destructor Destroy; override;
        procedure eval(f: TMTFunction; P: TVSubprogram; PL: TVList; b, e: integer); overload;
        procedure eval(PL: TVList); overload;
        procedure Run;
        procedure WaitEnd;
        function WaitResult: TValue;
    end;


function ifh_map_th(call: TCallProc; P: TVSubprogram; PL: TVList): TVList;

var threads_pool: array of TEvaluationThread;

{$IFDEF MULTITHREADING}
var th: boolean = false;
{$ELSE}
var th: boolean = true;
{$ENDIF}

procedure set_threads_count(n: integer);
function CPU_Count: integer;

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
        result := sysconf(83);
    {$ELSE}
        result := GetCPUCount;
    {$ENDIF}
    if result <1 then result := 1;
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
        for i := 0 to high(threads_pool) do threads_pool[i].Run;
        for i := 0 to high(threads_pool) do threads_pool[i].WaitEnd;
        result := TVList.Create;
        result.SetCapacity(Length(task_queue));
        for i := 0 to high(task_queue) do result.Add(task_queue[i]);
        SetLength(task_queue, 0);
        for i := 0 to high(threads_pool) do threads_pool[i].fProc := nil;
    finally
        th := false;
    end
    else result := ifh_map(call, P, PL, 0, PL.L[0].high);
end;

function  NextTask(out tn: integer): boolean;
begin
    EnterCriticalSection(task_cs);
    result := task_i<Length(task_queue);
    tn := task_i;
    Inc(task_i);
    LeaveCriticalSection(task_cs);
end;



{ TEvaluationThread }

procedure TEvaluationThread.SetProc(P: TVSubprogram);
begin
    fProc.Free;
    fProc := separate(P) as TVSubprogram;
end;

procedure TEvaluationThread.Execute;
var n: integer;
begin
    while not self.Terminated do begin
        try
            fError := false;
            //fResult := fFlow.eval(fExpression);
            while NextTask(n) do begin
                fExpression := TVList.Create([fProc.Copy]);
                fExpression.Append(task_queue[n] as TVList);
                task_queue[n] := fFlow.call(fExpression);
                FreeAndNil(fExpression);
            end;
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
    self.fflow := TEvaluationFlow.Create(nil);
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

procedure TEvaluationThread.eval(f: TMTFunction; P: TVSubprogram; PL: TVList;
    b, e: integer);
begin

end;


procedure TEvaluationThread.eval(PL: TVList);
begin
    fExpression := separate(PL) as TVList;
    RTLEventResetEvent(fComplitedEvent);
    self.Start;
end;

procedure TEvaluationThread.Run;
begin
    RTLEventResetEvent(fComplitedEvent);
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

