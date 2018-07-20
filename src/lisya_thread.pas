unit lisya_thread;

{$mode delphi}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, dlisp_eval, dlisp_values, lisya_ifh, lisya_gc;

type

    TMTFunction = function (call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;

    { TEvaluationThread }

    TEvaluationThread = class (TThread)
    private
        fflow: dlisp_eval.TEvaluationFlow;

        fExpression: TVList;

        fComplitedEvent: pRTLEvent;
        eclass, emessage, estack: unicodestring;
        fError: boolean;
    protected
        procedure Execute; override;
    public
        fResult: TValue;
        constructor Create;
        destructor Destroy; override;
        procedure eval(f: TMTFunction; P: TVSubprogram; PL: TVList; b, e: integer); overload;
        procedure eval(PL: TVList); overload;
        function WaitResult: TValue;
    end;


var threads_pool: array of TEvaluationThread;

{$IFDEF MULTITHREADING}
var th: boolean = false;
{$ELSE}
var th: boolean = true;
{$ENDIF}

procedure set_threads_count(n: integer);

implementation

var complited_cs: TRTLCriticalSection;
    complited_event: pRTLEvent;

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

procedure Ready;
begin
    EnterCriticalSection(complited_cs);
    RtlEventSetEvent(complited_event);
    LeaveCriticalSection(complited_cs);
end;

function NextThread: TEvaluationThread;
var i: integer;
begin
    RtlEventWaitFor(complited_event);
    EnterCriticalSection(complited_cs);
    RTLEventResetEvent(complited_event);
    for i := 0 to high(threads_pool) do begin
        if threads_pool[i].Suspended then begin
            result := threads_pool[i];
            Exit;
        end;
    end;


end;

{ TEvaluationThread }

procedure TEvaluationThread.Execute;
begin
    while not self.Terminated do begin
        try
            fError := false;
            fResult := fFlow.eval(fExpression);
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
        self.Suspended := true;
        ready;
    end;
end;

constructor TEvaluationThread.Create;
begin
    self.fExpression := nil;
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
    RTLeventdestroy(fComplitedEvent);
    self.Terminate;
    self.Suspended := false;
    inherited;
end;


procedure TEvaluationThread.eval(PL: TVList);
begin
    fExpression := separate(PL);
    RTLEventResetEvent(fComplitedEvent);
    self.Start;
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
    set_threads_count(4);
    {$ENDIF}
    InitCriticalSection(complited_cs);
    complited_event := RTLEventCreate;

finalization
    set_threads_count(0);
    DoneCriticalSection(complited_cs);
    RTLeventdestroy(complited_event);
end.

