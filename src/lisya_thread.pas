unit lisya_thread;

{$mode delphi}

interface

uses
    Classes, SysUtils, dlisp_eval, dlisp_values, lisya_ifh;

type

    TMTFunction = function (call: TCallProc; P: TVSubprogram; PL: TVList; b,e: integer): TValue;

    { TEvaluationThread }

    TEvaluationThread = class (TThread)
    private
        fflow: dlisp_eval.TEvaluationFlow;
        fFunction: TMTFunction;
        fExpression: TVList;
        fP: TVSubprogram;
        fData: TVList;
        fComplitedEvent: pRTLEvent;
        eclass, emessage, estack: unicodestring;
        fA, fB: integer;
        fError: boolean;
    protected
        procedure Execute; override;
    public
        fResult: TValue;
        constructor Create;
        destructor Destroy; override;
        procedure eval(f: TMTFunction; P: TVSubprogram; PL: TVList; b, e: integer);
        function WaitResult: TValue;
    end;


var map_threads_pool: array of TEvaluationThread;

{$IFDEF MULTITHREADING}
var th: boolean = false;
{$ELSE}
var th: boolean = true;
{$ENDIF}

procedure set_threads_count(n: integer);

implementation

procedure set_threads_count(n: integer);
var i, n_old: integer;
begin
    if n<Length(map_threads_pool) then begin
        for i := n to high(map_threads_pool) do map_threads_pool[i].Free;
        SetLength(map_threads_pool, n);
    end;

    if n>Length(map_threads_pool) then begin
        n_old := Length(map_threads_pool);
        SetLength(map_threads_pool, n);
        for i := n_old to high(map_threads_pool) do
            map_threads_pool[i] := TEvaluationThread.Create;
    end;

end;


{ TEvaluationThread }

procedure TEvaluationThread.Execute;
begin
    while not self.Terminated do begin
        try
            //fResult := fFun;//fFlow.eval(fExpression);
            fResult := fFunction(fflow.call, fP, fData, fA, fB);
            fExpression := nil;
            fError := false;
        except
            on E:ELE do begin
                fError := true;
                eclass := E.EClass;
                emessage := E.Message;
                estack := E.EStack;
            end;
            on E:Exception do begin
                WriteLn('необработанная ошибка в потоке');
                WriteLn(E.Message);
                raise;
            end;

        end;
        RtlEventSetEvent(fComplitedEvent);
        self.Suspended := true;
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
    //fExpression.Free;
    RTLeventdestroy(fComplitedEvent);
    self.Terminate;
    self.Suspended := false;
    inherited;
end;

procedure TEvaluationThread.eval(f: TMTFunction; P: TVSubprogram; PL: TVList; b, e: integer);
begin
    fFunction := f;
    fP := P;
    fData := PL;
    fA := b;
    fB := e;
    RTLEventResetEvent(fComplitedEvent);
    self.Start;
end;

function TEvaluationThread.WaitResult: TValue;
begin
    RtlEventWaitFor(fComplitedEvent);

    result := fResult;
    fResult := nil;
    if fError then begin
        WriteLn('in thread error');
        raise ELE.Create(emessage, eclass, estack);

    end;
end;


end.

