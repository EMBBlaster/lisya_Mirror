unit lisya_thread;

{$mode delphi}

interface

uses
    Classes, SysUtils, dlisp_eval, dlisp_values;

type

    { TSubprogramThread }

    TEvaluationThread = class (TThread)
    private
        fflow: dlisp_eval.TEvaluationFlow;
        fExpression: TValue;
        fComplitedEvent: pRTLEvent;
    protected
        procedure Execute; override;
    public
        fResult: TValue;
        constructor Create(CreateSuspended: boolean);
        destructor Destroy; override;
        procedure eval(ps: TVSymbolStack; V: TValue);
        function WaitResult: TValue;
    end;

implementation

{ TEvaluationThread }

procedure TEvaluationThread.Execute;
begin
    while not self.Terminated do begin
        try
            fResult := fFlow.eval(fExpression);
            fExpression := nil;
        except
            on E:ELE do
                fResult := TVError.Create(ecError, E.Message);
        end;
        RtlEventSetEvent(fComplitedEvent);
        self.Suspend;
    end;
end;

constructor TEvaluationThread.Create(CreateSuspended: boolean);
begin
    inherited Create(CreateSuspended);
    self.fExpression := nil;
    self.fflow := TEvaluationFlow.Create;
    self.fResult := nil;
    fComplitedEvent := RTLEventCreate;
end;


destructor TEvaluationThread.Destroy;
begin
    fflow.Free;
    fResult.Free;
    fExpression.Free;
    RTLeventdestroy(fComplitedEvent);
    self.Terminate;
    self.Resume;
    inherited;
end;

procedure TEvaluationThread.eval(ps: TVSymbolStack; V: TValue);
begin
    if fResult<>nil then raise ELE.Create('поток занят');

    fflow := TEvaluationFlow.Create(ps);
    fExpression := V;
    RTLEventResetEvent(fComplitedEvent);
    self.Resume;
end;

function TEvaluationThread.WaitResult: TValue;
begin
    RtlEventWaitFor(fComplitedEvent);
    //while not (self.Suspended) do Sleep(16);
    result := fResult;
    fResult := nil;
end;

end.

