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
        fExpression: TVList;
        fComplitedEvent: pRTLEvent;
        eclass, emessage, estack: unicodestring;
        fA, fB: integer;
        fError: boolean;
    protected
        procedure Execute; override;
    public
        fResult: TVList;
        constructor Create(CreateSuspended: boolean);
        destructor Destroy; override;
        procedure eval_map(PL: TVList; b, e: integer);
        function map_subseq(): TVList;
        function WaitResult: TVList;
    end;


implementation

{ TEvaluationThread }

procedure TEvaluationThread.Execute;
begin
    while not self.Terminated do begin
        try
            fResult := map_subseq;//fFlow.eval(fExpression);
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
                raise;
            end;


        end;
        RtlEventSetEvent(fComplitedEvent);
        self.Suspend;
    end;
end;

constructor TEvaluationThread.Create(CreateSuspended: boolean);
begin
    inherited Create(CreateSuspended);
    self.fExpression := nil;
    self.fflow := TEvaluationFlow.Create(nil);
    self.fResult := nil;
    fComplitedEvent := RTLEventCreate;
end;


destructor TEvaluationThread.Destroy;
begin
    fflow.Free;
    fResult.Free;
    //fExpression.Free;
    RTLeventdestroy(fComplitedEvent);
    self.Terminate;
    self.Resume;
    inherited;
end;


procedure TEvaluationThread.eval_map(PL: TVList; b, e: integer);
begin
    if fResult<>nil then raise ELE.Create('поток занят');

    fExpression := PL;
    fA := b;
    fB := e;
    RTLEventResetEvent(fComplitedEvent);
    self.Resume;
end;

function TEvaluationThread.map_subseq(): TVList;
var expr: TVList;
    i, j: integer;
begin try try
    result := TVList.Create;
    //expr := fExpression.Phantom_Copy;
    expr := fExpression.Copy as TVList;
    for i := fA to fB do begin
        //for j := 1 to fExpression.high do expr[j] := fExpression.L[j].look[i];
        for j := 1 to fExpression.high do expr[j] := fExpression.L[j][i];
        (result as TVList).Add(fflow.call(expr));
    end;
except
    FreeAndNil(result);
    raise;
end;
finally
    expr.Free;
end;
end;

function TEvaluationThread.WaitResult: TVList;
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

