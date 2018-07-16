﻿unit lisya_thread;

{$mode delphi}

interface

uses
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

finalization
    set_threads_count(0);

end.

