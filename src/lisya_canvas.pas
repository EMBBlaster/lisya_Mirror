unit lisya_canvas;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
    ,dlisp_values, lisya_predicates
    ;

type

    { TCanvasForm }

    TCanvasForm = class(TForm)
    private
        fW, fH: double;
        { private declarations }
    public
        procedure SetStructure(structure: TVRecord);
        procedure Post(msg: TVList);
        { public declarations }
    end;


    { TCanvasThread }

    TCanvasThread = class (TThread)
    private
        f1: TCanvasForm;
        fComplitedEvent: pRTLEvent;
    protected
        procedure Execute; override;
    public
        constructor Create;
        destructor Destroy; override;
        function WaitResult: TValue;
    end;


var
    CanvasForm: TCanvasForm;
    CanvasThread: TCanvasThread;

implementation

{$R *.lfm}

{ TCanvasThread }

procedure TCanvasThread.Execute;
begin
  while not self.Terminated do begin
        Application.ProcessMessages;
        sleep(0);
      //RtlEventSetEvent(fComplitedEvent);
      //self.Suspended := true;
  end;
end;

constructor TCanvasThread.Create;
begin
    self.FreeOnTerminate := true;
    fComplitedEvent := RTLEventCreate;
    Application.Initialize;
    f1 := TCanvasForm.Create(nil);
    f1.Show;

    inherited Create(false);
end;

destructor TCanvasThread.Destroy;
begin
    f1.Free;
    Application.Free;
    RTLeventdestroy(fComplitedEvent);
    self.Terminate;
    inherited Destroy;
end;

function TCanvasThread.WaitResult: TValue;
begin

end;

{ TCanvasForm }

procedure TCanvasForm.SetStructure(structure: TVRecord);
begin
    Caption := (structure.slot[TVSymbol.symbol_n('CAPTION')] as TVString).S;
end;

procedure TCanvasForm.Post(msg: TVList);
begin
    if (msg.Count=2) and vpKeyword_CAPTION(msg.look[0]) then begin
        caption := msg.S[1];
        Exit;
    end;

    if (msg.Count=2) and vpKeyword_WIDTH(msg.look[0]) then begin
        width := msg.I[1];
        Exit;
    end;
end;

end.

