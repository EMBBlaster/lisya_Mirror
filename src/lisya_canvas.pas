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

var
    CanvasForm: TCanvasForm;

implementation

{$R *.lfm}

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

