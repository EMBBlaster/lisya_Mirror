unit lisya_canvas;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
    ,dlisp_values
    ;

type

    { TCanvasForm }

    TCanvasForm = class(TForm)
    private
        fW, fH: double;
        { private declarations }
    public
        procedure SetStructure(structure: TVRecord);
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

end.

