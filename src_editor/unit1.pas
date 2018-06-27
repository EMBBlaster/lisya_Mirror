unit Unit1;
(*

This is an example how to write a highlighter from scratch.

    See the units for each example highlighter.

This is NOT about extending the IDE. This is about SynEdit and it's Highlighter only.
Therefore this does not include:
- registration in the component pallette.
- Using the Object Inspector
Those steps are the same as they would be for any other self writen componont.

*)
{$mode objfpc}{$H+}

interface

uses
  SynEdit, Forms, StdCtrls, ActnList, Dialogs, ExtCtrls, SimpleHl, ContextHL,
  FoldHl, Classes, process, SysUtils, SynEditTypes, Controls, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
      Action_NEW: TAction;
      Action_SearchBack: TAction;
      Action_search: TAction;
      Action_execute: TAction;
      Action_quick_save: TAction;
      Action_save_as: TAction;
      Action_open: TAction;
      ActionList1: TActionList;
      Edit_search: TEdit;
      OpenDialog1: TOpenDialog;
      Panel: TPanel;
      SaveDialog1: TSaveDialog;
    SynEdit1: TSynEdit;
    procedure Action_executeExecute(Sender: TObject);
    procedure Action_NEWExecute(Sender: TObject);
    procedure Action_openExecute(Sender: TObject);
    procedure Action_quick_saveExecute(Sender: TObject);
    procedure Action_save_asExecute(Sender: TObject);
    procedure Action_SearchBackExecute(Sender: TObject);
    procedure Action_searchExecute(Sender: TObject);
    procedure Edit_searchKeyPress(Sender: TObject; var Key: char);
    procedure Edit_searchKeyUp(Sender: TObject; var Key: Word;
        Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure Search(back: boolean = false);
  private
    FSynDemoHl: TSynDemoHl;
    FSynDemoHlContext: TSynDemoHlContext;
    FSynDemoHlFold: TSynDemoHlFold;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSynDemoHl := TSynDemoHl.Create(Self);
  FSynDemoHlContext := TSynDemoHlContext.Create(Self);
  FSynDemoHlFold := TSynDemoHlFold.Create(Self);
  SynEdit1.Highlighter := FSynDemoHl;
  if paramCount>0 then begin
        OpenDialog1.Filename := paramStr(1);
        SaveDialog1.FileName := paramStr(1);
        Caption := paramStr(1);
        SynEdit1.Lines.LoadFromFile(paramStr(1));
  end;
end;

procedure TForm1.SynEdit1Change(Sender: TObject);
begin
    if Form1.Caption[Length(Form1.Caption)]<>'*'
    then Form1.Caption:=Form1.Caption+' *';
end;

procedure TForm1.Search(back: boolean);
begin
    if not panel.Visible then begin
        panel.Visible:=true;
        edit_search.SetFocus;
    end
    else begin
        if back
        then SynEdit1.SearchReplace(edit_search.Text, '', [ssoFindContinue, ssoBackwards])
        else SynEdit1.SearchReplace(edit_search.Text, '', [ssoFindContinue]);
    end;
end;



procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := true;
    if Form1.Caption[Length(Form1.Caption)]='*' then begin
        case MessageDLG('Сохранить изменения?','Сохранить изменения в файле '+SaveDialog1.FileName+'?',
            mtConfirmation,[mbYes,mbNo,mbCancel],0) of
            mrYes: Action_quick_save.Execute;
            mrNo:;
            mrCancel: CanClose := false;
        end;
    end;
end;

procedure TForm1.Action_openExecute(Sender: TObject);
begin
    if OpenDialog1.Execute then begin
        SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
        Form1.Caption := OpenDialog1.FileName;
        SaveDialog1.FileName:=OpenDialog1.FileName;
    end;
end;

procedure TForm1.Action_executeExecute(Sender: TObject);
var p: TProcess;
begin
    Action_quick_save.Execute;

    p := TProcess.Create(nil);
    p.CommandLine:=
        ExtractFilePath(Application.ExeName)+{$IFDEF WINDOWS}'lisya.exe'{$ELSE}'lisya'{$ENDIF}
        +' "'+SaveDialog1.FileName+'"';
    p.CurrentDirectory:=ExtractFilePath(SaveDialog1.FileName);
    p.Options:=[poNewConsole,poNewProcessGroup];

    p.Execute;
    p.Free;
end;

procedure TForm1.Action_NEWExecute(Sender: TObject);
begin
    Action_quick_save.Execute;
    SaveDialog1.FileName:='';
    OpenDialog1.FileName:='';
    Caption := 'Новый';
    SynEdit1.Lines.Clear;
end;

procedure TForm1.Action_quick_saveExecute(Sender: TObject);
begin
    if SaveDialog1.FileName<>'' then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        Form1.Caption:=SaveDialog1.Filename;
    end
    else Action_save_as.Execute;
end;

procedure TForm1.Action_save_asExecute(Sender: TObject);
begin
    If SaveDialog1.Execute then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        OpenDialog1.FileName:=saveDialog1.FileName;
        Form1.Caption:=SaveDialog1.Filename;
    end;
end;

procedure TForm1.Action_SearchBackExecute(Sender: TObject);
begin
    Search(true);
end;

procedure TForm1.Action_searchExecute(Sender: TObject);
begin
    Search(false);
end;

procedure TForm1.Edit_searchKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TForm1.Edit_searchKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    case key of
        VK_ESCAPE: begin
            Edit_Search.Text:='';
            panel.Visible:=false;
        end;
        VK_RETURN: Action_Search.Execute;
    end;
end;

end.

