unit main;
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
  SynEdit, Forms, StdCtrls, ActnList, Dialogs, ExtCtrls, lisya_highlighter,
  Classes, process, SysUtils, SynEditTypes, Controls, LCLType;

type

  { Tmain_form }

  Tmain_form = class(TForm)
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

  public

  end;

var
  main_form: Tmain_form;

implementation

{$R *.lfm}

{ Tmain_form }

procedure Tmain_form.FormCreate(Sender: TObject);
begin
  SynEdit1.Highlighter := TSynDemoHl.Create(Self);
  if paramCount>0 then begin
        OpenDialog1.Filename := paramStr(1);
        SaveDialog1.FileName := paramStr(1);
        Caption := paramStr(1);
        SynEdit1.Lines.LoadFromFile(paramStr(1));
  end;
end;

procedure Tmain_form.SynEdit1Change(Sender: TObject);
begin
    if main_form.Caption[Length(main_form.Caption)]<>'*'
    then main_form.Caption:=main_form.Caption+' *';
end;

procedure Tmain_form.Search(back: boolean);
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



procedure Tmain_form.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure Tmain_form.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := true;
    if main_form.Caption[Length(main_form.Caption)]='*' then begin
        case MessageDLG('Сохранить изменения?','Сохранить изменения в файле '+SaveDialog1.FileName+'?',
            mtConfirmation,[mbYes,mbNo,mbCancel],0) of
            mrYes: Action_quick_save.Execute;
            mrNo:;
            mrCancel: CanClose := false;
        end;
    end;
end;

procedure Tmain_form.Action_openExecute(Sender: TObject);
begin
    if OpenDialog1.Execute then begin
        SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
        main_form.Caption := OpenDialog1.FileName;
        SaveDialog1.FileName:=OpenDialog1.FileName;
    end;
end;

procedure Tmain_form.Action_executeExecute(Sender: TObject);
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

procedure Tmain_form.Action_NEWExecute(Sender: TObject);
begin
    Action_quick_save.Execute;
    SaveDialog1.FileName:='';
    OpenDialog1.FileName:='';
    Caption := 'Новый';
    SynEdit1.Lines.Clear;
end;

procedure Tmain_form.Action_quick_saveExecute(Sender: TObject);
begin
    if SaveDialog1.FileName<>'' then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        main_form.Caption:=SaveDialog1.Filename;
    end
    else Action_save_as.Execute;
end;

procedure Tmain_form.Action_save_asExecute(Sender: TObject);
begin
    If SaveDialog1.Execute then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        OpenDialog1.FileName:=saveDialog1.FileName;
        main_form.Caption:=SaveDialog1.Filename;
    end;
end;

procedure Tmain_form.Action_SearchBackExecute(Sender: TObject);
begin
    Search(true);
end;

procedure Tmain_form.Action_searchExecute(Sender: TObject);
begin
    Search(false);
end;

procedure Tmain_form.Edit_searchKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure Tmain_form.Edit_searchKeyUp(Sender: TObject; var Key: Word;
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

