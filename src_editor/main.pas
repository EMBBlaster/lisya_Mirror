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
  Classes, process, SysUtils, SynEditTypes, Controls, LCLType

  {$IFDEF WINDOWS}, LazUnicode {$ENDIF}
  ;

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
    function confirm_saving: boolean;
    procedure Set_filename(fn: unicodestring);
    function save_as(): boolean;
    function quick_save(): boolean;

  public

  end;

var
  main_form: Tmain_form;

implementation

{$R *.lfm}

{ Tmain_form }

procedure Tmain_form.FormCreate(Sender: TObject);
begin
  SynEdit1.Highlighter := TSynLisya.Create(Self);
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

function Tmain_form.confirm_saving: boolean;
begin
    if main_form.Caption[Length(main_form.Caption)]='*' then begin
        case MessageDLG('Сохранить изменения?','Сохранить изменения в файле '+SaveDialog1.FileName+'?',
            mtConfirmation,[mbYes,mbNo,mbCancel],0) of
            mrYes: result := quick_save();
            mrNo: result := true;
            mrCancel: result := false;
        end;
    end
    else result := true;
end;

procedure Tmain_form.Set_filename(fn: unicodestring);
begin
    OpenDialog1.FileName:=fn;
    SaveDialog1.FileName:=fn;
    if fn<>''
    then begin
        Caption := fn;
        Application.Title:=ExtractFileName(fn);
    end
    else begin
        Caption := 'Новый';
        Application.Title:='Лися';
    end;
end;

function Tmain_form.save_as: boolean;
begin
    If SaveDialog1.Execute then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        set_filename(SaveDialog1.FileName);
        result := true;
    end
    else result := false;
end;

function Tmain_form.quick_save: boolean;
begin
    if SaveDialog1.FileName<>'' then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        set_filename(SaveDialog1.FileName);
        result := true;
    end
    else result := save_as();
end;



procedure Tmain_form.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure Tmain_form.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := confirm_saving;
end;

procedure Tmain_form.Action_openExecute(Sender: TObject);
begin
    if not Confirm_saving() then Exit;
    if OpenDialog1.Execute then begin
        SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
        set_filename(OpenDialog1.FileName)
    end;
end;

procedure Tmain_form.Action_executeExecute(Sender: TObject);
var p: TProcess;
begin
    quick_save;

    p := TProcess.Create(nil);
    p.CommandLine:=
    {$IFDEF WINDOWS}
        UnicodeToWinCP(ExtractFilePath(Application.ExeName)+'lisya.exe'
            +' "'+SaveDialog1.FileName+'"');
    {$ELSE}
        ExtractFilePath(Application.ExeName)+'lisya'+' "'+SaveDialog1.FileName+'"';
    {$ENDIF}
    p.CurrentDirectory:=ExtractFilePath(SaveDialog1.FileName);
    p.Options:=[poNewConsole,poNewProcessGroup];

    p.Execute;
    p.Free;
end;

procedure Tmain_form.Action_NEWExecute(Sender: TObject);
begin
    if not confirm_saving() then exit;
    quick_save;
    set_filename('');
    SynEdit1.Lines.Clear;
end;

procedure Tmain_form.Action_quick_saveExecute(Sender: TObject);
begin
    quick_save;
end;

procedure Tmain_form.Action_save_asExecute(Sender: TObject);
begin
    save_as;
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

