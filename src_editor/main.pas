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
  Classes, process, SysUtils, SynEditTypes, Controls, LCLType, ComCtrls

  {$IFDEF WINDOWS}, LazUnicode {$ENDIF}
  ;

type

  { Tmain_form }

  Tmain_form = class(TForm)
      Action_SetMain: TAction;
      Action_CloseTab: TAction;
      Action_SaveAll: TAction;
      Action_NEW: TAction;
      Action_SearchBack: TAction;
      Action_search: TAction;
      Action_execute: TAction;
      Action_quick_save: TAction;
      Action_save_as: TAction;
      Action_open: TAction;
      ActionList1: TActionList;
      Edit_search: TEdit;
      OpenDialog: TOpenDialog;
      SynEdit2: TSynEdit;
      tabs: TPageControl;
      Panel: TPanel;
      SaveDialog1: TSaveDialog;
    SynEdit1: TSynEdit;
    procedure Action_CloseTabExecute(Sender: TObject);
    procedure Action_executeExecute(Sender: TObject);
    procedure Action_NEWExecute(Sender: TObject);
    procedure Action_openExecute(Sender: TObject);
    procedure Action_quick_saveExecute(Sender: TObject);
    procedure Action_SaveAllExecute(Sender: TObject);
    procedure Action_save_asExecute(Sender: TObject);
    procedure Action_SearchBackExecute(Sender: TObject);
    procedure Action_searchExecute(Sender: TObject);
    procedure Action_SetMainExecute(Sender: TObject);
    procedure Edit_searchKeyPress(Sender: TObject; var Key: char);
    procedure Edit_searchKeyUp(Sender: TObject; var Key: Word;
        Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure Search(back: boolean = false);
    procedure tabsCloseTabClicked(Sender: TObject);
  private
    function confirm_saving: boolean;
    function save_as(ts: TTabSheet=nil): boolean;
    function quick_save(ts: TTabSheet=nil): boolean;
    function save_all(): boolean;
    procedure close_tab(tab: TTabSheet);

  public

  end;

var
  main_form: Tmain_form;

implementation

{$R *.lfm}

{ Tmain_form }

procedure Tmain_form.FormCreate(Sender: TObject);
var i: integer;
begin
    SynEdit1.Highlighter := TSynLisya.Create(Self);
    for i := 1 to paramCount do begin
        OpenDialog.Filename := paramStr(1);
        Action_NEWExecute(OpenDialog);
    end;

    if paramCount>0
    then Caption := ExtractFileDir(paramStr(1))
    else Caption := ExpandFileName('.');

    if paramCount=0 then Action_NewExecute(nil);
end;

procedure Tmain_form.SynEditChange(Sender: TObject);
var tab: TTabSheet;
begin
    tab := (sender as TSynEdit).Parent as TTabSheet;
    if tab.Caption[Length(tab.Caption)]<>'*'
    then tab.Caption:=tab.Caption+' *';
end;

function GetSaveDialog(ts: TTabSheet): TSaveDialog;
var i: integer;
begin
    result := nil;
    for i := 0 to ts.ComponentCount-1 do
        if ts.Components[i] is TSaveDialog then begin
            result := ts.Components[i] as TSaveDialog;
            Exit;
        end;
end;

function GetSynEdit(ts: TTabSheet): TSynEdit;
var i: integer;
begin
    result := nil;
    for i := 0 to ts.ComponentCount-1 do
        if ts.Components[i] is TSynEdit then begin
            result := ts.Components[i] as TSynEdit;
            Exit;
        end;
end;

procedure Tmain_form.Search(back: boolean);
begin
    if tabs.PageCount=0 then Exit;
    if not panel.Visible then begin
        panel.Visible:=true;
        edit_search.SetFocus;
    end
    else begin
        if back
        then GetSynEdit(tabs.ActivePage).SearchReplace(edit_search.Text, '', [ssoFindContinue, ssoBackwards])
        else GetSynEdit(tabs.ActivePage).SearchReplace(edit_search.Text, '', [ssoFindContinue]);
    end;
end;

procedure Tmain_form.tabsCloseTabClicked(Sender: TObject);
begin
    close_tab(sender as TTabSheet);
end;

function Tmain_form.confirm_saving: boolean;
var i: integer; unsaved: boolean;
begin
    unsaved := false;
    for i:=0 to tabs.PageCount-1 do unsaved := unsaved or (tabs.Pages[i].Caption[Length(tabs.Pages[i].Caption)]='*');

    if unsaved then begin
        case MessageDLG('Сохранить изменения?','Сохранить изменения?',
            mtConfirmation,[mbYes,mbNo,mbCancel],0) of
            mrYes: result := save_all();
            mrNo: result := true;
            mrCancel: result := false;
        end;
    end
    else result := true;
end;



function Tmain_form.save_as(ts: TTabSheet): boolean;
var tab: TTabSheet;
begin
    if tabs.PageCount=0 then Exit;
    if ts=nil then tab := tabs.ActivePage else tab := ts;
    if GetSaveDialog(tab).Execute
    then result := quick_save
    else result := false;
end;


function Tmain_form.quick_save(ts: TTabSheet): boolean;
var sd: TSaveDialog; tab: TTabSheet;
begin
    if tabs.PageCount=0 then Exit;
    if ts=nil then tab := tabs.ActivePage else tab := ts;
    sd := GetSaveDialog(tab);

    if sd.FileName<>'' then begin
        GetSynEdit(tab).Lines.SaveToFile(sd.FileName);
        tab.Caption:=ExtractFileName(sd.FileName);
        if tab.Tag=1 then tab.Caption:=#$25b6+' '+tab.Caption;
        result := true;
    end
    else result := save_as();
end;

function Tmain_form.save_all: boolean;
var i: integer;
begin
    result := true;
    for i := 0 to tabs.PageCount-1 do result := result and quick_save(tabs.Pages[i]);
end;

procedure Tmain_form.close_tab(tab: TTabSheet);
var can_close: boolean;
begin
    if tab.Caption[Length(tab.Caption)]='*' then begin
        case MessageDLG('Сохранить изменения?','Сохранить изменения?',
            mtConfirmation,[mbYes,mbNo,mbCancel],0) of
            mrYes: can_close := quick_save(tab);
            mrNo: can_close := true;
            mrCancel: can_close := false;
        end;
    end
    else can_close := true;

    if can_close then tab.Free;
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
    if OpenDialog.Execute then Action_NEWExecute(OpenDialog);
end;

procedure Tmain_form.Action_executeExecute(Sender: TObject);
var p: TProcess; sd: TSaveDialog; i: integer; tab: TTabSheet;
begin
    if tabs.PageCount=0 then Exit;
    save_all;
    tab := nil;
    for i := 0 to tabs.PageCount-1 do if tabs.Pages[i].Tag=1 then begin
        tab := tabs.Pages[i];
        Break;
    end;
    if tab=nil then tab := tabs.ActivePage;
    sd := GetSaveDialog(tab);

    p := TProcess.Create(nil);
    p.CommandLine:=
    {$IFDEF WINDOWS}
        UnicodeToWinCP(ExtractFilePath(Application.ExeName)+'lisya.exe'
            +' "'+SaveDialog1.FileName+'"');
    {$ELSE}
        ExtractFilePath(Application.ExeName)+'lisya'+' "'+sd.FileName+'"';
    {$ENDIF}
    p.CurrentDirectory:=ExtractFilePath(sd.FileName);
    p.Options:=[poNewConsole,poNewProcessGroup];

    p.Execute;
    p.Free;
end;

procedure Tmain_form.Action_CloseTabExecute(Sender: TObject);
begin
    close_tab(tabs.ActivePage);
end;

procedure Tmain_form.Action_NEWExecute(Sender: TObject);
var ts: TTabSheet; se: TSynEdit;  sd: TSaveDialog;
begin
    ts := tabs.AddTabSheet;
    ts.Tag:=0;
    if sender=OpenDialog
    then ts.Caption:=ExtractFileName(OpenDialog.FileName)
    else ts.Caption:='Новый';

    se := TSynEdit.Create(ts);
    se.Parent := ts;
    se.Align:= alClient;
    se.Highlighter := TSynLisya.Create(se);
    se.Options:=[eoAutoIndent,eoBracketHighlight,eoEnhanceHomeKey,eoGroupUndo,eoScrollPastEol,eoTabsToSpaces,eoTrimTrailingSpaces];
    se.Options2:=[eoEnhanceEndKey,eoFoldedCopyPaste,eoOverwriteBlock];
    se.TabWidth:=4;
    se.OnChange:=@SynEditChange;
    se.Keystrokes.Delete(45); //освободить Ctrl + N
    if Sender=OpenDialog
    then se.Lines.LoadFromFile(OpenDialog.FileName);

    sd := TSaveDialog.Create(ts);
    sd.DefaultExt:='.*.лися';
    sd.Filter:='Лися|*.lisya|*.лися|*|*.*';
    sd.FilterIndex:=1;
    sd.InitialDir:='.';
    if sender=OpenDialog
    then sd.FileName:=OpenDialog.FileName
    else sd.FileName:='';

    tabs.ActivePage := ts;
    try se.SetFocus; except end; //костыль нужен для подавлени ошибки при открытии файла из конструктора формы
end;

procedure Tmain_form.Action_quick_saveExecute(Sender: TObject);
begin
    quick_save;
end;

procedure Tmain_form.Action_SaveAllExecute(Sender: TObject);
begin
    save_all;
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

procedure Tmain_form.Action_SetMainExecute(Sender: TObject);
var i: integer;
begin
    if tabs.ActivePage.Tag=1
    then begin
        tabs.ActivePage.Tag := 0;
        tabs.ActivePage.Caption := tabs.ActivePage.Caption[5..Length(tabs.ActivePage.Caption)];
    end
    else begin
        for i := 0 to tabs.PageCount-1 do begin
            if tabs.pages[i].Tag=1 then tabs.pages[i].Caption := tabs.pages[i].Caption[5..Length(tabs.pages[i].Caption)];
            tabs.Pages[i].Tag:=0;
        end;
        tabs.ActivePage.Tag:=1;
        tabs.ActivePage.Caption:=#$25b6+' '+tabs.ActivePage.Caption;
    end;
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

