program lisya_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main
  { you can add units after this };

{$R *.res}

begin
    Application.Title:='Лися';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tmain_form, main_form);
  Application.Run;
end.

