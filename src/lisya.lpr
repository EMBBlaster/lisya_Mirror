﻿program lisya;


uses
    {$IFDEF LINUX}
    cthreads,
    cwstring,
    {$ENDIF}
    {$IFDEF GUI}
    Interfaces, Forms, lisya_canvas,
    {$ENDIF}
    sysutils,
    LResources,
    lisya_repl,
    dlisp_eval, lisya_zip;

var i: integer;

begin
    {$if declared(UseHeapTrace)}
    if FileExists('lisya.trc') then DeleteFile('lisya.trc');
    SetHeaptraceOutput('lisya.trc');
    {$ifend}

    {$IFDEF GUI}
    RequireDerivedFormResource := True;
    {$ENDIF}

    {$i ascii.lrs}

//    WriteLn(LazarusResources.Find('ascii').Value);
    for i:=0 to lazarusResources.Count-1 do WriteLn(lazarusResources.Items[i].Name);

    if (ParamCount=0) or not EXEC(paramStr(1)) then REPL;


end.


///your/lazarus/path/tools/lazres sound.lrs sound1.wav sound2.wav ...
//
//создаст sound.lrs из sound1.wav и sound2.wav.
//
//Потом включите его *после* lrs-файла формы:
//
//...
//initialization
//{$i unit1.lrs} // this is main resource file (first)
//{$i sound.lrs} // user defined resource file
//
//end.
//
//В Вашей программе эти ресурсы можно использовать следующим образом:
//
//Sound1AsString:=LazarusResources.Find('sound1').Value;

