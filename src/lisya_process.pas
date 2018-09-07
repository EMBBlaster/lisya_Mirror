unit lisya_process;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    {$IFDEF WINDOWS}
    LazUnicode,
    {$ENDIF}
    Classes, SysUtils, process,
    mar,
    lisia_charset;


type

    { TLProcess }

    TLProcess = class (TCountingObject)
    private
        p: TProcess;
        function GetInputPipe: TStream;
        function GetOutputpipe: TStream;
    public
        property In_pipe: TStream read GetInputPipe;
        property Out_pipe: TStream read GetOutputPipe;
        function ProcessID: integer;

        constructor Run(cmd: unicodestring; dir: unicodestring = '.');
        destructor Destroy; override;
        function description: unicodestring; override;

        procedure term;
        procedure CloseInput;
        procedure Wait;
    end;

implementation


function TLProcess.GetInputPipe: TStream;
begin
    result := P.Input;
end;

function TLProcess.GetOutputpipe: TStream;
begin
    result := P.Output;
end;

function TLProcess.ProcessID: integer;
begin
    result := p.ProcessID;
end;

constructor TLProcess.Run(cmd: unicodestring; dir: unicodestring);
begin
    inherited Create;
    p := TProcess.Create(nil);
    {$IFDEF WINDOWS}
    p.CommandLine:=UnicodeToWinCP(cmd);
    p.CurrentDirectory:=UnicodeToWinCP(DirSep(dir));
    {$ELSE}
    p.CommandLine:=cmd;;
    p.CurrentDirectory:=DirSep(dir);
    {$ENDIF};
    p.Options:=[poStderrToOutPut, poUsePipes];
    p.Execute;
end;

destructor TLProcess.Destroy;
begin
    P.Terminate(0);
    P.Free;
    inherited Destroy;
end;

function TLProcess.description: unicodestring;
begin
    if p=nil
    then result := 'nil'
    else result := {$IFDEF WINDOWS} WinCPtoUnicode(p.CommandLine){$ELSE} p.CommandLine{$ENDIF};
end;


procedure TLProcess.term;
begin
    P.Terminate(0);
    FreeAndNil(P);
end;

procedure TLProcess.CloseInput;
begin
    P.CloseInput;
end;

procedure TLProcess.Wait;
begin
    P.WaitOnExit;
end;


end.

