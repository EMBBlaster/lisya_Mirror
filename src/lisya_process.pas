unit lisya_process;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
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

        constructor Run(cmd: unicodestring; dir: unicodestring = '.');
        destructor Destroy; override;
        function description: unicodestring; override;

        procedure term;
        procedure CloseInput;
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

constructor TLProcess.Run(cmd: unicodestring; dir: unicodestring);
begin
    inherited Create;
    p := TProcess.Create(nil);
    p.CommandLine:=cmd;
    p.CurrentDirectory:=DirSep(dir);
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
    else result := p.CommandLine;
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


end.

