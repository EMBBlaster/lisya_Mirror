unit main;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF LINUX}
    cwstring,
    {$ENDIF}
    {$IFDEF WINDOWS}
    Windows,
    {$ENDIF}
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls, ExtDlgs, ActnList, pipes, Contnrs, math;

type

    TLCanvasException = exception;
    TPointFloat = record x,y: real; end;
    TMatrix = array[1..3,1..3] of real;

    { TGO }

    TGO = class
        fcolor: TColor;
        ftransform: TMatrix;
        procedure paint; virtual; abstract;
    end;


    { TGAxis }

    TGAxis = class (TGO)
        step: real;
    end;

    { TGAxisX }

    TGAxisX = class (TGAxis)
        procedure paint; override;
    end;

    { TGAxisY }

    TGAxisY = class (TGAxis)
        procedure paint; override;
    end;

    { TGGrid }

    TGGrid = class (TGO)
        stepX, stepY: real;
        procedure paint; override;
    end;

    { TGMark }

    TGMark = class (TGO)
        fp: TPointFloat;
        text: unicodestring;
        procedure paint; override;
    end;

    { TGPoint }

    TGPoint = class (TGO)
        x, y: real;
        procedure paint; override;
    end;

    { TGPolyline }

    TGPolyline = class (TGO)
        points: array of TPointFloat;
        procedure paint; override;
    end;

    TGPlot = class (TGPolyline)
    end;

    { TForm1 }

    TForm1 = class(TForm)
        Action_SaveAs: TAction;
        ActionList1: TActionList;
        SavePictureDialog1: TSavePictureDialog;
        Timer1: TTimer;
        objects: TObjectList;
        cmdl: TStringList;
        Timer2: TTimer;
        procedure Action_SaveAsExecute(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure Timer2Timer(Sender: TObject);
    private
        procedure on_command;
        procedure cmd_size;
        procedure cmd_area;
        procedure cmd_axis;
        procedure cmd_circle;
        procedure cmd_color;
        procedure cmd_grid;
        procedure cmd_mark;
        procedure cmd_plot;
        procedure cmd_point;
        procedure cmd_polyline;
        procedure cmd_translate;
        procedure cmd_rotate;
        procedure cmd_scale;
        { private declarations }
    public
        { public declarations }
    end;

const
    identity: TMatrix = ((1,0,0),(0,1,0),(0,0,1));

var
    Form1: TForm1;
    stdin: TInputPipeStream;
    cmd: unicodestring;


    lo_x: real = -1;
    hi_x: real = 1;
    a_x: real = 2;
    lo_y: real = -1;
    hi_y: real = 1;
    a_y: real = 2;
    cw,ch: integer;
    frame: integer;
    cnv: TCanvas;
    equal_scale: boolean = true;
    color: TColor = clBlack;
    transform: TMatrix = ((1,0,0),(0,1,0),(0,0,1));
    tm: TMatrix = ((1,0,0),(0,1,0),(0,0,1));


implementation


{$R *.lfm}

procedure error(const msg: unicodestring);
begin
    raise TLCanvasException.Create(msg);
end;

function StrToFloat(s: string): real;
var fs: TFormatSettings;
begin
    fs.DecimalSeparator:='.';
    if not TryStrToFloat(s, result, fs) then begin
        fs.DecimalSeparator:=',';
        result := SysUtils.StrToFloat(s, fs);
    end;
end;

function mul(const p: TPointFloat; const m: TMatrix): TPointFloat; overload;
begin
    result.x := p.x*m[1,1]+p.y*m[2,1]+1*m[3,1];
    result.y := p.x*m[1,2]+p.y*m[2,2]+1*m[3,2];
end;

function mul(const m1: TMatrix; const m2: TMatrix): TMatrix; overload;
begin
    result[1,1] := m1[1,1]*m2[1,1]+m1[1,2]*m2[2,1]+m1[1,3]*m2[3,1];
    result[1,2] := m1[1,1]*m2[1,2]+m1[1,2]*m2[2,2]+m1[1,3]*m2[3,2];
    result[1,3] := m1[1,1]*m2[1,3]+m1[1,2]*m2[2,3]+m1[1,3]*m2[3,3];

    result[2,1] := m1[2,1]*m2[1,1]+m1[2,2]*m2[2,1]+m1[2,3]*m2[3,1];
    result[2,2] := m1[2,1]*m2[1,2]+m1[2,2]*m2[2,2]+m1[2,3]*m2[3,2];
    result[2,3] := m1[2,1]*m2[1,3]+m1[2,2]*m2[2,3]+m1[2,3]*m2[3,3];

    result[3,1] := m1[3,1]*m2[1,1]+m1[3,2]*m2[2,1]+m1[3,3]*m2[3,1];
    result[3,2] := m1[3,1]*m2[1,2]+m1[3,2]*m2[2,2]+m1[3,3]*m2[3,2];
    result[3,3] := m1[3,1]*m2[1,3]+m1[3,2]*m2[2,3]+m1[3,3]*m2[3,3];
end;

function translate(const v: TPointFloat): TMatrix;
begin
    result := identity;
    result[1,3]:=v.x;
    result[2,3]:=v.y;
end;

procedure set_area(lx,hx,ly,hy: real);
begin
    lo_x := lx;
    hi_x := hx;
    lo_y := ly;
    hi_y := hy;
    a_x := hi_x - lo_x;
    a_y := hi_y - lo_y;
    cw := Form1.ClientWidth;
    ch := Form1.ClientHeight;

    frame := min(cw, ch);
end;

function scaleX(x: real): integer; inline;
var wx: real;
begin
    if equal_scale
    then wx := cw/2 - frame/2 + frame*(x - lo_x)/a_x
    else wx := cw*(x - lo_x)/a_x;
    result := Round(wx);
end;

function scaleY(y: real): integer; inline;
var wy: real;
begin
    if equal_scale
    then wy := ch/2 + frame/2 - frame*(y - lo_y)/a_y
    else wy := ch - ch*(y - lo_y)/a_y;
    result := Round(wy);
end;

function scale(x, y: real): TPoint;  overload;
var wx,wy: real;
begin
    if equal_scale
    then begin
        wx := cw/2 - frame/2 + frame*(x - lo_x)/a_x;
        wy := ch/2 + frame/2 - frame*(y - lo_y)/a_y;
    end
    else begin
        wx := cw*(x - lo_x)/a_x;
        wy := ch - ch*(y - lo_y)/a_y;
    end;
    result := classes.point(Round(wx), Round(wy));
end;

function scale(_p: TPointFloat): TPoint;  overload;
var wx,wy: real; p: TPointFloat;
begin
    p := mul(_p,tm);
    if equal_scale
    then begin
        wx := cw/2 - frame/2 + frame*(p.x - lo_x)/a_x;
        wy := ch/2 + frame/2 - frame*(p.y - lo_y)/a_y;
    end
    else begin
        wx := cw*(p.x - lo_x)/a_x;
        wy := ch - ch*(p.y - lo_y)/a_y;
    end;
    result := classes.point(Round(wx), Round(wy));
end;


{ TGGrid }

procedure TGGrid.paint;
var i, px, lx, hx, py, ly, hy: integer;
begin
    ly := scaleY(lo_y);
    hy := scaleY(hi_y);
    for i := round(lo_x/stepX)+1 to round(hi_x/stepX)-1 do begin
        px := scaleX(i*stepX);
        cnv.Line(px,ly,px,hy);
    end;
    lx := scaleX(lo_x);
    hx := scaleX(hi_x);
    for i := round(lo_y/stepY)+1 to round(hi_y/stepY)-1 do begin
        py := scaleY(i*stepY);
        cnv.Line(lx,py,hx,py);
    end;
end;

{ TGMark }

procedure TGMark.paint;
var p: TPoint;
begin
    p := scale(fp);
    cnv.Line(p.x-5,p.y-5,p.x+5, p.y+5);
    cnv.Line(p.x-5,p.y+5,p.x+5, p.y-5);
    cnv.TextOut(p.x+6,p.y-5,text);
end;

{ TGAxisY }

procedure TGAxisY.paint;
var h_pos: real; p: TPoint; py, i: integer; txt: string;
begin
    h_pos := 0;
    if lo_x>0 then h_pos := lo_x;
    if hi_x<0 then h_pos := hi_x;

    p := scale(h_pos, hi_y);
    p.y := p.y+1;
    cnv.Line(scale(h_pos, lo_y),p);
    cnv.Line(p.X,p.Y,p.X+3,p.Y+5);
    cnv.Line(p.X,p.Y,p.X-3,p.Y+5);

    for i := round(lo_y/step)+1 to round(hi_y/step)-1 do begin
        py := scaleY(i*step);
        cnv.Line(p.X+3, py, p.X, py);
        txt := FloatToStr(i*step);
        cnv.TextOut(p.x-cnv.TextWidth(txt)-1,py+1,txt);
    end;
end;

{ TGAxisX }

procedure TGAxisX.paint;
var v_pos: real; p: TPoint; px, i: integer; txt: string;
begin
    v_pos := 0;
    if lo_y>0 then v_pos := lo_y;
    if hi_y<0 then v_pos := hi_y;

    p := scale(hi_x, v_pos);
    p.x := p.x-1;
    cnv.Line(scale(lo_x,v_pos),p);
    cnv.Line(p.X,p.Y,p.X-5,p.Y+3);
    cnv.Line(p.X,p.Y,p.X-5,p.Y-3);

    for i := round(lo_x/step)+1 to round(hi_x/step)-1 do begin
        px := scaleX(i*step);
        cnv.Line(px,p.Y-3,px,p.Y);
        txt := FloatToStr(i*step);
        cnv.TextOut(px - cnv.TextWidth(txt)-1,p.y+1,txt);
    end;
end;



{ TGPolyline }

procedure TGPolyline.paint;
var i: integer;
begin
    cnv.MoveTo(scale(points[0]));
    for i := 1 to high(points) do cnv.LineTo(scale(points[i]));
end;

{ TGPoint }

procedure TGPoint.paint;
var p: TPoint;
begin
    p := scale(x,y);
    cnv.Pixels[p.X, p.Y] := fcolor;
end;

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
var b1, b2: byte; ch: unicodechar;
begin
    Timer1.Enabled:=false;
    while stdin.NumBytesAvailable>1 do begin
        b1 := stdin.ReadByte;
        b2 := stdin.ReadByte;
        ch := unicodechar(b1+256*b2);
        case ch of
            #10,#13: on_command;
            else cmd := cmd + ch;
        end;
    end;
    Timer1.Enabled:=true;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  timer2.Enabled:=false;
  cmd := 'grid 0.5 0.5';
  on_command;
  cmd := 'translate 0.2 0.2';
    on_command;

    cmd:='mark x 0.1 0.1 hello';
    on_command;
end;

procedure TForm1.on_command;
var cnt, i: integer;
begin try try
    cnt := objects.Count;
    cmdl.DelimitedText:= cmd;
    if cmdl.Count=0 then Exit;

    //ShowMessage(cmdl[0]);

    if cmdl[0]='area' then cmd_area else                          // min_x max_x min_y max_y
    if cmdl[0]='axis' then cmd_axis else                          // axis_name step
    if cmdl[0]='caption' then caption := cmdl[1] else             // caption
    if cmdl[0]='clear' then objects.Clear else                    //
    if cmdl[0]='color' then cmd_color else                        // color
    if cmdl[0]='equal_scale' then equal_scale := true else        //
    if cmdl[0]='grid' then cmd_grid else                          // stepx stepy
    if cmdl[0]='independent_scale' then equal_scale := false else //
    if cmdl[0]='mark' then cmd_mark else                          // symbol x y text
    if cmdl[0]='plot' then cmd_plot else                          // yaxis xstart xstep y1 y2 ...
    if cmdl[0]='point' then cmd_point else                        // x y
    if cmdl[0]='polyline' then cmd_polyline else                  // p1x p1y p2x p2y ...
    if cmdl[0]='reset_transform' then transform := identity else  //
    if cmdl[0]='rotate' then cmd_rotate else                      // angle
    if cmdl[0]='scale' then cmd_scale else                        // sx sy
    if cmdl[0]='size' then cmd_size;                              // width height
    if cmdl[0]='translate' then cmd_translate;                    // dx dy

    for i := cnt to objects.Count-1 do begin
        (objects[i] as TGO).fcolor:=color;
        (objects[i] as TGO).ftransform:=transform;
    end;

finally
    cmd := '';
    paint;
end;
except
    on E:Exception do
        MessageDlg(E.className, E.Message+' in '+LineEnding+cmd, mtError, [mbOk], 0);
end;
end;

procedure TForm1.cmd_size;
begin
    Form1.ClientWidth:=StrToInt(cmdl[1]);
    Form1.ClientHeight:= StrToInt(cmdl[2]);;
end;

procedure TForm1.cmd_area;
begin
    set_area(
        StrToFloat(cmdl[1]),
        StrToFloat(cmdl[2]),
        StrToFloat(cmdl[3]),
        StrToFloat(cmdl[4]));
end;

procedure TForm1.cmd_axis;
var axis: TGAxis;
begin
    if cmdl[1]='x' then axis := TGAxisX.Create else
    if cmdl[1]='y' then axis := TGAxisY.Create else
    error('invalid axis '+cmdl[1]);

    axis.step:=StrToFloat(cmdl[2]);
    objects.Add(axis);
end;

procedure TForm1.cmd_circle;
begin

end;

procedure TForm1.cmd_color;
begin
    if cmdl[1]='red' then color := clRed else
    if cmdl[1]='green' then color := clGreen else
    if cmdl[1]='blue' then color := clBlue else
    if cmdl[1]='black' then color := clBlack else
    if cmdl[1]='white' then color := clWhite else
    if cmdl[1]='lime' then color := clLime else
    if cmdl[1]='maroon' then color := clMaroon else
    if cmdl[1]='gray' then color := clGray else
    if cmdl[1]='silver' then color := clSilver else
    if cmdl[1]='yellow' then color := clYellow else
    if cmdl[1]='olive' then color := clOlive else
    if cmdl[1]='navy' then color := clNavy else
    if cmdl[1]='purple' then color := clPurple else
    if cmdl[1]='red' then color := clRed else
    if cmdl[1]='teal' then color := clTeal else
    if cmdl[1]='red' then color := clRed else
    if cmdl[1]='fuchsia' then color := clFuchsia else
    if cmdl[1]='aqua' then color := clAqua else
    if cmdl[1]='moneygreen' then color := clMoneyGreen else
    if cmdl[1]='skyblue' then color := clSkyBlue else
    if cmdl[1]='cream' then color := clCream else
    if cmdl[1]='medgray' then color := clMedGray else
    color := TColor(StrToInt(cmdl[1]));
end;

procedure TForm1.cmd_grid;
var g: TGGrid;
begin
    g := TGGrid.Create;
    g.stepX:=StrToFloat(cmdl[1]);
    g.stepY:=StrToFloat(cmdl[2]);
    objects.Add(g);
end;

procedure TForm1.cmd_mark;
var m: TGMark;
begin
    m := TGMark.Create;
    m.fp.x:= StrToFloat(cmdl[2]);
    m.fp.y:= StrToFloat(cmdl[3]);
    m.text:=cmdl[4];
    objects.Add(m);
end;

procedure TForm1.cmd_plot;
var p: TGPlot; x, step: real; i: integer;
begin
    p := TGPlot.Create;
    x := StrToFloat(cmdl[2]);
    step := StrToFloat(cmdl[3]);
    SetLength(p.points, cmdl.Count-4);
    for i := 0 to high(p.points) do begin
        p.points[i].x:=x+step*i;
        p.points[i].y:=StrToFloat(cmdl[i+4]);
    end;
    objects.Add(p);
end;

procedure TForm1.cmd_point;
var p: TGPoint;
begin
    p := TGPoint.Create;
    p.x := StrToFloat(cmdl[1]);
    p.y := StrToFloat(cmdl[2]);
    p.fcolor:=color;
    objects.Add(p);
end;

procedure TForm1.cmd_polyline;
var i: integer; pl: TGPolyline;
begin
    pl := TGPolyline.Create;
    SetLength(pl.points, (cmdl.Count-1) div 2);
    for i := 0 to high(pl.points) do begin
        pl.points[i].x:= StrToFloat(cmdl[1+i*2]);
        pl.points[i].y:= StrToFloat(cmdl[1+i*2+1]);
    end;
    objects.Add(pl);
end;

procedure TForm1.cmd_translate;
var tm: TMatrix;
begin
    tm := identity;
    tm[3,1]:=StrToFloat(cmdl[1]);
    tm[3,2]:=StrToFloat(cmdl[2]);
    transform := mul(transform, tm);
end;

procedure TForm1.cmd_rotate;
var a: real; tm: TMatrix;
begin
    tm := identity;
    a := math.degtorad(StrToFloat(cmdl[1]));
    tm[1,1] := cos(a); tm[1,2] := sin(a);
    tm[2,1] :=-sin(a); tm[2,2] := cos(a);
    transform := mul(transform, tm);
end;

procedure TForm1.cmd_scale;
var tm: TMatrix;
begin
    tm := identity;
    tm[1,1] := StrToFloat(cmdl[1]);
    tm[2,2] := StrToFloat(cmdl[2]);
    transform := mul(transform, tm);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    stdin := TInputPipeStream.Create({$IFDEF WINDOWS}GetStdHandle(std_input_handle){$ELSE}0{$ENDIF});
    cmdl := TStringList.Create;
    cmdl.Delimiter:=' ';
    objects := TObjectList.Create(true);
    cw := form1.ClientWidth;
    ch := form1.ClientHeight;
    cnv := form1.Canvas;
    set_area(lo_x, hi_x, lo_y, hi_y);
end;

procedure TForm1.Action_SaveAsExecute(Sender: TObject);
var b: TBitmap;
begin
    if SavePictureDialog1.Execute then try
        b := TBitmap.Create;
        b.SetSize(Form1.ClientWidth, form1.ClientHeight);
//        b.SetSize(cnv.ClipRect.Right-cnv.ClipRect.Left, cnv.ClipRect.Bottom-cnv.ClipRect.Top);
        b.Canvas.CopyRect(cnv.ClipRect, cnv, cnv.ClipRect);
        b.SaveToFile(SavePictureDialog1.FileName);
    finally
        b.Free;
    end;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
    stdin.Free;
    cmdl.Free;
    objects.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
var i: integer;
begin
    Form1.BeginFormUpdate;
    cnv.Brush.Color:=clWhite;
    cnv.Brush.Style:=bsSolid;
    cnv.FillRect(cnv.ClipRect);
    cnv.Pen.Color:= clBlack;
    cnv.Brush.Style:=bsClear;
    for i := 0 to objects.Count-1 do begin
      cnv.pen.Color := (objects[i] as TGO).fcolor;
      tm := (objects[i] as TGO).ftransform;
      (objects[i] as TGO).paint;
    end;
    Form1.EndFormUpdate;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
    set_area(lo_x, hi_x, lo_y, hi_y);
end;

end.


