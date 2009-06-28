unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, ParallelJobs;

type
  PCircleDrawParam = ^TCircleDrawParam;
  TCircleDrawParam = record
    Buffer: TBitmap;
    Position: Integer;
  end;
  
  TfrmMain = class(TForm)
    pnlInfo: TPanel;
    lblInfo: TLabel;
    btnSwitchDMode: TSpeedButton;
    btnAddBall: TSpeedButton;
    btnClean: TSpeedButton;
    btnAdd100: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddBallClick(Sender: TObject);
    procedure btnSwitchDModeClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAdd100Click(Sender: TObject);
  private
    { Private declarations }
  public
    CircleDrawBase: TCircleDrawParam;
    Jobs: TJobsGroup;
    procedure CircleDraw(ACircle: PCircleDrawParam);
    procedure DrawFlusher;
  end;

var
  frmMain: TfrmMain;

implementation

uses StrUtils;

{$R *.dfm}

var
  GlobalPause: boolean = false;

{ TfrmMain }

procedure TfrmMain.CircleDraw(ACircle: PCircleDrawParam);
var
  X, Y: integer;
  Canvas: TCanvas;
  Position: TPoint;
  Radius: Integer;
  Color: TColor;
  Speed: TPoint;
  EllipseRect: TRect;
  MaxWidth, MaxHeight: integer;
begin
  Radius := 10;
  Position.X := Radius;
  Position.Y := Radius;
  Color := RGB(Random(255), Random(255), Random(255));
  repeat
    Speed := Point((Random(5) - 5), (Random(5) - 5));
  until (Speed.X <> 0) and (Speed.Y <> 0);

  EllipseRect := Rect(0, 0, Radius * 2, Radius * 2);

  Canvas := ACircle^.Buffer.Canvas;

  MaxWidth := ACircle^.Buffer.Width - (Radius * 2);
  MaxHeight := ACircle^.Buffer.Height - (Radius * 2);

  Y := HiWord(ACircle.Position);
  if Y > MaxHeight then Y := MaxHeight;
  X := LoWord(ACircle.Position);
  if X > MaxWidth then Y := MaxWidth;

  while not CurrentJobTerminated do
    if not GlobalPause then
    begin
      ACircle^.Buffer.Canvas.Lock;
      try
        Canvas.Pen.Color := clBlack;
        Canvas.Brush.Color := clBlack;
        OffsetRect(EllipseRect, X, Y);
        Canvas.Ellipse(EllipseRect);
        OffsetRect(EllipseRect, -X, -Y);

        X := X + Speed.X;
        Y := Y + Speed.Y;
        if (X < 0) or (X > MaxWidth) then
          Speed.X := -Speed.X;
        if (Y < 0) or (Y > MaxHeight) then
          Speed.Y := -Speed.Y;

        Canvas.Pen.Color := Color xor $FFFFFF;
        Canvas.Brush.Color := Color;
        OffsetRect(EllipseRect, X, Y);
        Canvas.Ellipse(EllipseRect);
        OffsetRect(EllipseRect, -X, -Y);
      finally
        ACircle^.Buffer.Canvas.Unlock;
      end;
      Sleep(10);
    end else
      Sleep(1);

  Dispose(ACircle);
end;

procedure TfrmMain.DrawFlusher;
begin
  while not CurrentJobTerminated do
  begin
    CircleDrawBase.Buffer.Canvas.Lock;
    try
      if Canvas.TryLock then
      begin
        Canvas.Draw(0, 0, CircleDrawBase.Buffer);
        Canvas.Unlock;
      end;
    finally
      CircleDrawBase.Buffer.Canvas.Unlock;
    end;
    Sleep(10);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ControlStyle := ControlStyle + [csOpaque];

  with CircleDrawBase do
  begin
    Buffer := TBitmap.Create;
    Buffer.Width := 320;
    Buffer.Height := 220;
    Buffer.Canvas.Brush.Color := clBlack;
    Buffer.Canvas.FillRect(Buffer.Canvas.ClipRect);

    Position := 0;
  end;

  Jobs := TJobsGroup.Create('MyCircles');
  lblInfo.Caption := 'Balls count: 0';
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Jobs.Free;
  TerminateAllParallelJobs;
end;

procedure TfrmMain.btnAddBallClick(Sender: TObject);
var
  pParam: PCircleDrawParam;
begin
  Randomize;

  New(pParam);
  pParam^ := CircleDrawBase;
  pParam^.Position := MakeLong(
    Random(CircleDrawBase.Buffer.Width),
    Random(CircleDrawBase.Buffer.Height)
  );
  ParallelJob(Jobs, Self, @TfrmMain.CircleDraw, pParam);
  Jobs.StartJobs;

  lblInfo.Caption := Format('Balls count: %d', [Jobs.JobsCount]);
end;

procedure TfrmMain.btnSwitchDModeClick(Sender: TObject);
begin
  GlobalPause := not GlobalPause;
  btnSwitchDMode.Caption := IfThen(GlobalPause, '4', ';');
end;

procedure TfrmMain.btnCleanClick(Sender: TObject);
begin
  Jobs.Clear;
  lblInfo.Caption := 'Balls count: 0';
  CircleDrawBase.Buffer.Canvas.Brush.Color := clBlack;
  CircleDrawBase.Buffer.Canvas.FillRect(CircleDrawBase.Buffer.Canvas.ClipRect);
  Invalidate;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if Tag = 0 then
    ParallelJob(Self, @TfrmMain.DrawFlusher, nil);
  Tag := 1;
end;

procedure TfrmMain.btnAdd100Click(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 100 do
    btnAddBallClick(nil);
end;

end.
