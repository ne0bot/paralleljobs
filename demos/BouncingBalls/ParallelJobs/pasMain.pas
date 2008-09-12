unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, ParallelJobs;

type
  TfrmMain = class(TForm)
    pnlInfo: TPanel;
    lblInfo: TLabel;
    btnSwitchDMode: TSpeedButton;
    btnAddBall: TSpeedButton;
    btnClean: TSpeedButton;
    pnlDisplay: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddBallClick(Sender: TObject);
    procedure btnSwitchDModeClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
  private
    { Private declarations }
  public
    DisplayCanvas: TCanvas;
    Jobs: TJobsGroup;
    procedure CircleDraw(APosition: Longint);
  end;

  TPanellAccess = class(TPanel);

var
  frmMain: TfrmMain;

implementation

uses StrUtils;

{$R *.dfm}

var
  GlobalPause: boolean = false;

{ TfrmMain }

procedure TfrmMain.CircleDraw(APosition: Integer);
var
  X, Y: integer;
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
    Speed := Point((Random(10) - 5), (Random(10) - 5));
  until (Speed.X <> 0) and (Speed.Y <> 0);

  EllipseRect := Rect(0, 0, Radius * 2, Radius * 2);

  MaxWidth := (DisplayCanvas.ClipRect.Right - DisplayCanvas.ClipRect.Left) - (Radius * 2);
  MaxHeight := (DisplayCanvas.ClipRect.Bottom - DisplayCanvas.ClipRect.Top) - (Radius * 2);

  Y := HiWord(APosition);
  X := LoWord(APosition);

  while not CurrentJobTerminated do
    if not GlobalPause then
    begin
      DisplayCanvas.Lock;
      try
        DisplayCanvas.Pen.Color := clBlack;
        DisplayCanvas.Brush.Color := clBlack;
        OffsetRect(EllipseRect, X, Y);
        DisplayCanvas.Ellipse(EllipseRect);
        OffsetRect(EllipseRect, -X, -Y);

        X := X + Speed.X;
        Y := Y + Speed.Y;
        if (X < 0) or (X > MaxWidth) then
          Speed.X := -Speed.X;
        if (Y < 0) or (Y > MaxHeight) then
          Speed.Y := -Speed.Y;

        DisplayCanvas.Pen.Color := Color xor $FFFFFF;
        DisplayCanvas.Brush.Color := Color;
        OffsetRect(EllipseRect, X, Y);
        DisplayCanvas.Ellipse(EllipseRect);
        OffsetRect(EllipseRect, -X, -Y);
      finally
        DisplayCanvas.Unlock;
      end;
      Sleep(10);
    end else
      Sleep(1);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DisplayCanvas := TPanellAccess(pnlDisplay).Canvas;
  Jobs := TJobsGroup.Create('MyCircles');
  lblInfo.Caption := 'Balls count: 0';
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Jobs.Free;
end;

procedure TfrmMain.btnAddBallClick(Sender: TObject);
begin
  Randomize;
  ParallelJob(Jobs, Self, @TfrmMain.CircleDraw, Pointer(MakeLong(Random(100), Random(100))));
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
  pnlDisplay.Invalidate;
  lblInfo.Caption := 'Balls count: 0';
end;

end.
