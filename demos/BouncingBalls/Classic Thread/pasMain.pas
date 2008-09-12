unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Contnrs, StdCtrls, Buttons;

type
  TBall = class(TThread)
  private
    FCanvas: TCanvas;
    FX: integer;
    FY: integer;
  public
    constructor Create(ACanvas: TCanvas; AX, AY: integer);
    procedure Execute; override;
  end;

  TfrmMain = class(TForm)
    pnlDisplay: TPanel;
    pnlInfo: TPanel;
    lblInfo: TLabel;
    btnSwitchDMode: TSpeedButton;
    btnAddBall: TSpeedButton;
    btnClean: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddBallClick(Sender: TObject);
    procedure btnSwitchDModeClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
  private
    { Private declarations }
  public
    DisplayCanvas: TCanvas;
    BallsList: TObjectList;
  end;

  TPanellAccess = class(TPanel);

var
  frmMain: TfrmMain;

implementation

uses StrUtils;

{$R *.dfm}

var
  GlobalPause: boolean = false;

{ TBall }

constructor TBall.Create(ACanvas: TCanvas; AX, AY: integer);
begin
  FCanvas := ACanvas;
  FX := AX;
  FY := AY;

  inherited Create(false);
end;

procedure TBall.Execute;
var
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

  MaxWidth := (FCanvas.ClipRect.Right - FCanvas.ClipRect.Left) - (Radius * 2);
  MaxHeight := (FCanvas.ClipRect.Bottom - FCanvas.ClipRect.Top) - (Radius * 2);
  while not Terminated do
    if not GlobalPause then
    begin
      FCanvas.Lock;
      try
        FCanvas.Pen.Color := clBlack;      
        FCanvas.Brush.Color := clBlack;
        OffsetRect(EllipseRect, FX, FY);
        FCanvas.Ellipse(EllipseRect);
        OffsetRect(EllipseRect, -FX, -FY);

        FX := FX + Speed.X;
        FY := FY + Speed.Y;
        if (FX < 0) or (FX > MaxWidth) then
          Speed.X := -Speed.X;
        if (FY < 0) or (FY > MaxHeight) then
          Speed.Y := -Speed.Y;

        FCanvas.Pen.Color := Color xor $FFFFFF;
        FCanvas.Brush.Color := Color;
        OffsetRect(EllipseRect, FX, FY);
        FCanvas.Ellipse(EllipseRect);
        OffsetRect(EllipseRect, -FX, -FY);
      finally
        FCanvas.Unlock;
      end;
      Sleep(10);
    end else
      Sleep(1);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DisplayCanvas := TPanellAccess(pnlDisplay).Canvas;
  BallsList := TObjectList.Create;
  lblInfo.Caption := 'Balls count: 0';  
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  BallsList.Free;
end;

procedure TfrmMain.btnAddBallClick(Sender: TObject);
begin
  Randomize;
  BallsList.Add(TBall.Create(DisplayCanvas, Random(100), Random(100)));
  lblInfo.Caption := Format('Balls count: %d', [BallsList.Count]);
end;

procedure TfrmMain.btnSwitchDModeClick(Sender: TObject);
begin
  GlobalPause := not GlobalPause;
  btnSwitchDMode.Caption := IfThen(GlobalPause, '4', ';');
end;

procedure TfrmMain.btnCleanClick(Sender: TObject);
begin
  try
    BallsList.Free;
    pnlDisplay.Invalidate;
  finally
    BallsList := TObjectList.Create;
    lblInfo.Caption := 'Balls count: 0';
  end;
end;

end.
