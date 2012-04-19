unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ParallelJobs;

type
  TProcessJobState = (pjsQueue, pjsWaitingLock, pjsProcessing);
  TProcessJob = class
  private
    FTerminated: Boolean;
    FLocks: array of THandle;
    FLockIndex: integer;
    FIndex: integer;
    FProcessUseTime: integer;
    FPosition: integer;
    FColor: TColor;
    FClockDelta: Integer;
  public
    constructor Create(AJobs: TJobsGroup; AIndex: integer;
      ALocks: array of THandle; AClock: integer);
    destructor Destroy; override;

    procedure ProcessJob(AParam: Pointer);

    function CanProcess: Boolean;
    function State: TProcessJobState;
  end;

  TfrmMain = class(TForm)
    pnlBar: TPanel;
    Label1: TLabel;
    seProcessors: TSpinEdit;
    seJobs: TSpinEdit;
    Label2: TLabel;
    btnSimulate: TButton;
    seClock: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    btnPause: TButton;
    btnPlay: TButton;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
  private
    { Private declarations }
  public
    Jobs: TJobsGroup;
    
    ProcessJobsLock: array of THandle;

    ProcessJobs: array of TProcessJob;

    procedure Invalidate;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TProcessJob }

constructor TProcessJob.Create(AJobs: TJobsGroup; AIndex: integer;
  ALocks: array of THandle; AClock: integer);
var
  i: Integer;
begin
  FTerminated := false;

  Randomize;
  FIndex := AIndex;
  SetLength(FLocks, Length(ALocks));
  for i := 0 to High(ALocks) do
    FLocks[i] := ALocks[i];
  FColor := RGB(Random(155) + 100, Random(155) + 100, Random(155) + 100);
  FPosition := 0;
  FClockDelta := AClock;

  FProcessUseTime := Random(40);

  ParallelJob(AJobs, Self, @TProcessJob.ProcessJob);
end;

procedure TProcessJob.ProcessJob(AParam: Pointer);
begin
  while not (CurrentJobTerminated or FTerminated) do
  begin
    case State of
      pjsQueue: begin
        Inc(FPosition);
      end;
      pjsWaitingLock: begin
        while not (CurrentJobTerminated or FTerminated) do
          if CanProcess then
            Break;

        Inc(FPosition);
      end;
      pjsProcessing: begin
        Inc(FPosition);
        if FPosition > 60 + FProcessUseTime then
        begin
          FPosition := 0;
          SetEvent(FLocks[FLockIndex]);
        end;
      end;
    end;

    Sleep(FClockDelta);
  end;
end;

function TProcessJob.CanProcess: Boolean;
var
  Index: integer;
begin
  Index := WaitForMultipleObjects(Length(FLocks), @FLocks[0], False, INFINITE);
  Result := Index <> WAIT_TIMEOUT;
  if Result then
    FLockIndex := Index - WAIT_OBJECT_0;
end;

function TProcessJob.State: TProcessJobState;
begin
  Result := pjsQueue;       
  if FPosition = 15 then
    Result := pjsWaitingLock
  else if FPosition > 15 then
    Result := pjsProcessing;
end;

function GetCpuCount: _SYSTEM_INFO;
var
  SysInfo: _SYSTEM_INFO;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo;
end;

destructor TProcessJob.Destroy;
begin
  FTerminated := true;
  inherited;
end;

{ TfrmMain }

procedure TfrmMain.FormPaint(Sender: TObject);
var
  i, x, px, py: integer;
  dRect: TRect;
  s: string;
const
  JW = 25;
  JH = 15;
  STEP = 25;
begin
  with Canvas do
  begin
    Lock;
    try
      Pen.Style := psSolid;
      Brush.Style := bsSolid;

      Pen.Color := clWhite;
      Brush.Color := RGB($C0, $C0, $C0);
      dRect := ClipRect;
      Inc(dRect.Top, pnlBar.Height);
      Rectangle(dRect);

      for i := 0 to High(ProcessJobs) do
      begin
        Pen.Style := psSolid;
        Pen.Color := clWhite;
        Brush.Color := RGB($C0, $C0, $C0);
        dRect := Rect(STEP - 5, (STEP * 2) - 5, STEP + JW + 5, 16 * STEP + JH + 5);
        OffsetRect(dRect, ProcessJobs[i].FIndex * (STEP + JW), 0);
        OffsetRect(dRect, 0, pnlBar.Height);
        Rectangle(dRect);

        Pen.Style := psClear;
        dRect := Rect(STEP, (STEP * 2), STEP + JW, (STEP * 2) + JH);
        OffsetRect(dRect, ProcessJobs[i].FIndex * (STEP + JW), 0);
        OffsetRect(dRect, 0, pnlBar.Height);
        for x := 1 to 15 do
        begin
          if x mod 3 = 0 then
            InflateRect(dRect, -1, -1);

          Pen.Color := clWhite;
          Brush.Color := RGB($FA, $FE, $A5);

          Rectangle(dRect);
          OffsetRect(dRect, 0, STEP);
        end;
      end;

      Pen.Style := psSolid;
      Brush.Style := bsSolid;

      Brush.Color := RGB($C0, $C0, $C0);      

      dRect := Rect(STEP - 5, STEP - 5, STEP + JW + 5, 16 * STEP + JH + 5);
      OffsetRect(dRect, frmMain.Width - (STEP + JW) - 25, pnlBar.Height);
      Rectangle(dRect);

      Pen.Style := psClear;
      dRect := Rect(STEP, STEP, STEP + JW, STEP + JH);
      OffsetRect(dRect, frmMain.Width - (STEP + JW) - 25, pnlBar.Height);
      for x := 1 to 15 do
      begin
        if x mod 3 = 0 then
          InflateRect(dRect, -1, -1);

        Pen.Color := clWhite;
        Brush.Color := RGB($B0, $FE, $A5);

        Rectangle(dRect);
        OffsetRect(dRect, 0, STEP);
      end;

      Pen.Style := psSolid;
      Brush.Style := bsSolid;      

      for i := 0 to High(ProcessJobs) do
      begin
        Brush.Color := ProcessJobs[i].FColor;
        dRect := Rect(STEP, STEP, STEP + JW, STEP + JH);
        case ProcessJobs[i].State of
          pjsQueue: begin
            Pen.Color := clRed;
            OffsetRect(dRect, ProcessJobs[i].FIndex * (STEP + JW), ProcessJobs[i].FPosition * STEP);
          end;
          pjsWaitingLock: begin
            Pen.Color := clYellow;
            OffsetRect(dRect, ProcessJobs[i].FIndex * (STEP + JW), ProcessJobs[i].FPosition * STEP);
          end;
          pjsProcessing: begin
            Pen.Color := clGreen;
            if ProcessJobs[i].FPosition < 30 then
            begin
              px := ProcessJobs[i].FIndex * (STEP + JW);
              px := Round(px + ((ProcessJobs[i].FPosition - 16) * (((frmMain.Width - (STEP + JW) - 25) - px) / 16)));

              OffsetRect(dRect, px, 16 * STEP);
            end else if ProcessJobs[i].FPosition < 45 then
            begin
              px := frmMain.Width - (STEP + JW) - 25;

              OffsetRect(dRect, px, 16 * STEP - ((ProcessJobs[i].FPosition - 30) * STEP));
            end else if ProcessJobs[i].FPosition < 45 + 16 then
            begin
              px := ProcessJobs[i].FIndex * (STEP + JW);
              px := Round((frmMain.Width - (STEP + JW) - 25) - ((ProcessJobs[i].FPosition - 45) * (((frmMain.Width - (STEP + JW) - 25) - px) / 16)));

              OffsetRect(dRect, px, 15 * STEP - ((45 - 30) * STEP));
            end else
            begin
              Pen.Color := clLime;            
              px := ProcessJobs[i].FIndex * (STEP + JW);
              OffsetRect(dRect, px, 0);
            end;
          end;
        end;

        OffsetRect(dRect, 0, pnlBar.Height);
        RoundRect(dRect.Left, dRect.Top, dRect.Right, dRect.Bottom, 15, 15);
        Brush.Color := Pen.Color;
        Rectangle(dRect.Left, dRect.Top, dRect.Left + 15, dRect.Bottom);

        Brush.Style := bsClear;
        s := IntToStr(ProcessJobs[i].FIndex + 1);
        Font.Style := [fsBold];
        TextOut(dRect.Left + ((15 - TextWidth(s)) div 2), dRect.Top + ((JH - TextHeight(s)) div 2), s);
        //s := IntToStr(ProcessJobs[i].FPosition);
        //TextOut(dRect.Left + 15 + ((JW - 15 - TextWidth(s)) div 2), dRect.Top + ((JH - TextHeight(s)) div 2), s);
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ParallelJob(Self, @TfrmMain.Invalidate);

  ControlStyle := ControlStyle - [csOpaque];
  DoubleBuffered := true;

  seProcessors.Value := GetCpuCount.dwNumberOfProcessors;
  seJobs.Value := GetCpuCount.dwNumberOfProcessors * 2;
  
  Jobs := TJobsGroup.Create('Jobs');  
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  i, x: integer;
begin
  for i := 0 to High(ProcessJobsLock) do
    for x := 0 to High(ProcessJobsLock) do
      SetEvent(ProcessJobsLock[x]);

  TerminateAllParallelJobs(true);
  WaitAllParallelJobsFinalization;
end;

procedure TfrmMain.Invalidate;
begin
  while not Application.Terminated do
  begin
    Repaint;
    Sleep(10);
  end;
end;

procedure TfrmMain.btnSimulateClick(Sender: TObject);
var
  i: integer;
begin
  Jobs.Clear;

  for i := 0 to High(ProcessJobsLock) do
    CloseHandle(ProcessJobsLock[i]);

  for i := 0 to High(ProcessJobs) do
    ProcessJobs[i].Free;

  SetLength(ProcessJobsLock, seProcessors.Value);
  for i := 0 to High(ProcessJobsLock) do
    ProcessJobsLock[i] := CreateEvent(nil, false, true, PChar('PJSyncLock' + IntToStr(i)));

  SetLength(ProcessJobs, seJobs.Value);
  for i := 0 to High(ProcessJobs) do
    ProcessJobs[i] := TProcessJob.Create(Jobs, i, ProcessJobsLock, Round(((seClock.MaxValue / seClock.Value) * 10)));

  Jobs.StartJobs;
end;

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
  Jobs.StopJobs;
end;

procedure TfrmMain.btnPlayClick(Sender: TObject);
begin
  Jobs.StartJobs;
end;

end.
