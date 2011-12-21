unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    lblProcess: TLabel;
    edtTestCount: TEdit;
    Label1: TLabel;
    btnTest: TButton;
    lblCalls: TLabel;
    tmrCounterCheck: TTimer;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrCounterCheckTimer(Sender: TObject);
  private
    { Private declarations }
  public
    procedure AddCounter(AParam: Pointer);
  end;

var
  frmMain: TfrmMain;

implementation

uses ParallelJobs;

var
  HeavyUseGroup: TJobsGroup;
  Counter: Integer;
  rtl: TRTLCriticalSection;

{$R *.dfm}

procedure TfrmMain.AddCounter(AParam: Pointer);
begin
  InterlockedIncrement(Counter);
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var
  i: integer;
begin
  Counter := 0;
  if HeavyUseGroup <> nil then
    HeavyUseGroup.Free;
    
  HeavyUseGroup := TJobsGroup.Create('HeavyUseGroup');
  with HeavyUseGroup do
  begin
    InitJobCapture;
    try
      for i := 1 to StrToInt(edtTestCount.Text) do
      begin
        ParallelJob(Self, @TfrmMain.AddCounter, nil, false);
        lblCalls.Caption := 'Threads Created: ' + IntToStr(i);
        Application.ProcessMessages;
      end;
    finally
      EndJobCapture;
    end;

    lblCalls.Caption := lblCalls.Caption + ' | Jobs: ' + IntToStr(JobsCount);
    Application.ProcessMessages;

    lblCalls.Caption := lblCalls.Caption + ' | Started: ' + IntToStr(StartJobs);
    Application.ProcessMessages;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitializeCriticalSection(rtl);
end;

procedure TfrmMain.tmrCounterCheckTimer(Sender: TObject);
begin
  lblProcess.Caption := 'Counter: ' + IntToStr(Counter);
end;

end.
