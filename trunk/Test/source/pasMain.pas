unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  PMemo = ^TMemo;

  TfrmMain = class(TForm)
    btnJob1: TButton;
    mmoDisplay: TMemo;
    btnJob2: TButton;
    btnGroupJobs: TButton;
    btnStopTest3: TButton;
    btn1: TButton;
    pnlTArea: TPanel;
    mmoT1: TMemo;
    mmoT2: TMemo;
    mmoT3: TMemo;
    mmoT4: TMemo;
    procedure btnJob1Click(Sender: TObject);
    procedure btnJob2Click(Sender: TObject);
    procedure btnGroupJobsClick(Sender: TObject);
    procedure btnStopTest3Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pnlTAreaResize(Sender: TObject);
  private
    { Private declarations }
  public
    function SelfedJob: integer;
  end;

var
  frmMain: TfrmMain;

implementation

uses ParallelJobs;

{$R *.dfm}

var
  Test3Group: TJobsGroup;

procedure CentralLoop(AMemo: PMemo; AID: string);
var
  iId: integer;
  cTime, cSleep, cTimeout: Cardinal;
begin
  Randomize;
  iId := StrToInt(AID);
  cSleep := Random(1000) + 1;
  cTimeout := Random(5000) + 10;
  cTime := GetTickCount + cTimeout;
  AMemo^.Lines.Add('#' + AID + ' START');
  while (cTime > GetTickCount) and not CurrentJobTerminated do
  begin
    { Note: Memo add line is performed by SendMessage
    }
    AMemo^.Lines.Add(StringOfChar('_', iId * 2) + '#' + AID + ' ' +
      TimeToStr(Now) + ' - ' + IntToStr(cSleep));
    Sleep(cSleep);
  end;
  AMemo^.Lines.Add('#' + AID + ' END');    
end;

function Job1(AMemo: PMemo): integer;
begin
  CentralLoop(AMemo, '1');
  Result := 0;
end;

function Job2(AMemo: PMemo): integer;
begin
  CentralLoop(AMemo, '2');
  Result := 0;
end;

function Job3(AMemo: PMemo): integer;
begin
  CentralLoop(AMemo, '3');
  Result := 0;
end;

function Job4(AMemo: PMemo): integer;
begin
  CentralLoop(AMemo, '4');
  Result := 0;
end;

function Job5(AMemo: PMemo): integer;
begin
  CentralLoop(AMemo, '5');
  Result := 0;
end;

function TfrmMain.SelfedJob: integer;
var
  cTime: Cardinal;
  s, s2: string;
begin
  s := Caption;
  cTime := GetTickCount + 5000;
  while (cTime > GetTickCount) and not CurrentJobTerminated do
  begin
    s2 := s + ' #1: ' + TimeToStr(Now);
    { Note: Only PostMessage will create the corret display we want.
    }
    PostMessage(Handle, WM_SETTEXT, Length(s2), Integer(Pchar(s2)));
    { Note: lines.add works with parallelism because updates use MESSAGES
    }
    mmoDisplay.Lines.Add('SelfedJob: ' + TimeToStr(Now));
    Sleep(1000);
  end;
  { Note: PostMessage don't wait the result from the message process
    so this don't hangs the main thread if we are wait for a thread end.
  }
  PostMessage(Handle, WM_SETTEXT, Length(s), Integer(Pchar(s)));
  Result := 0;
end;

procedure TfrmMain.btnJob1Click(Sender: TObject);
begin
  ParallelJob(Self, @TfrmMain.SelfedJob, nil, true);
end;

procedure TfrmMain.btnJob2Click(Sender: TObject);
begin
  ParallelJob(@Job2, @mmoT2, true);
end;

procedure TfrmMain.btnGroupJobsClick(Sender: TObject);
begin
  Test3Group := TJobsGroup.Create('MyJobsGroup');
  with Test3Group do
    try
      Randomize;
      InitJobCapture;
      try
        ParallelJob(@Job1, @mmoT1, true);
        ParallelJob(@Job2, @mmoT2, true);
        ParallelJob(@Job3, @mmoT3, true);
      finally
        EndJobCapture;
      end;

      mmoDisplay.Lines.Add(Name + ' Starting 1...');
      StartJobs;
      while JobsIsRunning > 0 do
        Application.ProcessMessages;
      Clear;
      mmoDisplay.Lines.Add(Name + ' Ended 1...');

      Randomize;      
      InitJobCapture;
      try
        ParallelJob(@Job3, @mmoT2, true);
        ParallelJob(@Job4, @mmoT3, true);
        ParallelJob(@Job5, @mmoT4, true);
      finally
        EndJobCapture;
      end;

      mmoDisplay.Lines.Add(Name + ' Starting 2...');
      StartJobs;
      while JobsIsRunning > 0 do
        Application.ProcessMessages;
      Clear;
      mmoDisplay.Lines.Add(Name + ' Ended 2...');
    finally
      Free;
    end;
  Test3Group := nil;
end;

procedure TfrmMain.btnStopTest3Click(Sender: TObject);
begin
  if Test3Group <> nil then
    Test3Group.StopJobs;
end;

procedure TfrmMain.btn1Click(Sender: TObject);
begin
  TerminateAllParallelJobs;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TerminateAllParallelJobs;
  { Note: To avoid Memo add line vcl deadlock with SendMessage.
  }
  WaitAllParallelJobsFinalization(Application.ProcessMessages);
end;

procedure TfrmMain.pnlTAreaResize(Sender: TObject);
var
  iW: integer;
begin
  iW := (pnlTArea.Width div 4) - 4;
  mmoT1.Width := iW;
  mmoT2.Left := mmoT1.Left + mmoT1.Width + 4; 
  mmoT2.Width := iW;
  mmoT3.Left := mmoT2.Left + mmoT2.Width + 4;
  mmoT3.Width := iW;
  mmoT4.Left := mmoT3.Left + mmoT3.Width + 4;  
  mmoT4.Width := iW;
end;

end.
