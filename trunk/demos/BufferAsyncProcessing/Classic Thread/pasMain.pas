unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

const
  WM_PROCESSSTART        = WM_USER + $F0;
  WM_PROCESSSTOP         = WM_USER + $F1;
  WM_PROCESSSBUFFERCLEAN = WM_USER + $F2;
  WM_PROCESSSBUFFERADDED = WM_USER + $F3;

type
  PBuffer = ^TBuffer;
  TBuffer = packed record
    id: Cardinal;
    n: PBuffer;
  end;

  TBufferReaderCall = class(TThread)
  private
    FBuffer: PBuffer;
    constructor Create(ABuffer: PBuffer);
  public
    procedure Execute; override;
  end;

  TBufferDispatch = class(TThread)
  private
    constructor Create;
  public
    procedure Execute; override;
  end;

  TfrmMain = class(TForm)
    edtCreateCount: TEdit;
    btnCreate: TButton;
    mmoLog: TMemo;
    procedure btnCreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    BufferDispatch: TBufferDispatch;
    BufferCount: Cardinal;

    procedure WMProcessStart(var msg: TMessage); message WM_PROCESSSTART;
    procedure WMProcessStop(var msg: TMessage); message WM_PROCESSSTOP;
    procedure WMProcessBufferClean(var msg: TMessage); message WM_PROCESSSBUFFERCLEAN;
    procedure WMProcessBufferAdded(var msg: TMessage); message WM_PROCESSSBUFFERADDED;

    procedure Log(const ALog: string);

    procedure AddBuffer;

    procedure ReadBuffer(ABufferId: integer);
  end;

var
  frmMain: TfrmMain;

var
  FirstBuffer: PBuffer = nil;
  LastBuffer: PBuffer = nil;

  BufferLock: TRTLCriticalSection;
  BufferEvent: THandle;

implementation

{$R *.dfm}

{ TBufferReaderCall }

constructor TBufferReaderCall.Create(ABuffer: PBuffer);
begin
  FBuffer := ABuffer;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TBufferReaderCall.Execute;
begin
  frmMain.ReadBuffer(FBuffer^.id);
  Dispose(FBuffer);
end;

{ TBufferDispatch }

constructor TBufferDispatch.Create;
begin
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TBufferDispatch.Execute;
var
  BufferStep, BufferStepNext: PBuffer;
begin
  while not Terminated do
  begin
    EnterCriticalSection(BufferLock);
    try
      BufferStep := FirstBuffer;
      while BufferStep <> nil do
      begin
        BufferStepNext := BufferStep^.n;
        TBufferReaderCall.Create(BufferStep);
        BufferStep := BufferStepNext;
      end;

      FirstBuffer := nil;
      LastBuffer := nil;
      ResetEvent(BufferEvent);
    finally
      LeaveCriticalSection(BufferLock);
    end;

    WaitForSingleObject(BufferEvent, INFINITE);
  end;
end;

{ TfrmMain }

procedure TfrmMain.WMProcessStart(var msg: TMessage);
begin
  Log(Format('PROCESS START: %d', [msg.WParam]));
end;

procedure TfrmMain.WMProcessStop(var msg: TMessage);
begin
  Log(Format('PROCESS STOP: %d', [msg.WParam]));
end;

procedure TfrmMain.WMProcessBufferClean(var msg: TMessage);
begin
  Log(Format('PROCESS START: %d', [msg.WParam]));
end;

procedure TfrmMain.WMProcessBufferAdded(var msg: TMessage);
begin
  Log(Format('PROCESS START: %d', [msg.WParam]));
end;

procedure TfrmMain.Log(const ALog: string);
begin
  mmoLog.Lines.Add(DateTimeToStr(now) + ': ' + ALog);
end;

procedure TfrmMain.AddBuffer;
var
  NewBuffer: PBuffer;
begin
  EnterCriticalSection(BufferLock);
  try
    try
      New(NewBuffer);
      NewBuffer^.n := nil;
      NewBuffer^.id := BufferCount;
      
      Inc(BufferCount);

      if FirstBuffer = nil then
        FirstBuffer := NewBuffer;

      if LastBuffer <> nil then
        LastBuffer^.n := NewBuffer;

      LastBuffer := NewBuffer;
    except
      // Exception manager...
    end;
  finally
    LeaveCriticalSection(BufferLock);
    SetEvent(BufferEvent);      
  end;
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to StrToInt(edtCreateCount.Text) do
    AddBuffer;
end;

procedure TfrmMain.ReadBuffer(ABufferId: integer);
begin
  Randomize;
  PostMessage(Handle, WM_PROCESSSTART, ABufferId, 0);
  Sleep(Random(5000));
  PostMessage(Handle, WM_PROCESSSTOP, ABufferId, 0);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitializeCriticalSection(BufferLock);
  BufferEvent := CreateEvent(nil, true, false, nil);

  BufferDispatch := TBufferDispatch.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  BufferDispatch.Terminate;

  SetEvent(BufferEvent);
  CloseHandle(BufferEvent);  
end;

end.

