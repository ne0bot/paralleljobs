unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ScktComp;

type
  PSocketBuffer = ^TSocketBuffer;
  TSocketBuffer = packed record
    id: integer;
    d: string;
    s: TCustomWinSocket;
    n: PSocketBuffer;
  end;

  TAdvServerSocket = class(ScktComp.TServerSocket)
  protected
    procedure Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent); override;
    procedure Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer); override;
    procedure ClientError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  end;

  TServerCtrl = class
  private
    FSocket: TAdvServerSocket;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SocketClientRead(Sender: TObject; Socket: TCustomWinSocket);

    property Socket: TAdvServerSocket read FSocket;
  end;

  TfrmMain = class(TForm)
    Label1: TLabel;
    edtServerPort: TEdit;
    btnServerListen: TButton;
    mmoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnServerListenClick(Sender: TObject);
  private
    { Private declarations }
  public
    AsyncLogStrs: TStringList;
    AsyncLogLock: TRTLCriticalSection;

    Server: TServerCtrl;

    procedure Log(const ALog: string);
    procedure AsyncLog(const ALog: string);
    procedure SyncAsyncLog;

    procedure BufferRead(ABuffer: PSocketBuffer);
    procedure BufferDispatch;                    
  end;

var
  frmMain: TfrmMain;

var
  FirstSocketBuffer: PSocketBuffer = nil;
  LastSocketBuffer: PSocketBuffer = nil;

  SocketBufferCount: integer = 0;
  SocketBufferLock: TRTLCriticalSection;
  SocketBufferEvent: THandle;
  SocketRunCommandThreadTerminate: boolean = false;

implementation

uses ParallelJobs;

{$R *.dfm}

function CurrentTimeStr: string;
begin
  Result := '{S' + IntToStr(GetTickCount) + '}';
end;

{ TAdvServerSocket }

procedure TAdvServerSocket.Event(Socket: TCustomWinSocket;
  SocketEvent: TSocketEvent);

  procedure AsyncLog(const Alog: string);
  begin
    frmMain.AsyncLog('[' + IntToStr(Socket.Handle) + ']: ' + Alog);
  end;

begin
  inherited;

  case SocketEvent of
    seLookup     : AsyncLog('seLookup');
    seConnecting : AsyncLog('seConnecting');
    seConnect    : AsyncLog('seConnect');
    seListen     : AsyncLog('seListen');
    seDisconnect : AsyncLog('seDisconnect');
    seAccept     : AsyncLog('seAccept');
    seRead       : AsyncLog('seRead');
    seWrite      : AsyncLog('seWrite');
  end;

  if (Socket.Handle <> Self.Socket.Handle) then
    Socket.OnErrorEvent := ClientError;
end;

procedure TAdvServerSocket.Error(Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  inherited;

  frmMain.AsyncLog('Error: ' + IntToStr(Integer(ErrorEvent)) + ' code: ' + IntToStr(ErrorCode));

  ErrorCode := 0;
end;

procedure TAdvServerSocket.ClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  frmMain.AsyncLog('[' + IntToStr(Socket.Handle) + ']: Error: ' +
    IntToStr(Integer(ErrorEvent)) + ' code: ' + IntToStr(ErrorCode));

  ErrorCode := 0;
end;

{ TServerCtrl }

constructor TServerCtrl.Create;
begin
  FSocket := TAdvServerSocket.Create(nil);
  FSocket.OnClientRead := SocketClientRead;
end;

destructor TServerCtrl.Destroy;
begin
  FSocket.Free;

  inherited;
end;

procedure TServerCtrl.SocketClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
  NewBuffer: PSocketBuffer;
  ReceivedData: string;
begin
  SetLength(ReceivedData, Socket.ReceiveLength);
  Socket.ReceiveBuf(ReceivedData[1], Socket.ReceiveLength);

  try
    New(NewBuffer);

    Inc(SocketBufferCount);
    NewBuffer^.id := SocketBufferCount;

    NewBuffer^.d := ReceivedData + ' ' + CurrentTimeStr;
    NewBuffer^.s := Socket;

    NewBuffer^.n := nil;

    EnterCriticalSection(SocketBufferLock);
    try
      if FirstSocketBuffer = nil then
        FirstSocketBuffer := NewBuffer
      else
        LastSocketBuffer^.n := NewBuffer;

      LastSocketBuffer := NewBuffer;
    finally
      LeaveCriticalSection(SocketBufferLock);
      SetEvent(SocketBufferEvent);
    end;
  except
    // Exception manager...
  end;  
end;

{ TfrmMain }

procedure TfrmMain.Log(const ALog: string);
begin
  mmoLog.Lines.Add(DateTimeToStr(now) + ': ' + ALog);
end;

procedure TfrmMain.AsyncLog(const ALog: string);
begin
  EnterCriticalSection(AsyncLogLock);
  try
    AsyncLogStrs.Add(ALog);
  finally
    LeaveCriticalSection(AsyncLogLock);
  end;

  TThread.Synchronize(nil, SyncAsyncLog);
end;

procedure TfrmMain.SyncAsyncLog;
var
  i: integer;
begin
  EnterCriticalSection(AsyncLogLock);
  try
    for i := 0 to AsyncLogStrs.Count - 1 do
      Log(AsyncLogStrs[i]);
    AsyncLogStrs.Clear;
  finally
    LeaveCriticalSection(AsyncLogLock);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  AsyncLogStrs := TStringList.Create;
  InitializeCriticalSection(AsyncLogLock);

  InitializeCriticalSection(SocketBufferLock);
  SocketBufferEvent := CreateEvent(nil, true, false, nil);

  ParallelJob(Self, @TfrmMain.BufferDispatch);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SocketRunCommandThreadTerminate := true;  

  SetEvent(SocketBufferEvent);     
  CloseHandle(SocketBufferEvent);    

  AsyncLogStrs.Free;
end;

procedure TfrmMain.btnServerListenClick(Sender: TObject);
begin
  if Server = nil then
  begin
    Server := TServerCtrl.Create;
    Server.Socket.ServerType := stNonBlocking;
    Server.Socket.Port := StrToInt(edtServerPort.Text);
    Server.Socket.Active := true;

    btnServerListen.Caption := 'Desactive Server';    
  end else
  begin
    Server.Free;
    Server := nil;
    btnServerListen.Caption := 'Active Server';
  end;
end;

procedure TfrmMain.BufferRead(ABuffer: PSocketBuffer);
var
  sCmd, sParam, sTime: string;
begin
  try
    sCmd := Copy(ABuffer^.d, 1, 4);
    sParam := Copy(ABuffer^.d, 5, Length(ABuffer^.d));
    sTime := Copy(sParam, Pos('{', sParam) - 1, Length(sParam));
    sParam := Copy(sParam, 1, Length(sParam) - Length(sTime));

    AsyncLog('['+ ABuffer^.s.RemoteAddress + ']: ' +
      IntToStr(ABuffer^.id) + ' Cmd ' + sCmd + '(' + sParam + sTime + ') [' + DateTimeToStr(Now) + ']');

    if sCmd = '' then
    begin
      // none
    end else if sCmd = 'RAND' then
    begin
      // none
    end else if sCmd = 'WAIT' then
    begin

      Sleep(StrToInt(sParam));
      ABuffer^.s.SendText('WAIT R ' + sParam);
    end else if sCmd = 'RQST' then
    begin
      ABuffer^.s.SendText('RQST R ' + DateTimeToStr(Now));
    end else if sCmd = 'CCNT' then
    begin                                                                             
      ABuffer^.s.SendText('CCNT R ' + IntToStr(Server.Socket.Socket.ActiveConnections));
    end;
  finally
    ABuffer^.d := '';
    Dispose(ABuffer);
  end;
end;

procedure TfrmMain.BufferDispatch;
var
  BufferStep: PSocketBuffer;
  WaitSignal: Boolean;
begin
  while not SocketRunCommandThreadTerminate do
  begin
    ResetEvent(SocketBufferEvent);

    EnterCriticalSection(SocketBufferLock);
    try
      if FirstSocketBuffer <> nil then
      begin
        WaitSignal := False;

        BufferStep := FirstSocketBuffer;
        FirstSocketBuffer := FirstSocketBuffer^.n;
                    
        if FirstSocketBuffer = nil then
        begin
          LastSocketBuffer := nil;
          WaitSignal := true;
        end;

        ParallelJob(Self, @TfrmMain.BufferRead, BufferStep);
      end;
    finally
      LeaveCriticalSection(SocketBufferLock);
    end;

    if WaitSignal then
      WaitForSingleObject(SocketBufferEvent, INFINITE)
    else
      Sleep(10);
  end;
end;

end.

