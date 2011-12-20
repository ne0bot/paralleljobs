unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ScktComp;

type
  TfrmMain = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtHostAddr: TEdit;
    edtHostPort: TEdit;
    edtClientsCount: TEdit;
    btnActiveClients: TButton;
    mmoLog: TMemo;
    cbbClientCmds: TComboBox;
    btnClientSendCmd: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure btnActiveClientsClick(Sender: TObject);
    procedure ClientSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure btnClientSendCmdClick(Sender: TObject);
  private
    { Private declarations }
  public
    AsyncLogStrs: TStringList;
    AsyncLogLock: TRTLCriticalSection;

    Clients: array of TClientSocket;

    procedure Log(const ALog: string);
    procedure AsyncLog(const ALog: string);
    procedure SyncAsyncLog;
  end;

var
  frmMain: TfrmMain;

implementation

uses ParallelJobs, WinSock;

{$R *.dfm}

function CurrentTimeStr: string;
begin
  Result := '{C' + IntToStr(GetTickCount) + '}';
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
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  AsyncLogStrs.Free;
end;

procedure TfrmMain.ClientSocketRead(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  frmMain.AsyncLog('[' + IntToStr(Socket.Handle) + ']: Recv: ' +
    Socket.ReceiveText + ' [' + DateTimeToStr(Now) + ']');
end;

procedure TfrmMain.btnActiveClientsClick(Sender: TObject);
var
  i: integer;
begin
  if Length(Clients) = 0 then
  begin
    SetLength(Clients, StrToInt(edtClientsCount.Text));
    for i := 0 to High(Clients) do
    begin
      Clients[i] := TClientSocket.Create(nil);
      Clients[i].ClientType := ctNonBlocking;
      Clients[i].Host := edtHostAddr.Text;
      Clients[i].Port := StrToInt(edtHostPort.Text);
      Clients[i].OnRead := ClientSocketRead;
      Clients[i].OnError := ClientSocketError;
      Clients[i].Active := true;
      Application.ProcessMessages;

      if i mod SOMAXCONN = 0 then
      begin
        Application.ProcessMessages;
        Sleep(100);
      end;
    end;

    btnActiveClients.Caption := 'Desactive Clients';
  end else
  begin
    for i := 0 to High(Clients) do
    begin
      Clients[i].Active := false;
      Clients[i].Free;
    end;

    SetLength(Clients, 0);

    btnActiveClients.Caption := 'Active Clients';
  end;
end;

procedure TfrmMain.ClientSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  AsyncLog('[' + IntToStr(Socket.Handle) + ']: Error: ' +
    IntToStr(Integer(ErrorEvent)) + ' code: ' + IntToStr(ErrorCode));
  ErrorCode := 0;
end;

procedure TfrmMain.btnClientSendCmdClick(Sender: TObject);
var
  i: integer;

  procedure SendCommand(ACmd, AParam: string; AClientId: integer);
  begin
    Clients[AClientId].Socket.SendText(ACmd + AParam + ' ' + CurrentTimeStr);
  end;
begin
  case cbbClientCmds.ItemIndex of
    //Send random number
    0: begin
      for i := 0 to High(Clients) do
      begin
        Randomize;
        SendCommand('RAND', IntToStr(Random(MAXWORD)), i);
      end;
    end;
    //Send command and wait random response
    1: begin
      for i := 0 to High(Clients) do
      begin
        Randomize;
        SendCommand('WAIT', IntToStr(Random(5000)), i);
      end;
    end;
    //Send command and request server time
    2: begin
      for i := 0 to High(Clients) do
      begin
        Randomize;
        SendCommand('RQST', '', i);
      end;
    end;
    //Send command and request clients count
    3: begin
      for i := 0 to High(Clients) do
      begin
        Randomize;
        SendCommand('CCNT', '', i);
      end;
    end;        
  end;
end;

end.

