unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ParallelJobs;

type
  THexadecimalList = class(TList)
  private
    function GetInt(Index: Integer): string;
    procedure AddInt(AInt: integer); 
  public
    property Hexadecimal[Index: Integer]: string read GetInt;
  end;

  TfrmMain = class(TForm)
    tckSize: TTrackBar;
    lblSize: TLabel;
    Label1: TLabel;
    lblTimer: TLabel;
    btnSearch: TButton;
    edtSearchItem: TEdit;
    tckThreads: TTrackBar;
    Label2: TLabel;
    lblThreads: TLabel;
    btnRand: TButton;
    procedure tckSizeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tckThreadsChange(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnRandClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ THexadecimalList }

function THexadecimalList.GetInt(Index: Integer): string;
begin
  Result := IntToHex(Integer(Items[Index]), 2);
end;

procedure THexadecimalList.AddInt(AInt: integer);
begin
  Add(Pointer(AInt));
end;

{ TfrmMain }

procedure TfrmMain.tckSizeChange(Sender: TObject);
begin
  lblSize.Caption := IntToStr(tckSize.Position);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  TerminateAllParallelJobs;
end;

procedure TfrmMain.tckThreadsChange(Sender: TObject);
begin
  lblThreads.Caption := IntToStr(tckThreads.Position);
end;

procedure TfrmMain.btnSearchClick(Sender: TObject);
type
  PSearchCtrl = ^TSearchCtrl;
  TSearchCtrl = packed record
    FIndex: integer;
    FList: THexadecimalList;
    FStart: integer;
    FLength: integer;
    FSeekFor: string;
    FFounded: PInteger;
  end;

  procedure SeekFor(AParam: PSearchCtrl);
  var
    i: integer;
  begin
    with AParam^ do
      for i := FStart to FStart + FLength do
        if FList.Hexadecimal[i] = FSeekFor then
        begin
          FFounded^ := FIndex;
          Break;
        end else if FFounded^ <> 0 then
          Break;

    Dispose(AParam);
  end;

var
  StartTime: Cardinal;
  HexIntList: THexadecimalList;
  i, iFounder, iCurr, iDiv: integer;
  Jobs: TJobsGroup;
  pParam: PSearchCtrl;
begin
  HexIntList := THexadecimalList.Create;
  Jobs := TJobsGroup.Create('MySearchs');
  try
    for i := HexIntList.Count to tckSize.Position - 1 do
      HexIntList.AddInt(i);

    iFounder := 0;

    iDiv := tckSize.Position div tckThreads.Position;
    iCurr := 0;
    for i := 0 to tckThreads.Position - 1 do
    begin
      if i = tckThreads.Position - 1 then
        iDiv := tckSize.Position - iCurr - 1;

      New(pParam);
      with pParam^ do
      begin
        FIndex := i + 1;
        FList := HexIntList;
        FStart := iCurr;
        FLength := iDiv - 1;
        FSeekFor := IntToHex(StrToInt(edtSearchItem.Text), 2);
        FFounded := @iFounder;
      end;
      ParallelJob(Jobs, @SeekFor, pParam);
      inc(iCurr, iDiv);
    end;

    StartTime := GetTickCount;
    Jobs.StartJobs;

    while iFounder = 0 do
      Sleep(0);
    
    lblTimer.Caption := IntToStr(GetTickCount - StartTime) +
      ' : Thread ' + IntToStr(iFounder);
  finally
    Jobs.Free;
    HexIntList.Free;
  end;
end;

procedure TfrmMain.btnRandClick(Sender: TObject);
begin
  edtSearchItem.Text := IntToStr(Random(tckSize.Position));
end;

end.

