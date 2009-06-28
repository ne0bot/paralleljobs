unit pasMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  THexadecimalList = class(TList)
  private
    function GetInt(Index: Integer): string;
    procedure AddInt(AInt: integer); 
  public
    property Hexadecimal[Index: Integer]: string read GetInt;
  end;

  TSearchCtrl = class(TThread)
  private
    FIndex: integer;
    FList: THexadecimalList;
    FStart: integer;
    FLength: integer;
    FSeekFor: string;
    FFounded: PInteger;
    constructor Create(AIndex, AStart, ALength: Integer;
      ASeekFor: string; AFoundFlag: PInteger; AList: THexadecimalList);
  public
    procedure Execute; override;
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
    procedure tckThreadsChange(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnRandClick(Sender: TObject);
  private
    { Private declarations }
  public

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

{ TSearchCtrl }

constructor TSearchCtrl.Create(AIndex, AStart, ALength: Integer;
  ASeekFor: string; AFoundFlag: PInteger; AList: THexadecimalList);
begin
  FIndex := AIndex;
  FList := AList;
  FFounded := AFoundFlag;
  FStart := AStart;
  FLength := ALength;
  FSeekFor := ASeekFor;
  FreeOnTerminate := false;
  inherited Create(true); 
end;

procedure TSearchCtrl.Execute;
var
  i: integer;
begin
  for i := FStart to FStart + FLength do
    if FList.Hexadecimal[i] = FSeekFor then
    begin
      FFounded^ := FIndex;
      Break;
    end else if FFounded^ <> 0 then
      Break;
      
  Terminate;
end;

{ TfrmMain }

procedure TfrmMain.tckSizeChange(Sender: TObject);
begin
  lblSize.Caption := IntToStr(tckSize.Position);
end;

procedure TfrmMain.tckThreadsChange(Sender: TObject);
begin
  lblThreads.Caption := IntToStr(tckThreads.Position);
end;

procedure TfrmMain.btnSearchClick(Sender: TObject);
var
  StartTime: Cardinal;
  HexIntList: THexadecimalList;
  Threads: array of TSearchCtrl;
  i, iFounder, iCurr, iDiv: integer;
begin
  HexIntList := THexadecimalList.Create;
  try
    for i := HexIntList.Count to tckSize.Position do
      HexIntList.AddInt(i);

    SetLength(Threads, tckThreads.Position);
    iFounder := 0;

    iDiv := tckSize.Position div tckThreads.Position;
    iCurr := 0;
    for i := 0 to High(Threads) do
    begin
      if i = High(Threads) then
        iDiv := tckSize.Position - iCurr;
      Threads[i] := TSearchCtrl.Create(i + 1, iCurr, iDiv - 1,
        IntToHex(StrToInt(edtSearchItem.Text), 2), @iFounder, HexIntList);
      inc(iCurr, iDiv);
    end;

    StartTime := GetTickCount;
    for i := 0 to High(Threads) do
      Threads[i].Resume;

    while iFounder = 0 do
      Sleep(0);

    lblTimer.Caption := IntToStr(GetTickCount - StartTime) +
      ' : Thread ' + IntToStr(iFounder);

    for i := 0 to High(Threads) do
      Threads[i].Free;
  finally
    HexIntList.Free;  
  end;
end;

procedure TfrmMain.btnRandClick(Sender: TObject);
begin
  edtSearchItem.Text := IntToStr(Random(tckSize.Position));
end;

end.

