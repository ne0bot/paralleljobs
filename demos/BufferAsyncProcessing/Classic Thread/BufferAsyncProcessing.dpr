program BufferAsyncProcessing;

uses
  Forms,
  pasMain in 'pasMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffer Async Processing';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
