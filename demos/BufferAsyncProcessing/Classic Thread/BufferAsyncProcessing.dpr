program BufferAsyncProcessing;

uses
  Forms,
  pasMain in 'pasMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
