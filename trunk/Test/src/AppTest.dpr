program AppTest;

uses
  ParallelJobs in '..\..\Lib\ParallelJobs\ParallelJobs.pas',
  FastMM4,
  Forms,
  pasMain in 'pasMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AppTest';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
