program AppTest;

uses
  Fastmm4,
  Forms,
  pasMain in 'pasMain.pas' {frmMain},
  ParallelJobs in '..\..\Lib\ParallelJobs\ParallelJobs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AppTest';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
