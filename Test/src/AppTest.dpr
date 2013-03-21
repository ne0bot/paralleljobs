program AppTest;

uses
  FastMM4,
  ParallelJobs in '..\..\Lib\ParallelJobs\ParallelJobs.pas',  
  Forms,
  pasMain in 'pasMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AppTest';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
