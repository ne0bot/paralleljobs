program ProcessOnTheFly;

uses
  Forms,
  pasMain in 'pasMain.pas' {frmMain},
  ParallelJobs in '..\..\Lib\ParallelJobs\ParallelJobs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Process On The Fly';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
