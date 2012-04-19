program HeavyUse;

uses
  Forms,
  pasMain in 'pasMain.pas' {frmMain},
  ParallelJobs in '..\..\lib\ParallelJobs\ParallelJobs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Heavy Use';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
