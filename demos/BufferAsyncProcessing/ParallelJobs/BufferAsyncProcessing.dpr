program BufferAsyncProcessing;

uses
  Forms,
  pasMain in 'pasMain.pas' {frmMain},
  ParallelJobs in '..\..\..\Lib\ParallelJobs\ParallelJobs.pas';  

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffer Async Processing';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
