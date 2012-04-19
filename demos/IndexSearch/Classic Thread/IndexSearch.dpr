program IndexSearch;

uses
  Forms,
  pasMain in 'pasMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Index Searching';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
