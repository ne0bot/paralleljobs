#Start using ParallelJobs on your application.

# Introduction #

With the Unit ParallelJobs.pas linked in your project, you can start create a simple-to-advanced structure to do a parallel process.

As we know, a parallel process requests a basic knowledge synchronism, execution and semaphores. So, before start with PJ, it's higly recommended that you need this knowledges.

# Details #

Continuing about some approaches you can do to turn your application more efficient, PJ provides a simple-to-use way to create a parallel process.

Below a simple parallel process to keep the windows refreshing the caption with the current time.
```
procedure TfrmMain.CaptionClock;
begin
  while not CurrentJobTerminated do
  begin
    Caption := TimeToStr(Now);
    Sleep(1000);
  end;
end;

procedure TfrmMain.btnActiveClockClick(Sender: TObject);
begin
  ParallelJob(Self, @TfrmMain.CaptionClock);
end;
```

CaptionClock is a simple method of TfrmMain that works every second seting up the caption with the current time. Note that the CaptionClock method can access the frmMain like a default method, so PJ creates the possibility to turn call to a method to a parallel call. Let's see what that mean.

```
procedure TfrmMain.CaptionClock;
begin
  Caption := 'Modified Caption';
end;

procedure TfrmMain.btnActiveClockClick(Sender: TObject);
begin
  Caption := 'Caption';
  CaptionClock;
  if Caption = 'Caption' then
    ShowMessage('1º test - Caption not modified');

  Caption := 'Caption';
  ParallelJob(frmMain, @TfrmMain.CaptionClock);
  if Caption = 'Caption' then
    ShowMessage('2º test - Caption not modified');
end;
```

Only the 2nd ShowMessage will be called because CaptionClock will only make effect on the caption when internal caption assignment process ends.

Going more deep on parallel process and specialy with Delphi VCL structure, note that Caption can be access without a semaphore, but sometimes you'll need to sync the execution with the main thread to avoid concurrent access to a object or method that raises a AV (Access Violantion) Lets see a example that make use of main thread sync.

```
var
  sText: string;

procedure TfrmMain.TurnStringTo10;
begin
  while not CurrentJobTerminated do
  begin
    { Set String to 10 chars | btnTurnTo20Clickloop can run concurrent and raise a AV }
    SetLength(sText, 10);
    Sleep(0);
  end;
end;

{ Button Click }
procedure TfrmMain.btnTurnTo20Click(Sender: TObject);
var
  i: integer;
begin
  ParallelJob(frmMain, @TfrmMain.TurnStringTo10);
  for i := 1 to 20 do
  begin
    { Set String to 20 chars | TurnStringTo10 can run concurrent and raise a AV }
    SetLength(sText, 20); 
    { TurnStringTo10 can run and set it to 10 raising a Range Check Error }
    sText[i] := Char(Random(27) + $41);
    Sleep(1);
  end;
  Caption := sText;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  TerminateAllParallelJobs;
end;
```

A Range Check Error or a AV can raise on this code, so, to avoid this problem we need to create a semaphore establishing a critical section where needed. The code below show how we need to work to avoid problems with concurrent execution:
```
var
  sText: string;
  CS: TRTLCriticalSection;

procedure TfrmMain.TurnStringTo10;
begin
  while not CurrentJobTerminated do
  begin
    EnterCriticalSection(CS); { Enter Critical Section }
    try
      SetLength(sText, 10);
    finally
      LeaveCriticalSection(CS);
    end;
    Sleep(0);
  end;
end;

{ Button Click }
procedure TfrmMain.btnTurnTo20Click(Sender: TObject);
var
  i: integer;
begin
  InitializeCriticalSection(CS);

  ParallelJob(frmMain, @TfrmMain.TurnStringTo10);
  for i := 1 to 20 do
  begin
    EnterCriticalSection(CS); { Enter Critical Section }
    try
      SetLength(sText, 20);
      sText[i] := Char(Random(27) + $41);
    finally
      LeaveCriticalSection(CS);
    end;
    Sleep(1);
  end;
  Caption := sText;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  TerminateAllParallelJobs;
end;
```

That's the basic use of ParallelJobs, more can be coded, more can be improved.

Any Issues:
http://code.google.com/p/paralleljobs/issues/list

Thanks,
Gilberto Saraiva