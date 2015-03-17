# ParallelJobs Library #

ParallelJobs is a library that provide a easy way to create a parallel process on your application.

Basic functions of ParallelJobs:
```
  { Note: Basic use of ParallelJobs structure
    * SelfMode operation
      @ASelf = Object that contain the target
      @ATarget = The target to be called when job run
      @AParam = The pointer that will be pass as param to target
      @ASafeSection = Auto safe section control managed by ParallelJobs.
  }
  procedure ParallelJob(ASelf: TObject; ATarget: Pointer;
    AParam: Pointer = nil; ASafeSection: boolean = false); overload;

  { Note: Basic use of ParallelJobs structure
    * Direct operation
      @ATarget = The target to be called when job run
      @AParam = The pointer that will be pass as param to target
      @ASafeSection = Auto safe section control managed by ParallelJobs.
  }
  procedure ParallelJob(ATarget: Pointer; AParam: Pointer = nil;
    ASafeSection: boolean = false); overload;

  function ParallelJobsCount: integer;

  procedure TerminateAllParallelJobs(AForce: boolean = false);

type
  TWaitProcessNotify = procedure of object;

  procedure WaitAllParallelJobsFinalization(AWaitNotify: TWaitProcessNotify = nil);

  function CurrentJobId: DWORD;
  function CurrentJobHandle: THandle;
  function CurrentJobTerminated: boolean;
```

Works like a simple Win32 API call, as you can see below:
```delphi

ParallelJob(@MyProcess);```

Using object call:
```delphi

procedure TForm.RunJob;
begin
ParallelJob(Self, @TForm.MyProcess);
end;```
This will create a parallel process that will execute MyProcess function.

All controls and utilities are being implemented and still on development

**Why use paralleljobs?**

ParallelJobs on the current stage provide a very easy way to create a parallel process, like you can create using TThread, but without writing a lots of code.

# Simple DOS example #
```
program DOS;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  ParallelJobs in 'Lib\ParallelJobs\ParallelJobs.pas';

var
  { Note: Global Critical Section to avoid concurrent access to StepCount array
  }
  CS: TRTLCriticalSection;
  StepCount: array of integer;

const
  Coord: TCoord = (X: 0; Y: 0);
  SIZE_MAX = 50;

procedure StepToMax(Param: Longint);
begin
  while True do
  begin
    { Note: Enter CS area
    }
    EnterCriticalSection(CS);
    try
      StepCount[LoWord(Param)] := StepCount[LoWord(Param)] + 1;
      if StepCount[LoWord(Param)] = SIZE_MAX then
        Break;
    finally
      LeaveCriticalSection(CS);
    end;
    Sleep(HiWord(Param));
  end;
end;

var
  i: integer;
  sLine: string;
  dwNull: DWORD;
begin
  InitializeCriticalSection(CS);

  Randomize;

  SetLength(StepCount, 10);
  for i := 0 to High(StepCount) do
    { Note: Create a ParallelJob of StepToMax, pass as param the Index and a Random number to be the sleep time
          :: Simplified with MakeLong
    }
    ParallelJob(@StepToMax, Pointer(MakeLong(i, Random(255))));

  while true do
  begin
    FillConsoleOutputCharacter(GetStdHandle(STD_OUTPUT_HANDLE), ' ', 46*16,  Coord, dwNull);
    SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), Coord);

    WriteLn('DOS Threads :: http://code.google.com/p/paralleljobs/');

    for i := 0 to High(StepCount) do
    begin
      { Note: Enter CS area
      }
      EnterCriticalSection(CS);
      try
        sLine := StringOfChar('#', StepCount[i]);
      finally
        LeaveCriticalSection(CS);
      end;

      sLine := IntToStr(i) + ': ' + sLine + StringOfChar(' ', SIZE_MAX - Length(sLine)) +
          ': ' + IntToStr(StepCount[i]);

      Writeln(sLine);
    end;

    Sleep(100);
  end;
end.
```

# UseParallelJob #
http://code.google.com/p/paralleljobs/wiki/UseParallelJob


# Demos #
  * http://code.google.com/p/paralleljobs/wiki/SocketCom

  * http://code.google.com/p/paralleljobs/wiki/IndexSearch

  * http://code.google.com/p/paralleljobs/wiki/ProcessOnTheFly

  * http://code.google.com/p/paralleljobs/wiki/DOS

  * http://code.google.com/p/paralleljobs/wiki/HeavyUse

  * http://code.google.com/p/paralleljobs/wiki/BufferAsyncProcessing