program DOS;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  ParallelJobs in '..\..\Lib\ParallelJobs\ParallelJobs.pas';

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
