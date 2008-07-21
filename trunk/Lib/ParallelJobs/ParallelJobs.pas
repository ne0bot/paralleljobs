unit ParallelJobs;

{***************************************************************************
 * ParallelJobs Library
 *@module ParallelJobs
 *@version 2008.0.0.5
 *@author Gilberto Saraiva - http://gsaraiva.projects.pro.br/
 *@copyright Copyright © 2008, DevPartners, Gilberto Saraiva
 *@homepage http://devpartners.projects.pro.br/forum/index.php?board=8.0
 *
 * License: MPL 1.1
 * http://www.mozilla.org/MPL/MPL-1.1.html
 *
 ***************************************************************************
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is ParallelJobs.pas, released: 04 july 2008.
 *
 * The Initial Developer of the Original Code is Gilberto Saraiva.
 * Portions created by Gilberto Saraiva are Copyright (C) 2008 -
 * Gilberto Saraiva. All Rights Reserved.
 *
 * Contributor(s): .
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * GNU General Public License Version 2 or later (the "GPL"), in which case
 * the provisions of the GPL are applicable instead of those above.
 * If you wish to allow use of your version of this file only under the terms
 * of the GPL and not to allow others to use your version of this file
 * under the MPL, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the GPL.
 * If you do not delete the provisions above, a recipient may use your version
 * of this file under either the MPL or the GPL.
 *
 ***************************************************************************
 *
 * You may retrieve the latest version of this file at the DevPartners Forum,
 * located at http://devpartners.projects.pro.br/forum/?board=8
 *
 ***************************************************************************}

interface

uses Windows;

type
  { Note: This thunk provide a link to SelfMode
  }
  PJobsThunk = ^TJobsThunk;
  TJobsThunk = packed record
    a  : Byte;
    aV : Pointer;
    b  : Byte;
    bV : Pointer;
    c  : Byte;
    cV : Integer;
  end;

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
  procedure WaitAllParallelJobsFinalization;

  function CurrentJobId: DWORD;
  function CurrentJobHandle: THandle;
  function CurrentJobTerminated: boolean;

type
  { Note: Jobs states for group control
  }
  TJobState = (jgsRunning, jgsStopped);

  { Note: Job Item holder
    Holds ParallelJobs as chained list
  }
  PJobItem = ^TJobItem;
  TJobItem = record
    Job  : Pointer;
    prev : PJobItem;
    next : PJobItem;
  end;

  { Note: JobsGroup
    Provide a control for all jobs on the group
  }
  PJobsGroup = ^TJobsGroup;
  TJobsGroup = class
  private
    FName        : string;
    FLastGroup   : TJobsGroup;
    FFirstJob    : PJobItem;
    FLastJob     : PJobItem;
    FJobsCount   : integer;
    FJobsHandles : array of THandle;
    FLock        : boolean;
    procedure AddJob(AJob: Pointer);
    procedure DelJob(AJob: Pointer; AInternalEnd: boolean = false);
  protected
    procedure UpdateHandles;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    procedure Clear;

    procedure InitJobCapture;
    procedure EndJobCapture;

    procedure StartJobs;
    procedure StopJobs(AForce: boolean = false);
    
    function JobsIsRunning: Integer;
    function WaitForJobs(AWaitAll: boolean; AMilliseconds: DWORD): Integer;

    property Name: string read FName;
  end;

implementation

uses Math;

type
  { Note: Safe Section holder type
  }
  TSafeSectionInfo = record
    Self      : Pointer;
    Target    : Pointer;
    SafeFlag  : Boolean;
    RefsCount : Integer;
  end;

var
  { Note: Safe Section list
  }
  SafeSectionInfo: array of TSafeSectionInfo;

{ Note: Get the Safe Section Boolean pointer from the list
}
function SafeSectionInfoFlag(ASelf, ATarget: Pointer): PBoolean;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to High(SafeSectionInfo) do
    with SafeSectionInfo[i] do
      if (ASelf = Self) and (ATarget = Target) then
      begin
        if RefsCount = 0 then
          SafeFlag := false;
        RefsCount := RefsCount + 1;  
        Result := @SafeFlag;
      end;
        
  if Result = nil then
  begin
    SetLength(SafeSectionInfo, Length(SafeSectionInfo) + 1);
    with SafeSectionInfo[High(SafeSectionInfo)] do
    begin
      Self := ASelf;
      Target := ATarget;
      SafeFlag := false;
      RefsCount := 1;
      Result := @SafeFlag;
    end;
  end;
end;

{$W-}
{ Note: basic lock system
  Lock bool var
}
procedure LockVar(var AVar: Boolean);
  function LockCheck(var AVar: Boolean): Boolean;
  asm
    MOV  ECX, EAX 
    MOV  AL,  False
    MOV  DL,  True
    LOCK CMPXCHG [ECX], DL
  end;
begin
  while LockCheck(AVar) do
  begin
    Sleep(0);
    if not LockCheck(AVar) then Break;
    Sleep(1);
  end;
end;

{ Note: Unlock bool var
}
procedure UnlockVar(var AVar: Boolean);
begin
  AVar := False;
end;
{$W+}

var
  { Note: Hold the Current Group, for add job to group
    on the job creator.
      if nil then job don't have group.
      if not job will be added to the current group.
  }
  CurrentGroup: TJobsGroup = nil;

{ Note: Setup the thunk.
  Specific hardcoded for paralleljobs needs
}
procedure SetupThunk(var AJobThunk: TJobsThunk; ASelf, ATarget, AParam: Pointer);
const
  DefThunkPath: array [0..1] of Int64 = (204509162766520, 15269888);
begin
  Move(DefThunkPath, AJobThunk, 15);
  with AJobThunk do
  begin
    aV := ASelf;
    bV := AParam;
    cV := Integer(ATarget) - Integer(@c) - 5;
  end;
end;

type
  { Note: Job call mode
      SelfMode use a Object with Target
      Direct call the Target directly
  }
  TParallelCallMode = (pcmSelfMode, pcmDirect);

  { Note: Call Terminate state
      1 = None
      2 = Terminate Requested
      3 = Terminated
  }
  TParallelCallTerminateState = Byte;

  PParallelCall = ^TParallelCall;
  TParallelCall = packed record
    Mode        : TParallelCallMode;
    Thunk       : TJobsThunk;
    Call        : Pointer;
    Param       : Pointer;
    Hnd         : THandle;
    ID          : DWORD;

    Group       : TJobsGroup;
    GState      : TJobState;

    SafeSection : Boolean;
    SafeFlag    : PBoolean;
    Terminated  : TParallelCallTerminateState;
  end;

  { Note: Jobs holder, to provide a base for jobs controls
  }
  PParallelJobHolder = ^TParallelJobHolder;
  TParallelJobHolder = record
    Job: PParallelCall;
    next: PParallelJobHolder;
  end;

var
  { Note:
      HolderLock:
        for lock manipulation on the list
      First and Last:
        for control the chained list
      Current:
        for improve speed on the job and reduce
        the search process cost. 
  }
  HolderLock: boolean = false;
  TerminateNullJob: boolean = false;
  FirstParallelJobHolder   : PParallelJobHolder = nil;
  LastParallelJobHolder    : PParallelJobHolder = nil;
  CurrentParallelJobHolder : PParallelJobHolder = nil;

  procedure TerminateParrallelJob(AParallelJob: PParallelCall;
    AForce: boolean = false); forward;

{ Note: Create the a holder for a Job
}
procedure CreateHolder(AParallelJob: PParallelCall);
var
  pNew: PParallelJobHolder;
begin
  LockVar(HolderLock);
  try
    New(pNew);
    pNew^.Job := AParallelJob;
    pNew^.next := nil;

    if FirstParallelJobHolder = nil then
    begin
      FirstParallelJobHolder := pNew;
      LastParallelJobHolder := pNew;
    end else
    begin
      LastParallelJobHolder^.next := pNew;
      LastParallelJobHolder := pNew;
    end;
  finally
    UnlockVar(HolderLock);
  end;
end;

{ Note: Destroy the Job holder
}
procedure DestroyHolder(AParallelJob: PParallelCall);
var
  pWalk, pLast: PParallelJobHolder;
begin
  LockVar(HolderLock);
  try
    pLast := nil;
    pWalk := FirstParallelJobHolder;
    while pWalk <> nil do
      if pWalk^.Job = AParallelJob then
      begin
        UnlockVar(HolderLock);
        try
          TerminateParrallelJob(pWalk^.Job);
        finally
          LockVar(HolderLock);
        end;

        if CurrentParallelJobHolder = pWalk then
          CurrentParallelJobHolder := nil;

        if pLast <> nil then
          pLast^.next := pWalk^.next;

        if pWalk = LastParallelJobHolder then
          LastParallelJobHolder := pLast;

        if pWalk = FirstParallelJobHolder then
          FirstParallelJobHolder := pWalk^.next;

        Dispose(pWalk);
        pWalk := nil;
      end else
      begin
        pLast := pWalk;
        pWalk := pWalk^.next;
      end;
  finally
    UnlockVar(HolderLock);
  end;
end;

{ Note: Terminate thread
    Forced: Suspend the thread, imediatly stop. Can leak memory
    Normal: Pass the terminate flag to on and CurrentJobTerminated will
      return true. 
}
procedure TerminateParrallelJob(AParallelJob: PParallelCall;
  AForce: boolean = false);
var
  hThread: THandle;
begin
  hThread := AParallelJob^.Hnd;
  if hThread <> 0 then
    if AForce then
      SuspendThread(hThread)
    else
    begin
      if TerminateNullJob then
        AParallelJob^.Hnd := 0;

      if AParallelJob^.Terminated = 0 then
      begin
        AParallelJob^.Terminated := 1;
        if AParallelJob^.GState = jgsStopped then
          ResumeThread(hThread);

        if TerminateNullJob then
          while AParallelJob^.Terminated = 1 do
            Sleep(0);
      end;
    end;
        
  TerminateNullJob := False;    
end;

{ Note: Count the number of All Jobs exists on the memory. 
}
function ParallelJobsCount: integer;
var
  pWalk: PParallelJobHolder;
begin
  Result := 0;
  LockVar(HolderLock);
  try
    pWalk := FirstParallelJobHolder;
    while pWalk <> nil do
    begin
      Result := Result + 1;
      pWalk := pWalk^.next;
    end;
  finally
    UnlockVar(HolderLock);
  end;
end;

{ Note: Terminate all jobs
}
procedure TerminateAllParallelJobs(AForce: boolean = false);
var
  pWalk: PParallelJobHolder;
begin
  LockVar(HolderLock);
  try
    pWalk := FirstParallelJobHolder;
    while pWalk <> nil do
    begin
      TerminateParrallelJob(pWalk^.Job);
      pWalk := pWalk^.next;
    end;
  finally
    UnlockVar(HolderLock);
  end;
end;

{ Note: Wait for all jobs finalization
}
procedure WaitAllParallelJobsFinalization;
begin
  while FirstParallelJobHolder <> nil do
    Sleep(0);
end;

{ Note: Return the Job holder by ID
}
function GetJobHolderById(AID: DWORD): PParallelJobHolder;
begin
  Result := CurrentParallelJobHolder;
  if (Result <> nil) and (Result^.Job^.ID = AID) then
    Exit;

  LockVar(HolderLock);
  try
    Result := FirstParallelJobHolder;
    while Result <> nil do
      if Result^.Job^.ID = AID then
        Break
      else
        Result := Result^.next;
    CurrentParallelJobHolder := Result;
  finally
    UnlockVar(HolderLock);
  end;
end;

{ Note: forwarding EndParallelJob to ParallelWorker auto free
}
procedure EndParrallelJob(AParallelJob: PParallelCall;
  AEnd: boolean = false); forward;

{ Note: Central operation point of ParallelJobs system.
}
function ParallelWorker(AParam: PParallelCall): Integer; stdcall;
asm
  call GetCurrentThreadId
  mov [ebx+$1C],eax
  mov al,[ebx+$25]
  cmp al,0
  jz @Init
@CheckSafeSection:
  mov ecx,[ebx+$26]
  mov al,[ecx]
  cmp al,0
  jnz @CheckSafeSection
  mov [ecx],1
@Init:
  mov al,[ebx]
  cmp al,0
  jz @SelfMode
  jmp @Direct
@SelfMode:
  lea ecx,[ebx+$01]
  call ecx
  mov Result,eax
  jmp @End
@Direct:
  mov ecx,[ebx+$10]
  mov eax,[ebx+$14]
  push eax
  call ecx
  mov Result,eax
  pop eax
@End:
  mov ecx,[ebx+$26]
  mov [ecx],0
  mov byte ptr [ebx+$2A],$02
  mov eax,AParam
  push eax
  call EndParrallelJob
  pop eax
end;

{ Note: Initialize the Job structure
}
function InitParallelJob(AMode: TParallelCallMode; ASelf, ATarget,
  AParam: Pointer; ASafeSection: boolean): PParallelCall;
var
  dwNull: DWORD;
begin
  New(Result);
  FillChar(Result^, SizeOf(TParallelCall), 0);

  if CurrentGroup <> nil then
    CurrentGroup.AddJob(Result);
  CreateHolder(Result);

  Result^.ID := 0;  
  Result^.Group := CurrentGroup;
  Result^.GState := jgsStopped;
  Result^.Mode := AMode;
  case AMode of
    pcmSelfMode : begin
      SetupThunk(Result^.Thunk, ASelf, ATarget, AParam);
    end;
    pcmDirect   : begin
      Result^.Call := ATarget;
      Result^.Param := AParam;
    end;
  end;
  Result^.SafeSection := ASafeSection;
  Result^.SafeFlag := SafeSectionInfoFlag(ASelf, ATarget);
  Result^.Terminated := 0;

  Result^.Hnd := CreateThread(nil, 0, @ParallelWorker,
    Result, CREATE_SUSPENDED, dwNull);
end;

type
  PParallelEnd = ^TParallelEnd;
  TParallelEnd = packed record
    ParallelCall : PParallelCall;
    Hnd          : THandle;
  end;

{ Note: Finalize the job structure and allocations
}
function ParallelEndParrallelJob(AParam: PParallelEnd): Integer; stdcall;
var
  hThread: THandle;
begin
  with AParam^ do
  begin
    if ParallelCall^.Group <> nil then
      ParallelCall^.Group.DelJob(ParallelCall, true);

    hThread := ParallelCall^.Hnd;
    TerminateNullJob := true;
    DestroyHolder(ParallelCall);
    ParallelCall^.Hnd := 0;
    
    Dispose(ParallelCall);
    CloseHandle(hThread);
  end;
  Dispose(AParam);
  Result := 0;
end;

{ Note: Call finalize on a other thread
}
procedure EndParrallelJob(AParallelJob: PParallelCall; AEnd: boolean = false);
var
  pNew: PParallelEnd;
  dwNull: DWORD;
begin
  if AParallelJob^.Hnd = 0 then
    Exit;
    
  New(pNew);
  pNew^.ParallelCall := AParallelJob;
  pNew^.Hnd := CreateThread(nil, 0, @ParallelEndParrallelJob,
    pNew, CREATE_SUSPENDED, dwNull);
  ResumeThread(pNew^.Hnd);
end;

{ Note: Basic use of ParallelJobs structure
  * SelfMode operation
  Initialize the Job as needed and check for group
  if have no group then start the job imediatly
}
procedure ParallelJob(ASelf: TObject; ATarget: Pointer;
  AParam: Pointer = nil; ASafeSection: boolean = false);
var
  pJob: PParallelCall;
begin
  pJob := InitParallelJob(pcmSelfMode, ASelf, ATarget, AParam, ASafeSection);
  if pJob^.Group = nil then
    ResumeThread(pJob^.Hnd);
end;

{ Note: Basic use of ParallelJobs structure
  * Direct operation
  Initialize the Job as needed and check for group
  if have no group then start the job imediatly
}
procedure ParallelJob(ATarget: Pointer; AParam: Pointer = nil;
  ASafeSection: boolean = false);
var
  pJob: PParallelCall;
begin
  pJob := InitParallelJob(pcmDirect, nil, ATarget, AParam, ASafeSection);
  if pJob^.Group = nil then
    ResumeThread(pJob^.Hnd);
end;

{ Note: Return the Current Job ID 
}
function CurrentJobId: DWORD;
begin
  Result := GetCurrentThreadId;
end;

{ Note: Return the Handle of the thread of the Job
}
function CurrentJobHandle: THandle;
var
  pHolder: PParallelJobHolder;
begin
  pHolder := GetJobHolderById(GetCurrentThreadId);
  if pHolder = nil then
    Result := INVALID_HANDLE_VALUE
  else
    Result := pHolder^.Job^.Hnd;
end;

{ Note: Check for terminate flag
}
function CurrentJobTerminated: boolean;
var
  pHolder: PParallelJobHolder;
begin
  pHolder := GetJobHolderById(GetCurrentThreadId);
  if pHolder = nil then
    Result := False
  else
    Result := pHolder^.Job^.Terminated > 0;
end;

{ TJobsGroup }

constructor TJobsGroup.Create(AName: string);
begin
  inherited Create;
  FLock := false;
  FName := AName;
  Clear;
end;

destructor TJobsGroup.Destroy;
begin
  Clear;
  inherited;
end;

{ Note: Lock work section
}
procedure TJobsGroup.Lock;
begin
  LockVar(FLock);
end;

{ Note: Unlock work section
}
procedure TJobsGroup.Unlock;
begin
  UnlockVar(FLock);
end;

{ Note: Ends and clean the Job list from Group
}
procedure TJobsGroup.Clear;
var
  pWalk, pPrev: PJobItem;
begin
  pWalk := FLastJob;
  while pWalk <> nil do
  begin
    pPrev := pWalk^.prev;
    EndParrallelJob(PParallelCall(pWalk^.Job));
    pWalk := pPrev;
  end;

  while FFirstJob <> nil do ;

  Lock;
  try
    FJobsCount := 0;
    FFirstJob := nil;
    FLastJob := nil;
    SetLength(FJobsHandles, 0);
  finally
    Unlock;
  end;  
end;

{ Note: UpdateHandles is needed for WaitForJobs method
}
procedure TJobsGroup.UpdateHandles;
var
  pWalk: PJobItem;
  i: integer;
begin
  Lock;
  try
    i := 0;
    SetLength(FJobsHandles, FJobsCount);
    pWalk := FFirstJob;
    while pWalk <> nil do
    begin
      FJobsHandles[i] := PParallelCall(pWalk^.Job)^.Hnd;
      i := i + 1;
      pWalk := pWalk^.next;
    end;
  finally
    Unlock;
  end;
end;

{ Note: Initialize the Jobs capture.
  Hold the last CurrentGroup and setup CurrentGroup as it self
}
procedure TJobsGroup.InitJobCapture;
begin
  FLastGroup := CurrentGroup;
  CurrentGroup := Self;
end;

{ Note: Finalize the Jobs capture.
  Rollback the CurrentGroup for the saved value on InitJobCapture
}
procedure TJobsGroup.EndJobCapture;
begin
  CurrentGroup := FLastGroup;
end;

{ Note: Internal add job to group
}
procedure TJobsGroup.AddJob(AJob: Pointer);
var
  pNew: PJobItem;
begin
  Lock;
  try
    New(pNew);
    pNew^.prev := nil;
    pNew^.next := nil;

    pNew^.Job := AJob;
    if FFirstJob = nil then
    begin
      FFirstJob := pNew;
      FLastJob := pNew;
    end else
    begin
      FLastJob^.next := pNew;
      pNew^.prev := FLastJob;
      FLastJob :=  pNew;
    end;
    FJobsCount := FJobsCount + 1;
  finally
    Unlock;
  end;
  UpdateHandles;
end;

{ Note: Internal remove job from Group
}
procedure TJobsGroup.DelJob(AJob: Pointer; AInternalEnd: boolean = false);
var
  pWalk: PJobItem;
begin
  Lock;
  try
    pWalk := FFirstJob;
    while pWalk <> nil do
      if pWalk^.Job = AJob then
      begin
        PParallelCall(pWalk^.Job)^.Group := nil;
        
        if not AInternalEnd then
          EndParrallelJob(PParallelCall(pWalk^.Job));

        if pWalk = FLastJob then
          FLastJob := pWalk^.prev;

        if pWalk = FFirstJob then
          FFirstJob := pWalk^.next;

        if pWalk^.prev <> nil then
          pWalk^.prev^.next := pWalk^.next;

        if pWalk^.next <> nil then
          pWalk^.next^.prev := pWalk^.prev;

        Dispose(pWalk);
        pWalk := nil;
        FJobsCount := FJobsCount - 1;
        Sleep(0);
      end else
        pWalk := pWalk^.next;
  finally
    Unlock;
  end;
  UpdateHandles;   
end;

{ Note: Start all jobs of the Group
}
procedure TJobsGroup.StartJobs;
var
  pWalk: PJobItem;
begin
  pWalk := FFirstJob;
  while pWalk <> nil do
  begin
    if PParallelCall(pWalk^.Job)^.GState = jgsStopped then
    begin
      ResumeThread(PParallelCall(pWalk^.Job)^.Hnd);
      PParallelCall(pWalk^.Job)^.GState := jgsRunning;
    end;

    pWalk := pWalk^.next;
  end;
  UpdateHandles;
end;

{ Note: Stop all jobs of the Group
}
procedure TJobsGroup.StopJobs(AForce: boolean = false);
var
  pWalk: PJobItem;
begin
  pWalk := FFirstJob;
  while pWalk <> nil do
  begin
    if PParallelCall(pWalk^.Job)^.GState = jgsRunning then
    begin
      SuspendThread(PParallelCall(pWalk^.Job)^.Hnd);
      PParallelCall(pWalk^.Job)^.GState := jgsStopped;
    end;
    pWalk := pWalk^.next;
  end;
  UpdateHandles;  
end;

{ Note: Return the count of jobs of the Group still running
}
function TJobsGroup.JobsIsRunning: Integer;
var
  pWalk: PJobItem;
begin
  Result := 0;
  pWalk := FFirstJob;
  while pWalk <> nil do
  begin
    if PParallelCall(pWalk^.Job)^.GState = jgsRunning then
      Result := Result + 1;
    pWalk := pWalk^.next;
  end;
end;

{ Note: Simplify the events capture from jobs thread
}
function TJobsGroup.WaitForJobs(AWaitAll: boolean; AMilliseconds: DWORD): Integer;
begin
  Result := WaitForMultipleObjects(Length(FJobsHandles), @FJobsHandles,
    AWaitAll, AMilliseconds);
end;

initialization

finalization
  TerminateAllParallelJobs;
  WaitAllParallelJobsFinalization;

end.
