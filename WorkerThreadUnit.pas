unit WorkerThreadUnit;

{
  提供了方便使用的线程封装。

  Author: cnwinds
  Create Date: 2006-11-18
}

interface

uses Windows, Classes, SyncObjs, SysUtils;

type

{ TCustomWorkerThread }

  TCustomWorkerThread = class(TThread)
  private
    FIsRunning: Boolean;
    FEvent: TEvent;
    FSleepTime: Integer;
    procedure SetSleepTime(const Value: Integer);
    
  protected
    procedure Execute; override;

  protected
    procedure BeforeRun; virtual;
    procedure Process; virtual; abstract;
    procedure AfterRun; virtual;

    procedure BeforeKill; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure Join;
    procedure Kill;

    property IsRunning: Boolean read FIsRunning;
    property SleepTime: Integer read FSleepTime write SetSleepTime;
  end;

implementation

{ TCustomWorkerThread }

constructor TCustomWorkerThread.Create;
begin
  inherited Create(True);
  FIsRunning := False;
  FEvent := TEvent.Create(nil, False, False, IntToStr(Integer(Self)));
  FSleepTime := 10;
end;

destructor TCustomWorkerThread.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TCustomWorkerThread.Execute;
begin
  FIsRunning := True;
  BeforeRun;
  FEvent.ResetEvent;
  while FIsRunning do
  begin
    if not FIsRunning then Break;
    try
      Process;
    except
      // nothing
    end;
    if FSleepTime <> 0 then FEvent.WaitFor(FSleepTime);
  end;
  AfterRun;
end;

procedure TCustomWorkerThread.BeforeRun;
begin
  // nothing
end;

procedure TCustomWorkerThread.AfterRun;
begin
  // nothing
end;

procedure TCustomWorkerThread.BeforeKill;
begin
  // nothing;
end;

procedure TCustomWorkerThread.SetSleepTime(const Value: Integer);
begin
  if Value <> FSleepTime then
  begin
    FSleepTime := Value;
    FEvent.SetEvent;
  end;
end;

procedure TCustomWorkerThread.Start;
begin
  Self.Resume;
end;

procedure TCustomWorkerThread.Stop;
begin
  FIsRunning := False;
  FEvent.SetEvent;
end;

procedure TCustomWorkerThread.Join;
begin
  Self.WaitFor;
end;

procedure TCustomWorkerThread.Kill;
begin
  BeforeKill;
{$IFDEF MSWINDOWS}
  TerminateThread(Handle, 0);
{$ENDIF}
{$IFDEF LINUX}
  //todo kill thread
{$ENDIF}
end;

end.
