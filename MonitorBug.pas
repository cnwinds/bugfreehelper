unit MonitorBug;

interface

uses WorkerThreadUnit, IdHttp, SysUtils, StrUtils, Classes;

type

  TMonitorBugStatue = ( mbsOffline,     // 离线
                        mbsLogining,    // 正在登陆
                        mbsNormal,      // 登陆成功
                        mbsParamFail,   // 参数设置不正确
                        mbsLoginFail,   // 登陆失败
                        mbsHttpFail);   // 访问服务器失败

  TBugCountChangeNotify = procedure(Sender: TObject; OldCount, NewCount: Integer) of object;

  TBugStatueChangeNotify = procedure(Sender: TObject; OldStatue, NewStatue: TMonitorBugStatue) of object;

  TMonitorBug = class(TCustomWorkerThread)
  private
    FBugFreeUrl: string;
    FUserName: string;
    FUserPWD: string;

    FIsLogin: Boolean;
    FLastUpdateTime: TDateTime;
    FBugCount: Integer;
    FMonitorBugStatue: TMonitorBugStatue;

    Http: TIdHTTP;
    FBugChangeNotify: TBugCountChangeNotify;
    FBugStatueChangeNotify: TBugStatueChangeNotify;

    procedure SetStatue(Value: TMonitorBugStatue);
    function GetUpdateIntervalSec: Integer;
    procedure SetUpdateIntervalSec(const Value: Integer);

  protected
    procedure Process; override;
    procedure AfterRun; override;

    function Login: Boolean;
    function GetBugCount(var Count: Integer): Boolean;
    function Logout: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property BugFreeUrl: string read FBugFreeUrl write FBugFreeUrl;
    property UserName: string read FUserName write FUserName;
    property UserPWD: string read FUserPWD write FUserPWD;
    property UpdateIntervalSec: Integer read GetUpdateIntervalSec write SetUpdateIntervalSec;

    property IsLogin: Boolean read FIsLogin;
    property LastUpdateTime: TDateTime read FLastUpdateTime;
    property BugCount: Integer read FBugCount;
    property Statue: TMonitorBugStatue read FMonitorBugStatue;

    property BugChangeNotify: TBugCountChangeNotify read FBugChangeNotify write FBugChangeNotify;
    property BugStatueChangeNotify: TBugStatueChangeNotify read FBugStatueChangeNotify write FBugStatueChangeNotify;
  end;

implementation

{ TMonitorBug }

constructor TMonitorBug.Create;
begin
  inherited;

  Http := TIdHTTP.Create;
  Http.ProtocolVersion := pv1_1;

  // Bug更新时间
  SleepTime := 10 * 1000;
  FIsLogin := False;
  FBugCount := 0;
  
  SetStatue(mbsOffline);

  BugChangeNotify := nil;
  BugStatueChangeNotify := nil;
end;

destructor TMonitorBug.Destroy;
begin
  FreeAndNil(Http);
  inherited;
end;

function TMonitorBug.Login: Boolean;
const
  SuccessFlag = 'index.php';
var
  Url: string;
  Params: TStrings;
  Html: string;
  StartIndex: Integer;
begin
  Result := False;

  if BugFreeUrl = '' then Exit;
  if UserName = '' then Exit;
  if UserPWD = '' then Exit;

  Params := TStringList.Create;
  try
    Params.Add('BugUserName=' + UserName);
    Params.Add('BugUserPWD=' + UserPWD);
    Params.Add('Lang=ChineseUTF8');
    Params.Add('CssStyle=Default');
    Params.Add('HttpRefer=');
    Params.Add('ActionMode=Login');
    Url := BugFreeUrl + '/Login.php';
    Html := Http.Post(Url, Params);
    StartIndex := Pos(SuccessFlag, Html);
    if StartIndex <= 0 then Exit;
  finally
    FreeAndNil(Params);
  end;
  Result := True;
end;

function TMonitorBug.GetBugCount(var Count: Integer): Boolean;
const
  RecordStartFlag = 'title=''Record:';
  RecordEndFlag = #13#10;
  RecordEndFlag1 = #10;

var
  Url: string;
  Html: string;
  StartIndex, EndIndex: Integer;
  RecordStr: string;
begin
  Result := False;
  Url := BugFreeUrl + '/QueryBug.php?QueryMode=AssignedTo|' + UserName;
  Html := Http.Get(Url);
  StartIndex := Pos(RecordStartFlag, Html);
  if StartIndex <= 0 then Exit;

  Inc(StartIndex, Length(RecordStartFlag));
  EndIndex := PosEx(RecordEndFlag, Html, StartIndex);
  if EndIndex = 0 then
    EndIndex := PosEx(RecordEndFlag1, Html, StartIndex);

  RecordStr := Copy(Html, StartIndex, EndIndex - StartIndex);
  Count := StrToIntDef(RecordStr, 0);
  Result := True;
end;

function TMonitorBug.Logout: Boolean;
begin
  Result := False;

  if BugFreeUrl = '' then Exit;

  try
    Http.Get(BugFreeUrl + '/Logout.php?Logout=Yes');
    Result := True;
  except
    // nothing
  end;
end;

procedure TMonitorBug.Process;
var
  BugCount: Integer;
begin
  case FMonitorBugStatue of
    mbsOffline:
    begin
      SetStatue(mbsLogining);
      Process;
    end;
    mbsLogining:
    begin
      if (BugFreeUrl = '') or (UserName = '') or (UserPWD = '') then
      begin
        SetStatue(mbsParamFail);
        Exit;
      end;

      try
        if not Login then
          SetStatue(mbsLoginFail)
        else begin
          SetStatue(mbsNormal);
          Process;
        end;
      except
        SetStatue(mbsHttpFail);
      end;
    end;
    mbsNormal:
    begin
      try
        if GetBugCount(BugCount) then
        begin
          try
            if (FBugCount <> BugCount) and Assigned(BugChangeNotify) then
              BugChangeNotify(Self, FBugCount, BugCount);
          except
            // nothing
          end;
          FBugCount := BugCount;
          FLastUpdateTime := Now;
        end else
        begin
          Logout;
          SetStatue(mbsOffline);
        end;
      except
        SetStatue(mbsHttpFail);
      end;
    end;
    mbsParamFail:
    begin
      // nothing
    end;
    mbsLoginFail:
    begin
      // nothing
    end;
    mbsHttpFail:
    begin
      SetStatue(mbsOffline);
      Process;
    end;
  end;
end;

procedure TMonitorBug.SetStatue(Value: TMonitorBugStatue);
begin
  if Value <> FMonitorBugStatue then
  begin
    case Value of
      mbsOffline:
      begin
        FIsLogin := False;
      end;
      mbsLogining:
      begin
        // nothing
      end;
      mbsNormal:
      begin
        FIsLogin := True;
      end;
      mbsParamFail:
      begin
        FIsLogin := False;
      end;
      mbsLoginFail:
      begin
        FIsLogin := False;
      end;
      mbsHttpFail:
      begin
        FIsLogin := False;
      end;
    end;

    try
      if Assigned(FBugStatueChangeNotify) then
        FBugStatueChangeNotify(Self, FMonitorBugStatue, Value);
    except
      // nothing
    end;

    FMonitorBugStatue := Value;
  end;
end;

procedure TMonitorBug.AfterRun;
begin
//  Logout;
end;

function TMonitorBug.GetUpdateIntervalSec: Integer;
begin
  Result := SleepTime div 1000;
end;

procedure TMonitorBug.SetUpdateIntervalSec(const Value: Integer);
begin
  if SleepTime <> Value then
  begin
    SleepTime := Value * 1000;
  end;
end;

end.
