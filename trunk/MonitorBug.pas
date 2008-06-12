unit MonitorBug;

interface

uses Forms, WorkerThreadUnit, StupidHttp, ParamUnit, SysUtils, StrUtils, Classes;

type

  TMonitorBugStatue = ( mbsOffline,     // 离线
                        mbsLogining,    // 正在登陆
                        mbsNormal,      // 登陆成功
                        mbsParamFail,   // 参数设置不正确
                        mbsLoginFail,   // 登陆失败
                        mbsHttpFail);   // 访问服务器失败

  TBugCountChangeNotify = procedure(Sender: TObject; OldUnclosedBugCount, NewUnclosedBugCount: Integer;
                                    OldUnresolvedBugCount, NewUnresolvedBugCount: Integer) of object;

  TBugStatueChangeNotify = procedure(Sender: TObject; OldStatue, NewStatue: TMonitorBugStatue) of object;

  TMonitorBug = class(TCustomWorkerThread)
  private
    FBugFreeUrl: string;
    FUserName: string;
    FUserPWD: string;

    FIsLogin: Boolean;
    FLastUpdateTime: TDateTime;
    FUnclosedBugCount: Integer;
    FUnresolvedBugCount: Integer;
    FMonitorBugStatue: TMonitorBugStatue;

    Http: TStupidHttp;
    FBugChangeNotify: TBugCountChangeNotify;
    FBugStatueChangeNotify: TBugStatueChangeNotify;
    FVer: string;

    procedure SetStatue(Value: TMonitorBugStatue);
    function GetUpdateIntervalSec: Integer;
    procedure SetUpdateIntervalSec(const Value: Integer);

  protected
    procedure Process; override;
    procedure AfterRun; override;

    function Login: Boolean;
    function GetBugCount(var UnclosedBugCount, UnresolvedBugCount: Integer): Boolean;
    function Logout: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property Ver: string read FVer write FVer;
    property BugFreeUrl: string read FBugFreeUrl write FBugFreeUrl;
    property UserName: string read FUserName write FUserName;
    property UserPWD: string read FUserPWD write FUserPWD;
    property UpdateIntervalSec: Integer read GetUpdateIntervalSec write SetUpdateIntervalSec;

    property IsLogin: Boolean read FIsLogin;
    property LastUpdateTime: TDateTime read FLastUpdateTime;
    property UnclosedBugCount: Integer read FUnclosedBugCount;  // 未关闭bug
    property UnresolvedBugCount: Integer read FUnresolvedBugCount; // 未解决bug
    property Statue: TMonitorBugStatue read FMonitorBugStatue;

    property BugChangeNotify: TBugCountChangeNotify read FBugChangeNotify write FBugChangeNotify;
    property BugStatueChangeNotify: TBugStatueChangeNotify read FBugStatueChangeNotify write FBugStatueChangeNotify;
  end;

implementation

{ TMonitorBug }

constructor TMonitorBug.Create;
begin
  inherited;
  FVer := 'For BugFree2.0(RTM)';
  Http := TStupidHttp.Create(FVer);

  // Bug更新时间
  SleepTime := 10 * 1000;
  FIsLogin := False;
  FUnclosedBugCount := 0;
  FUnresolvedBugCount := 0;
  
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
  Params: TParamList;
  Html: string;
  StartIndex: Integer;
begin
  Result := False;

  if BugFreeUrl = '' then Exit;
  if UserName = '' then Exit;
  if UserPWD = '' then Exit;

  Params := TParamList.Create;
  try
    Params.SetParam('${post}', 'xajax=xCheckUserLogin&xajaxr=1213254554968&xajaxargs[]=%3Cxjxquery%3E%3Cq%3ETestUserName%3D${username}%26TestUserPWD%3D${password}%26Language%3DZH_CN_UTF-8%26HttpRefer%3D%3C%2Fq%3E%3C%2Fxjxquery%3E');
    Params.SetParam('${username}', UserName);
    Params.SetParam('${password}', UserPWD);
    Html := Http.Post(BugFreeUrl + '/Login.php', Params);
    
    StartIndex := Pos(SuccessFlag, Html);
    if StartIndex <= 0 then Exit;
  finally
    FreeAndNil(Params);
  end;
  Result := True;
end;

function TMonitorBug.GetBugCount(var UnclosedBugCount, UnresolvedBugCount: Integer): Boolean;
const
  UnclosedBugCountParam = 'AutoComplete=on&AndOr0=And&Field0=ProjectName&Operator0=%3D&' +
  'Value0=&AndOrGroup=AND&AndOr1=And&Field1=OpenedBy&Operator1=%3D&Value1=&' +
  'AndOr2=And&Field2=ModulePath&Operator2=LIKE&Value2=&AndOr3=And&' +
  'Field3=AssignedTo&Operator3=%3D&Value3=${username}&AndOr4=And&' +
  'Field4=BugID&Operator4=%3D&Value4=&AndOr5=And&Field5=BugTitle&' +
  'Operator5=LIKE&Value5=&PostQuery=%E6%8F%90%E4%BA%A4%E6%9F%A5%E8%AF%A2%E5%86%85%E5%AE%B9&QueryType=Bug';

  UnresolvedBugParam = 'AutoComplete=on&AndOr0=And&Field0=BugStatus&Operator0=%3D&' +
  'Value0=Active&AndOrGroup=AND&AndOr1=And&Field1=OpenedBy&Operator1=%3D&Value1=&' +
  'AndOr2=And&Field2=ModulePath&Operator2=LIKE&Value2=&AndOr3=And&' +
  'Field3=AssignedTo&Operator3=%3D&Value3=${username}&AndOr4=And&' +
  'Field4=BugID&Operator4=%3D&Value4=&AndOr5=And&Field5=BugTitle&' +
  'Operator5=LIKE&Value5=&PostQuery=%E6%8F%90%E4%BA%A4%E6%9F%A5%E8%AF%A2%E5%86%85%E5%AE%B9&QueryType=Bug';

  function GetBugCount(PostParam: string; var BugCount: Integer): Boolean;
  var
    Param: TParamList;
    Html: string;
    StartIndex: Integer;
    RecordStr: string;
  begin
    Result := False;
    Param := TParamList.Create;
    try
      Param.SetParam('${post}', PostParam);
      Param.SetParam('${username}', UserName);
      Html := Http.Post(BugFreeUrl + '/BugList.php', Param);
      TParamList.FindSubStr(Html, '<td style="text-align:left;border:0;width:30%">'#13#10, #13#10, Html);
      StartIndex := Pos('/', Html);
      if StartIndex <= 0 then Exit;
      StartIndex := StartIndex + 1;
      RecordStr := MidStr(Html, StartIndex, MaxInt);
      BugCount := StrToIntDef(RecordStr, 0);
      Result := True;
    finally
      FreeAndNil(Param);
    end;
  end;
  
begin
  if GetBugCount(UnclosedBugCountParam, UnclosedBugCount) and
    GetBugCount(UnresolvedBugParam, UnresolvedBugCount) then
    Result := True
  else
    Result := False;
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
  UnclosedBugCount, UnresolvedBugCount: Integer;
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
        if GetBugCount(UnclosedBugCount, UnresolvedBugCount) then
        begin
          try
            if ((FUnclosedBugCount <> UnclosedBugCount) or (FUnresolvedBugCount <> UnresolvedBugCount)) then
              if Assigned(BugChangeNotify) then
                BugChangeNotify(Self, FUnclosedBugCount, UnclosedBugCount, FUnresolvedBugCount, UnresolvedBugCount);
          except
            // nothing
          end;
          FUnclosedBugCount := UnclosedBugCount;
          FUnresolvedBugCount := UnresolvedBugCount;
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
