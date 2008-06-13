unit MonitorBug;

interface

uses Forms, WorkerThreadUnit, StupidHttp, ParamUnit, SysUtils, StrUtils,
  Classes, ShellAPI, Windows, ConfigUnit, EncodeUnit;

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
    FIsLogin: Boolean;
    FLastUpdateTime: TDateTime;
    FUnclosedBugCount: Integer;
    FUnresolvedBugCount: Integer;
    FMonitorBugStatue: TMonitorBugStatue;

    Http: TStupidHttp;
    FBugChangeNotify: TBugCountChangeNotify;
    FBugStatueChangeNotify: TBugStatueChangeNotify;
    FVer: string;

    FUnclosedBugList: TParamList;
    FUnresolvedBugList: TParamList;

    procedure SetStatue(Value: TMonitorBugStatue);
    function GetBugCount(Html: string; var BugCount: Integer): Boolean;
    function GetBugList(Html: string; var List: TParamList): Boolean;

  protected
    procedure Process; override;
    procedure AfterRun; override;

    function Login: Boolean;
    function GetBugDetail(var UnclosedBugCount, UnresolvedBugCount: Integer): Boolean;
    function Logout: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenIE(const Url: string);

    property Ver: string read FVer;

    property IsLogin: Boolean read FIsLogin;
    property LastUpdateTime: TDateTime read FLastUpdateTime;
    property UnclosedBugCount: Integer read FUnclosedBugCount;  // 未关闭bug
    property UnresolvedBugCount: Integer read FUnresolvedBugCount; // 未解决bug
    property Statue: TMonitorBugStatue read FMonitorBugStatue;

    property UnclosedBugList: TParamList read FUnclosedBugList;
    property UnresolvedBugList: TParamList read FUnresolvedBugList;

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

  FUnclosedBugList := TParamList.Create;
  FUnresolvedBugList := TParamList.Create;

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
  FreeAndNil(FUnclosedBugList);
  FreeAndNil(FUnresolvedBugList);
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

  if Config.BugFreeUrl = '' then Exit;
  if Config.UserName = '' then Exit;
  if Config.UserPWD = '' then Exit;

  Params := TParamList.Create;
  try
    Params.SetParam('${post}', 'xajax=xCheckUserLogin&xajaxr=1213254554968&xajaxargs[]=%3Cxjxquery%3E%3Cq%3ETestUserName%3D${username}%26TestUserPWD%3D${password}%26Language%3DZH_CN_UTF-8%26HttpRefer%3D%3C%2Fq%3E%3C%2Fxjxquery%3E');
    Params.SetParam('${username}', Config.UserName);
    Params.SetParam('${password}', Config.UserPWD);
    Html := Http.Post(Config.BugFreeUrl + '/Login.php', Params);
    
    StartIndex := Pos(SuccessFlag, Html);
    if StartIndex <= 0 then Exit;
  finally
    FreeAndNil(Params);
  end;
  Result := True;
end;

function TMonitorBug.GetBugDetail(var UnclosedBugCount, UnresolvedBugCount: Integer): Boolean;
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

var
  Html: string;

  function GetPost(PostParam: string): string;
  var
    Param: TParamList;
  begin
    Param := TParamList.Create;
    try
      Param.SetParam('${post}', PostParam);
      Param.SetParam('${username}', Config.UserName);
      Result := Http.Post(Config.BugFreeUrl + '/BugList.php', Param);
    finally
      FreeAndNil(Param);
    end;
  end;

begin
  Result := False;

  Html := GetPost(UnclosedBugCountParam);
  if not GetBugCount(Html, UnclosedBugCount) then Exit;
  GetBugList(Html, FUnclosedBugList);

  Html := GetPost(UnresolvedBugParam);
  if not GetBugCount(Html, UnresolvedBugCount) then Exit;
  GetBugList(Html, FUnresolvedBugList);

  Result := True;
end;

function TMonitorBug.GetBugCount(Html: string; var BugCount: Integer): Boolean;
var
  StartIndex: Integer;
  RecordStr: string;
begin
  Result := False;
  TParamList.FindSubStr(Html, '<td style="text-align:left;border:0;width:30%">'#13#10, #13#10, Html);
  StartIndex := Pos('/', Html);
  if StartIndex <= 0 then Exit;
  StartIndex := StartIndex + 1;
  RecordStr := MidStr(Html, StartIndex, MaxInt);
  BugCount := StrToIntDef(RecordStr, 0);
  Result := True;
end;

function TMonitorBug.GetBugList(Html: string; var List: TParamList): Boolean;
const
  NumFirstFlag = '<td align="center">' + #13#10 + '                  <nobr>';
  NumLastFlag = '</nobr>';
  PriFirstFlag = '<nobr>';
  PriLastFlag = '</nobr>';
  UrlFirstFlag = '<a href="';
  UrlLastFlag = '" title="';
  TitleFirstFlag = 'target="_blank"><nobr>';
  TitleLastFlag = '</nobr></a>';
var
  B, E: Integer;
  Url: string;
  Title: string;
begin
  Result := False;
  List.Clear;
  E := 0;
  while E < Length(Html) do
  begin
    B := PosEx(NumFirstFlag, Html, E + 1);
    if B = 0 then Break;
    B := B + Length(NumFirstFlag);
    E := PosEx(NumLastFlag, Html, B);
    Title := '[' + MidStr(Html, B, E - B) + ']';

    B := PosEx(PriFirstFlag, Html, E + 1) + Length(PriFirstFlag);
    E := PosEx(PriLastFlag, Html, B);
    Title := Title + '(' + MidStr(Html, B, E - B) + ')';
    
    B := PosEx(UrlFirstFlag, Html, E + 1) + Length(UrlFirstFlag);
    E := PosEx(UrlLastFlag, Html, B);
    Url := Config.BugFreeUrl + '/' + MidStr(Html, B, E - B);
    B := PosEx(TitleFirstFlag, Html, E + 1) + Length(TitleFirstFlag);
    E := PosEx(TitleLastFlag, Html, B);
    Title := Title + MidStr(Html, B, E - B);
    Title := Utf8ToAnsiString(Title, CP_ACP);
    List.SetParam(Title, Url);
  end;
  Result := True;
end;

function TMonitorBug.Logout: Boolean;
begin
  Result := False;

  if Config.BugFreeUrl = '' then Exit;

  try
    //todo Http.Get(Config.BugFreeUrl + '/Logout.php?Logout=Yes');
    Result := True;
  except
    // nothing
  end;
end;

procedure TMonitorBug.OpenIE(const Url: string);
begin
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOW);
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
      if (Config.BugFreeUrl = '') or (Config.UserName = '') or (Config.UserPWD = '') then
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
        if GetBugDetail(UnclosedBugCount, UnresolvedBugCount) then
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
        Http.SetIESessionCookie; // 设置IECookies，使得登录状态可以保持
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
  Logout;
  Http.ClearIESessionCookie;
end;

end.
