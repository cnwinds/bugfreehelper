unit MonitorBug;

interface

uses Forms, WorkerThreadUnit, StupidHttp, ParamUnit, SysUtils, StrUtils,
  Classes, ConfigUnit, ConvertCharset, IdURI;

type

  TMonitorBug = class;
  TBugPatter = class;

  TMonitorBugStatue = ( mbsOffline,     // 离线
                        mbsLogining,    // 正在登陆
                        mbsNormal,      // 登陆成功
                        mbsParamFail,   // 参数设置不正确
                        mbsLoginFail,   // 登陆失败
                        mbsHttpFail);   // 访问服务器失败

  TBugCountChangeNotify = procedure(Sender: TObject;
    BugPatter: TBugPatter; OldBugCount, NewBugCount: Integer) of object;

  TBugStatueChangeNotify = procedure(Sender: TObject; OldStatue, NewStatue: TMonitorBugStatue) of object;

  { TBugUpdateState - Bug数更新的状态 }
  
  TBugUpdateState = (busNotUpdate, busUpdateFail, busUpdateSuccess);

  { TBugPatter - 每种Bug数获取的方式 }
  
  TBugPatter = class(TObject)
  public
    function GetUpdateState: TBugUpdateState; virtual; abstract;
    function GetBugCount: Integer; virtual; abstract;
    function GetBugDesc: string; virtual; abstract;
    function GetBugList: TParamList; virtual; abstract;

    function UpdateBugInfo: Boolean; virtual; abstract;
  end;

  { TBugPatterList }
  
  TBugPatterList = class(TObject)
  private
    FList: TList;

    function GetItems(I: Integer): TBugPatter;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Add(BP: TBugPatter);
    procedure Clear;
    function GetCount: Integer;

    property Items[I: Integer]: TBugPatter read GetItems; default;
    property Count: Integer read GetCount;
  end;

  TUnclosedBugPatter = class(TBugPatter)
  private
    FMB: TMonitorBug;
    FCount: Integer;
    FBugList: TParamList;
    FBugUpdateState: TBugUpdateState;

    function UpdateBugCount(Html: string; var BugCount: Integer): Boolean;
    function UpdateBugList(Html: string; var List: TParamList): Boolean;

  public
    constructor Create(MB: TMonitorBug);
    destructor Destroy; override;

    function GetUpdateState: TBugUpdateState; override;

    function GetBugCount: Integer; override;
    function GetBugDesc: string; override;
    function GetBugList: TParamList; override;

    function UpdateBugInfo: Boolean; override;
  end;

  { TMonitorBug }
  
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

    FBugPatterList: TBugPatterList;

    procedure SetStatue(Value: TMonitorBugStatue);

  protected
    procedure Process; override;
    procedure AfterRun; override;

    function Login: Boolean;
    function UpdateBugInfo: Boolean;
    function Logout: Boolean;
    function EncodeParam(Param: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    function GetIndexPageUrl: string;

    property Ver: string read FVer;

    property IsLogin: Boolean read FIsLogin;
    property LastUpdateTime: TDateTime read FLastUpdateTime;
    property Statue: TMonitorBugStatue read FMonitorBugStatue;
    property BugPatterList: TBugPatterList read FBugPatterList;

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

  FBugPatterList := TBugPatterList.Create;

  FBugPatterList.Add(TUnclosedBugPatter.Create(Self)); // 加入需要显示的Bug

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
  FreeAndNil(FBugPatterList);
  inherited;
end;

function TMonitorBug.EncodeParam(Param: string): string;
const
  UnsafeChar = '@#$%^&+=[]\;,/{}|:"<>?`';
var                       
  I, J: Integer;

  function CharIsInSet(c: Char; S: string): Integer;
  begin
    Result := Pos(c, S);
  end;
begin
  for I := 1 to Length(Param) do
  begin
    J := CharIsInSet(Param[I], UnsafeChar);
    if J <> 0 then
      Result := Result + '%25' + IntToHex(Ord(Param[i]), 2)
    else
      Result := Result + Param[I];
  end;
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
    Params.SetParam('${username}', EncodeParam(Config.UserName));
    Params.SetParam('${password}', EncodeParam(Config.UserPWD));
    Html := Http.Post(Config.BugFreeUrl + '/Login.php', Params);
    
    StartIndex := Pos(SuccessFlag, Html);
    if StartIndex <= 0 then Exit;
  finally
    FreeAndNil(Params);
  end;
  Result := True;
end;

function TMonitorBug.UpdateBugInfo: Boolean;
var
  I: Integer;
  Bug: TBugPatter;
  OldCount: Integer;
begin
  for I := 0 to FBugPatterList.Count - 1 do
  begin
    Bug := FBugPatterList[I];
    OldCount := Bug.GetBugCount;
    if Bug.UpdateBugInfo then
    begin
      try
        if (OldCount <> Bug.GetBugCount) and Assigned(BugChangeNotify) then
          BugChangeNotify(Self, Bug, OldCount, Bug.GetBugCount);
      except
        // nothing
      end;
    end;
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

function TMonitorBug.GetIndexPageUrl: string;
begin
  Result := Config.BugFreeUrl + '/' + 'index.php';
end;

procedure TMonitorBug.Process;
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
        if UpdateBugInfo then
        begin
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

{ TUnclosedBugPatter }

constructor TUnclosedBugPatter.Create(MB: TMonitorBug);
begin
  FMB := MB;
  FBugList := TParamList.Create;
  FBugUpdateState := busNotUpdate;
end;

destructor TUnclosedBugPatter.Destroy;
begin
  FreeAndNil(FBugList);
  inherited;
end;

function TUnclosedBugPatter.GetBugCount: Integer;
begin
  Result := FCount;
end;

function TUnclosedBugPatter.GetBugDesc: string;
begin
  Result := '未关闭Bug';
end;

function TUnclosedBugPatter.GetBugList: TParamList;
begin
  Result := FBugList;
end;

function TUnclosedBugPatter.GetUpdateState: TBugUpdateState;
begin
  Result := FBugUpdateState;
end;

function TUnclosedBugPatter.UpdateBugInfo: Boolean;
const
  UnclosedBugCountParam = 'AutoComplete=on&AndOr0=And&Field0=ProjectName&Operator0=%3D&' +
  'Value0=&AndOrGroup=AND&AndOr1=And&Field1=OpenedBy&Operator1=%3D&Value1=&' +
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
      Result := FMB.Http.Post(Config.BugFreeUrl + '/BugList.php', Param);
    finally
      FreeAndNil(Param);
    end;
  end;

begin
  Html := GetPost(UnclosedBugCountParam);
  try
    Result := UpdateBugCount(Html, FCount);
    if not Result then raise Exception.Create('');
    Result := UpdateBugList(Html, FBugList);
    if not Result then raise Exception.Create('');
    FBugUpdateState := busUpdateSuccess;
  except
    FBugUpdateState := busUpdateFail;
    Result := False;
    Exit;
  end;
end;

function TUnclosedBugPatter.UpdateBugCount(Html: string; var BugCount: Integer): Boolean;
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

function TUnclosedBugPatter.UpdateBugList(Html: string; var List: TParamList): Boolean;
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
  List.Clear;
  List.SetParam('>>>>>>最多前20项<<<<<<', Config.BugFreeUrl + '/BugList.php');

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
    Title := Utf8ToAnsiString(Title);
    List.SetParam(Title, Url);
  end;
  Result := True;
end;

{ TBugPatterList }

constructor TBugPatterList.Create;
begin
  FList := TList.Create;
end;

destructor TBugPatterList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TBugPatterList.Add(BP: TBugPatter);
begin
  FList.Add(BP);
end;

procedure TBugPatterList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TBugPatter(FList[I]).Free;
  FList.Clear;
end;

function TBugPatterList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBugPatterList.GetItems(I: Integer): TBugPatter;
begin
  Result := TBugPatter(FList[I]);
end;

end.
