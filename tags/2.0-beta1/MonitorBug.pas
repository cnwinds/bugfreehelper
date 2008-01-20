unit MonitorBug;

interface

uses SysUtils, StrUtils, Classes, Windows, Forms,
     SHDocVw, mshtml, OleCtrls, ExtCtrls, Ole2, SndKey32;

type

{ TMonitorBug }

  TMonitorBugStatue = ( mbsInit,            // 初始化，开始登录
                        mbsWaitLoginPage,   // 等待登录页面返回
                        mbsWaitLoginResult, // 等待登录结果

                        mbsIdel,            // 空闲状态(登陆成功)

                        mbsWaitQueryPage,   // 等待查询页面返回
                        mbsWaitQueryResult, // 等待查询结果

                        mbsLogout,          // 注销

                        mbsParamFail,       // 参数设置不正确
                        mbsLoginFail,       // 登陆失败
                        mbsHttpFail);       // 访问服务器失败

  TBugCountChangeNotify = procedure(Sender: TObject;
    OldCount, NewCount: Integer) of object;

  TBugStatueChangeNotify = procedure(Sender: TObject;
    OldStatue, NewStatue: TMonitorBugStatue) of object;

  TMonitorBug = class(TObject)
  private
    FBugFreeUrl: string;
    FUserName: string;
    FUserPWD: string;

    FIsLogin: Boolean;
    FLastUpdateTime: TDateTime;
    FBugCount: Integer;
    FMonitorBugStatue: TMonitorBugStatue;

    FSleepTime: DWORD;

    FOwner: TForm;
    FHttp, FCloneWeb: TWebBrowser;
    FBugChangeNotify: TBugCountChangeNotify;
    FBugStatueChangeNotify: TBugStatueChangeNotify;

    FLastTickCount: DWORD;

    FTimer: TTimer;

    procedure SetState(Value: TMonitorBugStatue);
    function GetUpdateIntervalSec: DWORD;
    procedure SetUpdateIntervalSec(const Value: DWORD);

    procedure Process;

    procedure OnTimerProcess(Sender: TObject);

    function CheckCloneWeb: Boolean;

  public
    constructor Create(AOwner: TForm);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure CloneNewWindow;

    property WebBrowser: TWebBrowser read FHttp write FHttp;
    property BugFreeUrl: string read FBugFreeUrl write FBugFreeUrl;
    property UserName: string read FUserName write FUserName;
    property UserPWD: string read FUserPWD write FUserPWD;
    property UpdateIntervalSec: DWORD
      read GetUpdateIntervalSec write SetUpdateIntervalSec;

    property IsLogin: Boolean read FIsLogin;
    property LastUpdateTime: TDateTime read FLastUpdateTime;
    property BugCount: Integer read FBugCount;
    property State: TMonitorBugStatue read FMonitorBugStatue;

    property BugChangeNotify: TBugCountChangeNotify read FBugChangeNotify write FBugChangeNotify;
    property BugStatueChangeNotify: TBugStatueChangeNotify read FBugStatueChangeNotify write FBugStatueChangeNotify;
  end;

implementation

{ TMonitorBug }

procedure TMonitorBug.CloneNewWindow;
begin
  while not CheckCloneWeb do
    Application.ProcessMessages;

  FCloneWeb.SetFocus;
  SendKeys('^n', False); // 模拟按下Ctrl+N，产生一个新的IE窗口
end;

{
  如果页面已经可以按下Ctrl+N产生新窗口则返回True
}
function TMonitorBug.CheckCloneWeb: Boolean;
var
  HtmlDoc: IHTMLDocument3;
  HtmlElement: IHTMLElement;
begin
  Result := False;
  // 如果页面的框架已经打开，则停止页继续访问（会有脚本报错）
  HtmlDoc := FCloneWeb.Document as IHTMLDocument3;
  if HtmlDoc <> nil then
  begin
    HtmlElement := HtmlDoc.getElementById('RightBottomFrame') as IHTMLElement;
    if HtmlElement <> nil then
    begin
      FCloneWeb.Stop; // 防止脚本错误
      Result := True;
    end;
  end;
  // 打开bugfree主页  
  if (HtmlElement = nil) and
      (FCloneWeb.ReadyState = READYSTATE_COMPLETE) then
    FCloneWeb.Navigate(BugFreeUrl);
end;

constructor TMonitorBug.Create(AOwner: TForm);
begin
  inherited Create;

  FOwner := AOwner;

  // 创建控件
  FHttp := TWebBrowser.Create(FOwner);
//  FHttp.Top := -100;
//  FHttp.Left := -100;
//  FHttp.Width := 10;
//  FHttp.Height := 10;
  FHttp.Top := 0;
  FHttp.Left := 0;
  FHttp.Width := 400;
  FHttp.Height := 200;
  FHttp.ParentWindow := FOwner.Handle;

  FCloneWeb := TWebBrowser.Create(FOwner);
//  FCloneWeb.Top := -100;
//  FCloneWeb.Left := -100;
//  FCloneWeb.Width := 10;
//  FCloneWeb.Height := 10;
  FCloneWeb.Top := 50;
  FCloneWeb.Left := 50;
  FCloneWeb.Width := 300;
  FCloneWeb.Height := 200;
  FCloneWeb.ParentWindow := FOwner.Handle;

  FTimer := nil;

  // Bug更新时间
  FSleepTime := 10 * 1000;
  FIsLogin := False;
  FBugCount := 0;

  BugChangeNotify := nil;
  BugStatueChangeNotify := nil;

  SetState(mbsInit);
end;

destructor TMonitorBug.Destroy;
begin
  Stop;
  
  FreeAndNil(FHttp);
  FreeAndNil(FCloneWeb);

  inherited;
end;

procedure TMonitorBug.Process;
  procedure DoInit;
  begin
    // 使用注销地址为了防止已经登录的情况会出现错误
    FHttp.Navigate(BugFreeUrl + '/Logout.php?Logout=Yes');

    SetState(mbsWaitLoginPage);
  end;

  procedure DoWaitLoginPage;
  var
    HtmlDoc: IHTMLDocument3;
    HtmlForm: IHTMLFormElement;
    HtmlElement: IHTMLElement;
  begin
    if FHttp.ReadyState = READYSTATE_COMPLETE then
    begin
      try
        // 准备登录
        HtmlDoc := FHttp.Document as IHTMLDocument3;
        HtmlForm := HtmlDoc.getElementById('LoginForm') as IHTMLFormElement;
        HtmlElement := HtmlForm.item('TestUserName', 0) as IHTMLElement;
        // 设置登录用户名和密码
        HtmlElement.setAttribute('value', UserName, 0);
        HtmlElement := HtmlForm.item('TestUserPWD', 0) as IHTMLElement;
        HtmlElement.setAttribute('value', UserPWD, 0);
        // 点击登录按钮(登录按钮没有设置id，所以只能通过索引的方式确定位置)
        HtmlElement := HtmlForm.Item(2, 0) as IHTMLElement;
        HtmlElement.click;

        SetState(mbsWaitLoginResult);
      except
        SetState(mbsHttpFail); // 如果没有正常打开页面会出现异常
      end;
    end else
    begin
      // 等待登录页面是否超时
      if ((GetTickCount - FLastTickCount) > 20 * 1000) then
        SetState(mbsHttpFail);
    end;
  end;

  procedure QueryBug;
  begin
    FHttp.Navigate(BugFreeUrl + '/SearchBug.php');

    SetState(mbsWaitQueryPage);
  end;

  procedure DoWaitLoginResult;
  const
    LoginResponse_HtmlName = 'ActionMessage';
    LoginSuccess = '登录成功';
    LoginFail = '您的用户名或密码不正确。请重新输入!';
  var
    HtmlDoc: IHTMLDocument3;
    HtmlElement: IHTMLElement;
    LoginResult: string;
  begin
    try
      // 等待ajax的登录结果
      HtmlDoc := FHttp.Document as IHTMLDocument3;
      HtmlElement := HtmlDoc.getElementById(LoginResponse_HtmlName) as IHTMLElement;
      if HtmlElement.innerText <> '' then
      begin
        // 阻止登录页面转向
        FHttp.Stop;
        // 分析登录的结果
        LoginResult := HtmlElement.innerText;
        if LoginResult = LoginSuccess then
          QueryBug
        else
          SetState(mbsLoginFail);
      end else
      begin
        // 等待登录结果是否超时
        if ((GetTickCount - FLastTickCount) > 20 * 1000) then
          SetState(mbsInit);
      end;
    except
      SetState(mbsHttpFail);
    end;
  end;

  procedure DoIdel;
  begin
    CheckCloneWeb;
    if ((GetTickCount - FLastTickCount) > FSleepTime) then
      QueryBug;
  end;

  procedure DoWaitQueryPage;
  var
    HtmlDoc: IHTMLDocument3;
    HtmlElement: IHTMLElement;
    HtmlElement3: IHTMLElement3;
  begin
    CheckCloneWeb;
    try
      if FHttp.ReadyState = READYSTATE_COMPLETE then
      begin
        // 设置查找条件为：指派给自己并且是激活状态的bug
        HtmlDoc := FHttp.Document as IHTMLDocument3;
        HtmlElement := HtmlDoc.getElementById('Field1') as IHTMLElement;
        HtmlElement.setAttribute('value', 'BugStatus', 0);
        HtmlElement3 := HtmlDoc.getElementById('Field1') as IHTMLElement3;
        HtmlElement3.fireEvent('onchange', EmptyParam); // 需要触发事件
        HtmlElement := HtmlDoc.getElementById('Value1') as IHTMLElement;
        HtmlElement.setAttribute('value', 'Active', 0);
        HtmlElement := HtmlDoc.getElementById('Value3') as IHTMLElement;
        HtmlElement.setAttribute('value', UserName, 0);
        // 设置在本页面返回结果
        HtmlElement := HtmlDoc.getElementById('SearchBug');
        HtmlElement.setAttribute('target', '_self', 0);
        // 开始查询
        HtmlElement := HtmlDoc.getElementById('PostQuery');
        HtmlElement.click;

        SetState(mbsWaitQueryResult);
      end else
      begin
        // 等待登录页面是否超时
        if ((GetTickCount - FLastTickCount) > 20 * 1000) then
        begin
          QueryBug;
        end;
      end;
    except
      QueryBug;
    end;
  end;

  procedure DoWaitQueryResult;
  var
    HtmlDoc: IHTMLDocument3;
    HtmlElement: IHTMLElement;
    StartPos, EndPos, BugCount: Integer;
    Html: string;
  begin
    CheckCloneWeb;
    // 判断是否已经返回结果
    HtmlDoc := FHttp.Document as IHTMLDocument3;
    HtmlElement := HtmlDoc.getElementById('_PageTotal');
    if (HtmlElement <> nil) then
    begin
      // 取出结果
      HtmlElement := (FHttp.Document as IHTMLDocument2).body;
      Html := HtmlElement.innerText;
      // 分析结果
      StartPos := PosEx('结果', Html);
      StartPos := PosEx('/', Html, StartPos) + 1;
      EndPos := PosEx(' ', Html, StartPos);
      BugCount := StrToInt(Copy(Html, StartPos, EndPos - StartPos));
      try
        if (FBugCount <> BugCount) and Assigned(BugChangeNotify) then
          BugChangeNotify(Self, FBugCount, BugCount);
      except
        // nothing
      end;
      FBugCount := BugCount;
      FLastUpdateTime := Now;

      SetState(mbsIdel);
    end else
    begin
      // 等待登录页面是否超时
      if ((GetTickCount - FLastTickCount) > 20 * 1000) then
        QueryBug;
    end;
  end;

  procedure DoLogout;
  begin
    // todo 注销
  end;

  procedure DoParamFail;
  begin
    // nothing
  end;

  procedure DoLoginFail;
  begin
    // nothing
  end;

  procedure DoHttpFail;
  begin
    if ((GetTickCount - FLastTickCount) > 20 * 1000) then
      SetState(mbsInit); // 等待一段时间重新登录
  end;
begin
  case FMonitorBugStatue of
      mbsInit: DoInit;  // 初始化，开始登录
      mbsWaitLoginPage: DoWaitLoginPage; // 等待登录页面返回
      mbsWaitLoginResult: DoWaitLoginResult; // 等待登录结果
      mbsIdel: DoIdel; // 空闲状态(登陆成功)
      mbsWaitQueryPage: DoWaitQueryPage; // 等待查询页面返回
      mbsWaitQueryResult: DoWaitQueryResult; // 等待查询结果
      mbsLogout: DoLogout; // 注销
      mbsParamFail: DoParamFail; // 参数设置不正确
      mbsLoginFail: DoLoginFail; // 登陆失败
      mbsHttpFail: DoHttpFail; // 访问服务器失败
  end;
end;

procedure TMonitorBug.SetState(Value: TMonitorBugStatue);
  procedure EnterInit;
  begin
    if (BugFreeUrl = '') or (UserName = '') or (UserPWD = '') then
    begin
      SetState(mbsParamFail);
      Exit;
    end;
    FIsLogin := False;
  end;

  procedure EnterWaitLoginPage;
  begin
    FIsLogin := False;
    // 记录时间
    FLastTickCount := GetTickCount;
  end;

  procedure EnterWaitLoginResult;
  begin
    FIsLogin := False;
    // 记录时间
    FLastTickCount := GetTickCount;
  end;

  procedure EnterIdel;
  begin
    FIsLogin := True;
    // 记录时间
    FLastTickCount := GetTickCount;
  end;

  procedure EnterWaitQueryPage;
  begin
    FIsLogin := True;
    // 记录时间
    FLastTickCount := GetTickCount;
  end;

  procedure EnterWaitQueryResult;
  begin
    FIsLogin := True;
    // 记录时间
    FLastTickCount := GetTickCount;
  end;

  procedure EnterLogout;
  begin
    FIsLogin := False;
  end;

  procedure EnterParamFail;
  begin
    FIsLogin := False;
  end;

  procedure EnterLoginFail;
  begin
    FIsLogin := False;
  end;

  procedure EnterHttpFail;
  begin
    FIsLogin := False;
    // 记录时间
    FLastTickCount := GetTickCount;
  end;
begin
//  if Value <> FMonitorBugStatue then
  begin
    case Value of
      mbsInit: EnterInit;  // 初始化，开始登录
      mbsWaitLoginPage: EnterWaitLoginPage; // 等待登录页面返回
      mbsWaitLoginResult: EnterWaitLoginResult; // 等待登录结果
      mbsIdel: EnterIdel; // 空闲状态(登陆成功)
      mbsWaitQueryPage: EnterWaitQueryPage; // 等待查询页面返回
      mbsWaitQueryResult: EnterWaitQueryResult; // 等待查询结果
      mbsLogout: EnterLogout; // 注销
      mbsParamFail: EnterParamFail; // 参数设置不正确
      mbsLoginFail: EnterLoginFail; // 登陆失败
      mbsHttpFail: EnterHttpFail; // 访问服务器失败
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

function TMonitorBug.GetUpdateIntervalSec: DWORD;
begin
  Result := FSleepTime div 1000;
end;

procedure TMonitorBug.OnTimerProcess(Sender: TObject);
begin
  Process;
end;

procedure TMonitorBug.SetUpdateIntervalSec(const Value: DWORD);
begin
  if FSleepTime <> Value then
  begin
    FSleepTime := Value * 1000;
  end;
end;

procedure TMonitorBug.Start;
begin
  if FTimer <> nil then Stop;

  FTimer := TTimer.Create(FOwner);
  FTimer.OnTimer := OnTimerProcess;
  FTimer.Interval := 100;
  FTimer.Enabled := True;
end;

procedure TMonitorBug.Stop;
begin
  if FTimer = nil then Exit;

  FTimer.Enabled := False;
  FreeAndNil(FTimer);
end;

initialization
  coInitialize(nil);

finalization
  coUninitialize;

end.
