unit MonitorBug;

interface

uses SysUtils, StrUtils, Classes, Windows, Forms,
     SHDocVw, mshtml, OleCtrls, ExtCtrls, Ole2, SndKey32;

type

{ TMonitorBug }

  TMonitorBugStatue = ( mbsInit,            // ��ʼ������ʼ��¼
                        mbsWaitLoginPage,   // �ȴ���¼ҳ�淵��
                        mbsWaitLoginResult, // �ȴ���¼���

                        mbsIdel,            // ����״̬(��½�ɹ�)

                        mbsWaitQueryPage,   // �ȴ���ѯҳ�淵��
                        mbsWaitQueryResult, // �ȴ���ѯ���

                        mbsLogout,          // ע��

                        mbsParamFail,       // �������ò���ȷ
                        mbsLoginFail,       // ��½ʧ��
                        mbsHttpFail);       // ���ʷ�����ʧ��

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
  SendKeys('^n', False); // ģ�ⰴ��Ctrl+N������һ���µ�IE����
end;

{
  ���ҳ���Ѿ����԰���Ctrl+N�����´����򷵻�True
}
function TMonitorBug.CheckCloneWeb: Boolean;
var
  HtmlDoc: IHTMLDocument3;
  HtmlElement: IHTMLElement;
begin
  Result := False;
  // ���ҳ��Ŀ���Ѿ��򿪣���ֹͣҳ�������ʣ����нű�����
  HtmlDoc := FCloneWeb.Document as IHTMLDocument3;
  if HtmlDoc <> nil then
  begin
    HtmlElement := HtmlDoc.getElementById('RightBottomFrame') as IHTMLElement;
    if HtmlElement <> nil then
    begin
      FCloneWeb.Stop; // ��ֹ�ű�����
      Result := True;
    end;
  end;
  // ��bugfree��ҳ  
  if (HtmlElement = nil) and
      (FCloneWeb.ReadyState = READYSTATE_COMPLETE) then
    FCloneWeb.Navigate(BugFreeUrl);
end;

constructor TMonitorBug.Create(AOwner: TForm);
begin
  inherited Create;

  FOwner := AOwner;

  // �����ؼ�
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

  // Bug����ʱ��
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
    // ʹ��ע����ַΪ�˷�ֹ�Ѿ���¼���������ִ���
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
        // ׼����¼
        HtmlDoc := FHttp.Document as IHTMLDocument3;
        HtmlForm := HtmlDoc.getElementById('LoginForm') as IHTMLFormElement;
        HtmlElement := HtmlForm.item('TestUserName', 0) as IHTMLElement;
        // ���õ�¼�û���������
        HtmlElement.setAttribute('value', UserName, 0);
        HtmlElement := HtmlForm.item('TestUserPWD', 0) as IHTMLElement;
        HtmlElement.setAttribute('value', UserPWD, 0);
        // �����¼��ť(��¼��ťû������id������ֻ��ͨ�������ķ�ʽȷ��λ��)
        HtmlElement := HtmlForm.Item(2, 0) as IHTMLElement;
        HtmlElement.click;

        SetState(mbsWaitLoginResult);
      except
        SetState(mbsHttpFail); // ���û��������ҳ�������쳣
      end;
    end else
    begin
      // �ȴ���¼ҳ���Ƿ�ʱ
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
    LoginSuccess = '��¼�ɹ�';
    LoginFail = '�����û��������벻��ȷ������������!';
  var
    HtmlDoc: IHTMLDocument3;
    HtmlElement: IHTMLElement;
    LoginResult: string;
  begin
    try
      // �ȴ�ajax�ĵ�¼���
      HtmlDoc := FHttp.Document as IHTMLDocument3;
      HtmlElement := HtmlDoc.getElementById(LoginResponse_HtmlName) as IHTMLElement;
      if HtmlElement.innerText <> '' then
      begin
        // ��ֹ��¼ҳ��ת��
        FHttp.Stop;
        // ������¼�Ľ��
        LoginResult := HtmlElement.innerText;
        if LoginResult = LoginSuccess then
          QueryBug
        else
          SetState(mbsLoginFail);
      end else
      begin
        // �ȴ���¼����Ƿ�ʱ
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
        // ���ò�������Ϊ��ָ�ɸ��Լ������Ǽ���״̬��bug
        HtmlDoc := FHttp.Document as IHTMLDocument3;
        HtmlElement := HtmlDoc.getElementById('Field1') as IHTMLElement;
        HtmlElement.setAttribute('value', 'BugStatus', 0);
        HtmlElement3 := HtmlDoc.getElementById('Field1') as IHTMLElement3;
        HtmlElement3.fireEvent('onchange', EmptyParam); // ��Ҫ�����¼�
        HtmlElement := HtmlDoc.getElementById('Value1') as IHTMLElement;
        HtmlElement.setAttribute('value', 'Active', 0);
        HtmlElement := HtmlDoc.getElementById('Value3') as IHTMLElement;
        HtmlElement.setAttribute('value', UserName, 0);
        // �����ڱ�ҳ�淵�ؽ��
        HtmlElement := HtmlDoc.getElementById('SearchBug');
        HtmlElement.setAttribute('target', '_self', 0);
        // ��ʼ��ѯ
        HtmlElement := HtmlDoc.getElementById('PostQuery');
        HtmlElement.click;

        SetState(mbsWaitQueryResult);
      end else
      begin
        // �ȴ���¼ҳ���Ƿ�ʱ
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
    // �ж��Ƿ��Ѿ����ؽ��
    HtmlDoc := FHttp.Document as IHTMLDocument3;
    HtmlElement := HtmlDoc.getElementById('_PageTotal');
    if (HtmlElement <> nil) then
    begin
      // ȡ�����
      HtmlElement := (FHttp.Document as IHTMLDocument2).body;
      Html := HtmlElement.innerText;
      // �������
      StartPos := PosEx('���', Html);
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
      // �ȴ���¼ҳ���Ƿ�ʱ
      if ((GetTickCount - FLastTickCount) > 20 * 1000) then
        QueryBug;
    end;
  end;

  procedure DoLogout;
  begin
    // todo ע��
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
      SetState(mbsInit); // �ȴ�һ��ʱ�����µ�¼
  end;
begin
  case FMonitorBugStatue of
      mbsInit: DoInit;  // ��ʼ������ʼ��¼
      mbsWaitLoginPage: DoWaitLoginPage; // �ȴ���¼ҳ�淵��
      mbsWaitLoginResult: DoWaitLoginResult; // �ȴ���¼���
      mbsIdel: DoIdel; // ����״̬(��½�ɹ�)
      mbsWaitQueryPage: DoWaitQueryPage; // �ȴ���ѯҳ�淵��
      mbsWaitQueryResult: DoWaitQueryResult; // �ȴ���ѯ���
      mbsLogout: DoLogout; // ע��
      mbsParamFail: DoParamFail; // �������ò���ȷ
      mbsLoginFail: DoLoginFail; // ��½ʧ��
      mbsHttpFail: DoHttpFail; // ���ʷ�����ʧ��
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
    // ��¼ʱ��
    FLastTickCount := GetTickCount;
  end;

  procedure EnterWaitLoginResult;
  begin
    FIsLogin := False;
    // ��¼ʱ��
    FLastTickCount := GetTickCount;
  end;

  procedure EnterIdel;
  begin
    FIsLogin := True;
    // ��¼ʱ��
    FLastTickCount := GetTickCount;
  end;

  procedure EnterWaitQueryPage;
  begin
    FIsLogin := True;
    // ��¼ʱ��
    FLastTickCount := GetTickCount;
  end;

  procedure EnterWaitQueryResult;
  begin
    FIsLogin := True;
    // ��¼ʱ��
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
    // ��¼ʱ��
    FLastTickCount := GetTickCount;
  end;
begin
//  if Value <> FMonitorBugStatue then
  begin
    case Value of
      mbsInit: EnterInit;  // ��ʼ������ʼ��¼
      mbsWaitLoginPage: EnterWaitLoginPage; // �ȴ���¼ҳ�淵��
      mbsWaitLoginResult: EnterWaitLoginResult; // �ȴ���¼���
      mbsIdel: EnterIdel; // ����״̬(��½�ɹ�)
      mbsWaitQueryPage: EnterWaitQueryPage; // �ȴ���ѯҳ�淵��
      mbsWaitQueryResult: EnterWaitQueryResult; // �ȴ���ѯ���
      mbsLogout: EnterLogout; // ע��
      mbsParamFail: EnterParamFail; // �������ò���ȷ
      mbsLoginFail: EnterLoginFail; // ��½ʧ��
      mbsHttpFail: EnterHttpFail; // ���ʷ�����ʧ��
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
