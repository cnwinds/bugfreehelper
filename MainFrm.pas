unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, ExtCtrls, MonitorBug, Menus, ImgList, ShellAPI, OleCtrls, SHDocVw,
  mshtml, ActiveX, IeConst;

type
  TBugFreeHelperForm = class(TForm)
    MainTrayIcon: TTrayIcon;
    MainPopupMenu: TPopupMenu;
    Timer1: TTimer;
    ImageList1: TImageList;
    TrayTimer: TTimer;
    ImageList2: TImageList;
    procedure TrayTimerTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FMonitorBug: TMonitorBug;

    procedure CreateMonitor;
    procedure DestroyMonitor;

    procedure BugCountChange(Sender: TObject; OldCount, NewCount: Integer);
    procedure BugStatueChange(Sender: TObject; OldStatue, NewStatue: TMonitorBugStatue);
  public
    { Public declarations }
    procedure MainClose(Sender: TObject);
    procedure OpenBrowser(Sender: TObject);
    procedure OpenSetting(Sender: TObject);
    procedure ShowAboutForm(Sender: TObject);
  end;

var
  BugFreeHelperForm: TBugFreeHelperForm;

implementation

uses SettingFrm, ConfigUnit, AboutFrm;

{$R *.dfm}

procedure TBugFreeHelperForm.BugCountChange(Sender: TObject; OldCount,
  NewCount: Integer);
begin
  MainTrayIcon.BalloonTitle := '';

  if NewCount = 0 then
    MainTrayIcon.BalloonHint := Format('太好了，你已经清除了所有你的Bug :)', [])
  else if OldCount < NewCount then
    MainTrayIcon.BalloonHint := Format('注意：新发现了%d个Bug，现在你共有%d个Bug', [NewCount - OldCount, NewCount])
  else
    MainTrayIcon.BalloonHint := Format('恭喜了，减少了%d个Bug，现在你共有%d个Bug', [OldCount - NewCount, NewCount]);
  MainTrayIcon.BalloonTimeout := 5000;
  MainTrayIcon.BalloonFlags := bfInfo;
  MainTrayIcon.ShowBalloonHint;
end;

procedure TBugFreeHelperForm.BugStatueChange(Sender: TObject; OldStatue,
  NewStatue: TMonitorBugStatue);
begin
  case NewStatue of
    mbsParamFail:
    begin
      MainTrayIcon.BalloonHint := Format('当前的参数无法登陆服务器，请先设置参数', []);
      MainTrayIcon.BalloonTimeout := 5000;
      MainTrayIcon.BalloonFlags := bfWarning;
      MainTrayIcon.ShowBalloonHint;
    end;
    mbsLoginFail:
    begin
      MainTrayIcon.BalloonHint := Format('可能用户密码设置错误，请重新设置参数', []);
      MainTrayIcon.BalloonTimeout := 5000;
      MainTrayIcon.BalloonFlags := bfError;
      MainTrayIcon.ShowBalloonHint;
    end;
    mbsHttpFail:
    begin
      MainTrayIcon.BalloonHint := Format('可能BugFree地址设置不正确或服务器暂时不能访问，请重新设置参数或让程序稍后重试', []);
      MainTrayIcon.BalloonTimeout := 5000;
      MainTrayIcon.BalloonFlags := bfWarning;
      MainTrayIcon.ShowBalloonHint;
    end;
  end;
end;

procedure TBugFreeHelperForm.CreateMonitor;
begin
  if FMonitorBug <> nil then DestroyMonitor;

  FMonitorBug := TMonitorBug.Create(Self);
  FMonitorBug.BugFreeUrl := Config.BugFreeUrl;
  FMonitorBug.UserName := Config.UserName;
  FMonitorBug.UserPWD := Config.UserPWD;
  FMonitorBug.UpdateIntervalSec := Config.UpdateIntervalSec;
  FMonitorBug.BugChangeNotify := BugCountChange;
  FMonitorBug.BugStatueChangeNotify := BugStatueChange;

  FMonitorBug.Start;
end;

procedure TBugFreeHelperForm.DestroyMonitor;
begin
  if FMonitorBug <> nil then
  begin
    FMonitorBug.Stop;
    FreeAndNil(FMonitorBug);
  end;
end;

procedure TBugFreeHelperForm.FormCreate(Sender: TObject);
begin
  MainTrayIcon.Icons := ImageList1;
  MainTrayIcon.Visible := True;
  MainTrayIcon.IconIndex := 0;
  MainTrayIcon.Hint := 'BugFree小助手';
end;

procedure TBugFreeHelperForm.FormDestroy(Sender: TObject);
begin
  DestroyMonitor;
end;

procedure TBugFreeHelperForm.MainClose(Sender: TObject);
begin
  Close;
end;

procedure TBugFreeHelperForm.OpenBrowser(Sender: TObject);
begin
  FMonitorBug.CloneNewWindow;
end;

procedure TBugFreeHelperForm.OpenSetting(Sender: TObject);
begin
  SettingForm.ShowModal;
  DestroyMonitor;
  CreateMonitor;
end;

procedure TBugFreeHelperForm.ShowAboutForm(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TBugFreeHelperForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  CreateMonitor;
end;

procedure TBugFreeHelperForm.TrayTimerTimer(Sender: TObject);
begin
  if FMonitorBug = nil then
  begin
    MainTrayIcon.Icons := ImageList1;
    MainTrayIcon.Animate := False;
    MainTrayIcon.IconIndex := 0;
    Exit;
  end;

  case FMonitorBug.State of
    mbsInit:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := 0;
    end;
    mbsWaitLoginPage, mbsWaitLoginResult:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := (MainTrayIcon.IconIndex + 1) mod 2
    end;
    mbsIdel, mbsWaitQueryPage, mbsWaitQueryResult:
    begin
      if FMonitorBug.BugCount = 0 then
      begin
        MainTrayIcon.Icons := ImageList1;
        MainTrayIcon.Animate := False;
        MainTrayIcon.IconIndex := 1;
      end else begin
        MainTrayIcon.Icons := ImageList2;
        MainTrayIcon.AnimateInterval := 200;
        MainTrayIcon.Animate := True;
      end;
    end;
    mbsParamFail, mbsLoginFail, mbsHttpFail:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := 2;
    end;
  end;
end;

procedure TBugFreeHelperForm.MainPopupMenuPopup(Sender: TObject);
const
  MenuTipInfo: array[0..11] of string = (
    '准备登陆',
    '正在等待登录页面返回...)',
    '正在验证登陆，耐心等一下 :)',
    '恭喜你[%s]，没有你的Bug',
    '[%s]未解决Bug%d个...',
    '正在获取最新的bug数...',
    '请设置必要的参数...',
    '[%s]密码设错误，赶紧重新设置...',
    '访问服务器失败',

    '版本[%d.%d]beta1...',
    '设置参数...',
    '退出');
var
  Ver: Cardinal;

  function CreateMenuItem(Caption: string; Event: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := Caption;
    Result.OnClick := Event;
  end;

  procedure AddMenuItem(Caption: string; Event: TNotifyEvent);
  begin
    MainPopupMenu.Items.Add(CreateMenuItem(Caption, Event));
  end;

begin
  MainPopupMenu.Items.Clear;

  Ver := GetFileVersion(Application.ExeName);

  AddMenuItem(Format(MenuTipInfo[9], [Ver shr 16, Ver and $ffff]),
    ShowAboutForm);
  AddMenuItem(Format('-', []), nil);

  if FMonitorBug <> nil then
  begin
    case FMonitorBug.State of
      mbsInit:
      begin
        AddMenuItem(Format(MenuTipInfo[0], []), nil);
      end;
      mbsWaitLoginPage:
      begin
        AddMenuItem(Format(MenuTipInfo[1], []), nil);
      end;
      mbsWaitLoginResult:
      begin
        AddMenuItem(Format(MenuTipInfo[2], []), nil);
      end;
      mbsIdel:
      begin
        if FMonitorBug.BugCount = 0 then
          AddMenuItem(Format(MenuTipInfo[3],
            [FMonitorBug.UserName, FMonitorBug.BugCount]), OpenBrowser)
        else
          AddMenuItem(Format(MenuTipInfo[4],
            [FMonitorBug.UserName, FMonitorBug.BugCount]), OpenBrowser);
      end;
      mbsWaitQueryPage, mbsWaitQueryResult:
      begin
        if FMonitorBug.BugCount = 0 then
          AddMenuItem(Format(MenuTipInfo[5],
            [FMonitorBug.UserName, FMonitorBug.BugCount]), OpenBrowser)
        else
          AddMenuItem(Format(MenuTipInfo[4],
            [FMonitorBug.UserName, FMonitorBug.BugCount]), OpenBrowser);
      end;
      mbsParamFail:
      begin
        AddMenuItem(Format(MenuTipInfo[6], []), OpenSetting);
      end;
      mbsLoginFail:
      begin
        AddMenuItem(Format(MenuTipInfo[7],
          [FMonitorBug.UserName]), OpenSetting)
      end;
      mbsHttpFail:
      begin
        AddMenuItem(Format(MenuTipInfo[8], []), OpenSetting);
      end;
    end;
  end;

  AddMenuItem('-', nil);
  AddMenuItem(MenuTipInfo[10], OpenSetting);
  AddMenuItem('-', nil);
  AddMenuItem(MenuTipInfo[11], MainClose);
end;

end.
