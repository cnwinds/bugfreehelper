unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, ExtCtrls, MonitorBug, Menus, ImgList, ShellAPI;

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

    procedure BugCountChange(Sender: TObject;
      OldUnclosedBugCount, NewUnclosedBugCount: Integer;
      OldUnresolvedBugCount, NewUnresolvedBugCount: Integer);
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

procedure TBugFreeHelperForm.BugCountChange(Sender: TObject;
    OldUnclosedBugCount, NewUnclosedBugCount: Integer;
    OldUnresolvedBugCount, NewUnresolvedBugCount: Integer);
begin
  MainTrayIcon.BalloonTitle := '';

  if NewUnclosedBugCount = 0 then
    MainTrayIcon.BalloonHint := Format('太好了，你已经清除了所有你的Bug :)', [])
  else if OldUnclosedBugCount < NewUnclosedBugCount then
    MainTrayIcon.BalloonHint := Format('注意：新发现了%d个Bug，现在你共有%d个Bug',
      [NewUnclosedBugCount - OldUnclosedBugCount, NewUnclosedBugCount])
  else
    MainTrayIcon.BalloonHint := Format('恭喜了，减少了%d个Bug，现在你共有%d个Bug',
      [OldUnclosedBugCount - NewUnclosedBugCount, NewUnclosedBugCount]);
  MainTrayIcon.BalloonTimeout := 5000;
  MainTrayIcon.BalloonFlags := bfInfo;
  MainTrayIcon.ShowBalloonHint;
end;

procedure TBugFreeHelperForm.BugStatueChange(Sender: TObject; OldStatue,
  NewStatue: TMonitorBugStatue);
begin
  case NewStatue of
    mbsOffline:
    begin
      // nothing
    end;
    mbsLogining:
    begin
      // nothing
    end;
    mbsNormal: 
    begin
      // nothing
    end;
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

  FMonitorBug := TMonitorBug.Create;
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
    FMonitorBug.Join;
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
var
  Url: string;
begin
  Url := Format('%s\index.php?BugUserName=%s&BugUserPWD=%s',
                [Config.BugFreeUrl, Config.UserName, Config.UserPWD]);
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOW);
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

  case FMonitorBug.Statue of
    mbsOffline:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := 0;
    end;
    mbsLogining: 
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := (MainTrayIcon.IconIndex + 1) mod 2
    end;
    mbsNormal:
    begin
      if FMonitorBug.UnclosedBugCount = 0 then
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
    mbsParamFail:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := 2;
    end;
    mbsLoginFail:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := 2;
    end;
    mbsHttpFail:
    begin
      MainTrayIcon.Icons := ImageList1;
      MainTrayIcon.Animate := False;
      MainTrayIcon.IconIndex := 2;
    end;
  end;
end;

procedure TBugFreeHelperForm.MainPopupMenuPopup(Sender: TObject);
var
  Ver: Cardinal;

  function CreateMenuItem(Caption: string; Event: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := Caption;
    Result.OnClick := Event;
  end;

begin
  MainPopupMenu.Items.Clear;

  Ver := GetFileVersion(Application.ExeName);
  MainPopupMenu.Items.Add(CreateMenuItem(Format('版本[%d.%d] %s...', [Ver shr 16, Ver and $ffff, FMonitorBug.Ver]), ShowAboutForm));
  MainPopupMenu.Items.Add(CreateMenuItem(Format('-', []), nil));

  if FMonitorBug <> nil then
  begin
    case FMonitorBug.Statue of
      mbsOffline:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('准备登陆', []), nil));
      end;
      mbsLogining:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('正在登陆中，耐心等一下 :)', []), nil));
      end;
      mbsNormal:
      begin
        if FMonitorBug.UnclosedBugCount = 0 then
          MainPopupMenu.Items.Add(CreateMenuItem(Format('恭喜你[%s]，没有你的Bug', [FMonitorBug.UserName]), OpenBrowser))
        else begin
          MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]未关闭Bug%d个...', [FMonitorBug.UserName, FMonitorBug.UnclosedBugCount]), OpenBrowser));
          MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]未解决Bug%d个...', [FMonitorBug.UserName, FMonitorBug.UnresolvedBugCount]), OpenBrowser));
        end;
      end;
      mbsParamFail:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('请设置必要的参数...', []), OpenSetting));
      end;
      mbsLoginFail:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]密码设错误，赶紧重新设置...', [FMonitorBug.UserName]), OpenSetting))
      end;
      mbsHttpFail:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('访问服务器失败', []), OpenSetting));
      end;
    end;
  end;

  MainPopupMenu.Items.Add(CreateMenuItem('-', nil));
  MainPopupMenu.Items.Add(CreateMenuItem('设置参数...', OpenSetting));
  MainPopupMenu.Items.Add(CreateMenuItem('-', nil));
  MainPopupMenu.Items.Add(CreateMenuItem('退出', MainClose));
end;

end.
