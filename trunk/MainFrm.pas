unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, MonitorBug, Menus, ImgList, ShellAPI;

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
      PatternBug: TPatternBug; OldBugCount, NewBugCount: Integer);
    procedure BugStatueChange(Sender: TObject; OldStatue, NewStatue: TMonitorBugStatue);
    procedure OpenMenuItemUrl(Sender: TObject);
    procedure OpenIndexPage(Sender: TObject);
    procedure OpenIe(const Url: string);
  public
    { Public declarations }
    procedure MainClose(Sender: TObject);
    procedure OpenSetting(Sender: TObject);
    procedure ShowAboutForm(Sender: TObject);
  end;

var
  BugFreeHelperForm: TBugFreeHelperForm;

implementation

uses SettingFrm, ConfigUnit, AboutFrm;

{$R *.dfm}

procedure TBugFreeHelperForm.BugCountChange(Sender: TObject;
    PatternBug: TPatternBug; OldBugCount, NewBugCount: Integer);
begin
  MainTrayIcon.BalloonTitle := '';

  if NewBugCount = 0 then
    MainTrayIcon.BalloonHint := Format('太好了，你已经清除了所有的%s :)',
      [PatternBug.GetBugDesc])
  else if OldBugCount < NewBugCount then
    MainTrayIcon.BalloonHint := Format('注意：新发现了%d个%s，现在你共有%d个%s。',
      [NewBugCount - OldBugCount, PatternBug.GetBugDesc, NewBugCount, PatternBug.GetBugDesc])
  else
    MainTrayIcon.BalloonHint := Format('恭喜了，减少了%d个%s，现在你共有%d个%s。',
      [OldBugCount - NewBugCount, PatternBug.GetBugDesc, NewBugCount, PatternBug.GetBugDesc]);
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

procedure TBugFreeHelperForm.OpenIndexPage(Sender: TObject);
begin
  OpenIe(FMonitorBug.GetIndexPageUrl);
end;

procedure TBugFreeHelperForm.OpenMenuItemUrl(Sender: TObject);
var
  MI: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    MI := TMenuItem(Sender);
    OpenIe(MI.Hint);
  end;
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

procedure TBugFreeHelperForm.OpenIe(const Url: string);
begin
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOW);
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
      if FMonitorBug.PatternBugList.Items[0].GetBugCount = 0 then  // 判断第一项Bug数是否为0
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

  function CreateMenuItem(Caption: string; Event: TNotifyEvent;
    Tag: Integer = 0; Hint: string = ''): TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := Caption;
    Result.OnClick := Event;
    Result.Tag := Tag;
    Result.Hint := Hint;
  end;

  procedure AddBugListMenuItem;
  var
    I, J: Integer;
    Key, Value: string;
    MenuItem: TMenuItem;
    Bug: TPatternBug;
  begin
    MainPopupMenu.Items.Add(CreateMenuItem(Format('BugFree首页...',
      []), OpenIndexPage));
    MainPopupMenu.Items.Add(CreateMenuItem(Format('-', []), nil));

    for I := 0 to FMonitorBug.PatternBugList.Count - 1 do
    begin
      Bug := FMonitorBug.PatternBugList[I];
      if Bug.GetBugCount = 0 then
        MainPopupMenu.Items.Add(
          CreateMenuItem(Format('恭喜你[%s]，没有%s',
          [Config.UserName, Bug.GetBugDesc]), OpenIndexPage))
      else begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]%d个%s',
          [Config.UserName, Bug.GetBugCount, Bug.GetBugDesc]), nil));
        MenuItem := MainPopupMenu.Items[MainPopupMenu.Items.Count - 1];
        for J := 0 to Bug.GetBugList.Count - 1 do
        begin
          Bug.GetBugList.GetParam(J, Key, Value);
          MenuItem.Add(CreateMenuItem(Key, OpenMenuItemUrl, J, Value));
        end;
      end;
    end;
  end;

begin
  MainPopupMenu.Items.Clear;

  Ver := GetFileVersion(Application.ExeName);
  MainPopupMenu.Items.Add(CreateMenuItem(Format('版本[%d.%d] %s...',
    [Ver shr 16, Ver and $ffff, FMonitorBug.Version]), ShowAboutForm));
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
        MainPopupMenu.Items.Add(
          CreateMenuItem(Format('正在登陆中，耐心等一下 :)', []), nil));
      end;
      mbsNormal:
      begin
        AddBugListMenuItem;
      end;
      mbsParamFail:
      begin
        MainPopupMenu.Items.Add(
          CreateMenuItem(Format('请设置必要的参数...', []), OpenSetting));
      end;
      mbsLoginFail:
      begin
        MainPopupMenu.Items.Add(
          CreateMenuItem(Format('[%s]密码设错误，赶紧重新设置...',
          [Config.UserName]), OpenSetting))
      end;
      mbsHttpFail:
      begin
        MainPopupMenu.Items.Add(
          CreateMenuItem(Format('访问服务器失败', []), OpenSetting));
      end;
    end;
  end;

  MainPopupMenu.Items.Add(CreateMenuItem('-', nil));
  MainPopupMenu.Items.Add(CreateMenuItem('设置参数...', OpenSetting));
  MainPopupMenu.Items.Add(CreateMenuItem('-', nil));
  MainPopupMenu.Items.Add(CreateMenuItem('退出', MainClose));
end;

end.
