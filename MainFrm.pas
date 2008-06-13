unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, MonitorBug, Menus, ImgList;

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
    procedure OpenMenuItemUrl(Sender: TObject);
  public
    { Public declarations }
    procedure MainClose(Sender: TObject);
    procedure OpenIndexPage(Sender: TObject);
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
    MainTrayIcon.BalloonHint := Format('̫���ˣ����Ѿ�������������Bug :)', [])
  else if OldUnclosedBugCount < NewUnclosedBugCount then
    MainTrayIcon.BalloonHint := Format('ע�⣺�·�����%d��Bug�������㹲��%d��Bug',
      [NewUnclosedBugCount - OldUnclosedBugCount, NewUnclosedBugCount])
  else
    MainTrayIcon.BalloonHint := Format('��ϲ�ˣ�������%d��Bug�������㹲��%d��Bug',
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
      MainTrayIcon.BalloonHint := Format('��ǰ�Ĳ����޷���½���������������ò���', []);
      MainTrayIcon.BalloonTimeout := 5000;
      MainTrayIcon.BalloonFlags := bfWarning;
      MainTrayIcon.ShowBalloonHint;
    end;
    mbsLoginFail:
    begin
      MainTrayIcon.BalloonHint := Format('�����û��������ô������������ò���', []);
      MainTrayIcon.BalloonTimeout := 5000;
      MainTrayIcon.BalloonFlags := bfError;
      MainTrayIcon.ShowBalloonHint;
    end;
    mbsHttpFail:
    begin
      MainTrayIcon.BalloonHint := Format('����BugFree��ַ���ò���ȷ���������ʱ���ܷ��ʣ����������ò������ó����Ժ�����', []);
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
  MainTrayIcon.Hint := 'BugFreeС����';
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
  FMonitorBug.OpenIE(Config.BugFreeUrl + '/' + 'index.php');
end;

procedure TBugFreeHelperForm.OpenMenuItemUrl(Sender: TObject);
var
  MI: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    MI := TMenuItem(Sender);
    FMonitorBug.OpenIE(MI.Hint);
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

  function CreateMenuItem(Caption: string; Event: TNotifyEvent;
    Tag: Integer = 0; Hint: string = ''): TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := Caption;
    Result.OnClick := Event;
    Result.Tag := Tag;
    Result.Hint := Hint;
  end;

  procedure AddMenuItem;
  var
    I: Integer;
    Key, Value: string;
    mi: TMenuItem;
  begin
    if FMonitorBug.UnclosedBugCount = 0 then
      MainPopupMenu.Items.Add(CreateMenuItem(Format('��ϲ��[%s]��û�����Bug', [Config.UserName]), OpenIndexPage))
    else begin
      MainPopupMenu.Items.Add(CreateMenuItem(Format('BugFree��ҳ...', []), OpenIndexPage));
      MainPopupMenu.Items.Add(CreateMenuItem(Format('-', []), nil));

      MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]δ�ر�Bug%d��...', [Config.UserName, FMonitorBug.UnclosedBugCount]), nil));
      mi := MainPopupMenu.Items[MainPopupMenu.Items.Count - 1];
      mi.Add(CreateMenuItem(Format('>>>>>>���ǰ20��<<<<<<', []), nil));
      for I := 0 to FMonitorBug.UnclosedBugList.Count - 1 do
      begin
        FMonitorBug.UnclosedBugList.GetParam(I, Key, Value);
        mi.Add(CreateMenuItem(Key, OpenMenuItemUrl, I, Value));
      end;

      MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]δ���Bug%d��...', [Config.UserName, FMonitorBug.UnresolvedBugCount]), nil));
      mi := MainPopupMenu.Items[MainPopupMenu.Items.Count - 1];
      mi.Add(CreateMenuItem(Format('>>>>>>���ǰ20��<<<<<<', []), nil));
      for I := 0 to FMonitorBug.UnresolvedBugList.Count - 1 do
      begin
        FMonitorBug.UnresolvedBugList.GetParam(I, Key, Value);
        mi.Add(CreateMenuItem(Key, OpenMenuItemUrl, I, Value));
      end;

    end;

  end;

begin
  MainPopupMenu.Items.Clear;

  Ver := GetFileVersion(Application.ExeName);
  MainPopupMenu.Items.Add(CreateMenuItem(Format('�汾[%d.%d] %s...', [Ver shr 16, Ver and $ffff, FMonitorBug.Ver]), ShowAboutForm));
  MainPopupMenu.Items.Add(CreateMenuItem(Format('-', []), nil));

  if FMonitorBug <> nil then
  begin
    case FMonitorBug.Statue of
      mbsOffline:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('׼����½', []), nil));
      end;
      mbsLogining:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('���ڵ�½�У����ĵ�һ�� :)', []), nil));
      end;
      mbsNormal:
      begin
        AddMenuItem;
      end;
      mbsParamFail:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('�����ñ�Ҫ�Ĳ���...', []), OpenSetting));
      end;
      mbsLoginFail:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('[%s]��������󣬸Ͻ���������...', [Config.UserName]), OpenSetting))
      end;
      mbsHttpFail:
      begin
        MainPopupMenu.Items.Add(CreateMenuItem(Format('���ʷ�����ʧ��', []), OpenSetting));
      end;
    end;
  end;

  MainPopupMenu.Items.Add(CreateMenuItem('-', nil));
  MainPopupMenu.Items.Add(CreateMenuItem('���ò���...', OpenSetting));
  MainPopupMenu.Items.Add(CreateMenuItem('-', nil));
  MainPopupMenu.Items.Add(CreateMenuItem('�˳�', MainClose));
end;

end.
