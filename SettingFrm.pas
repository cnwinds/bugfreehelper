unit SettingFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls, ConfigUnit, Registry;

type
  TSettingForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    Label5: TLabel;
    CheckBox1: TCheckBox;
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FDirty: Boolean;
  public
    { Public declarations }
  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.dfm}

procedure TSettingForm.Button1Click(Sender: TObject);

  procedure AutoRun(IsRun: Boolean);
  var
    Reg: TRegistry;
  begin
    Reg := TRegistry.Create;
    try
      if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', False) then
      begin
        if IsRun then
          Reg.WriteString('BugFreeHelper', Application.ExeName)
        else
          Reg.WriteString('BugFreeHelper', '');
      end;
    finally
      Reg.Free;
    end;
  end;

begin
  Config.BugFreeUrl := Edit1.Text;
  Config.UserName := Edit2.Text;
  Config.UserPWD := Edit3.Text;
  Config.UpdateIntervalSec := SpinEdit1.Value;
  Config.AutoStart := CheckBox1.Checked;
  Config.Save;
  AutoRun(Config.AutoStart);

  FDirty := False;
//  MessageBox(Application.Handle, '参数已经保存！', '提示', MB_OK + MB_ICONINFORMATION);
  Close;
end;

procedure TSettingForm.Button2Click(Sender: TObject);
begin
  if FDirty then
  begin
    if MessageBox(Application.Handle, '数据被修改，是否放弃修改？', '提示', MB_YESNO + MB_ICONQUESTION) = ID_YES then
    begin
      Close;
      Exit;
    end
    else Exit;
  end;
  Close;
end;

procedure TSettingForm.Edit1Change(Sender: TObject);
begin
  FDirty := True;
end;

procedure TSettingForm.FormShow(Sender: TObject);
begin
  Edit1.Text := Config.BugFreeUrl;
  Edit2.Text := Config.UserName;
  Edit3.Text := Config.UserPWD;
  SpinEdit1.Value := Config.UpdateIntervalSec;
  CheckBox1.Checked := Config.AutoStart;
  FDirty := False;
end;

end.
