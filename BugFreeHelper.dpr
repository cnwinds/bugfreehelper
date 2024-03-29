program BugFreeHelper;

uses
  Windows,
  Forms,
  Ole2,
  MainFrm in 'MainFrm.pas' {BugFreeHelperForm},
  MonitorBug in 'MonitorBug.pas',
  SettingFrm in 'SettingFrm.pas' {SettingForm},
  ConfigUnit in 'ConfigUnit.pas',
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  ParamUnit in 'ParamUnit.pas',
  StupidHttp in 'StupidHttp.pas',
  WorkerThreadUnit in 'WorkerThreadUnit.pas',
  ConvertCharset in 'ConvertCharset.pas';

{$R *.res}

begin
  if FindWindow('TBugFreeHelperForm', nil) <> 0 then Exit;

  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TBugFreeHelperForm, BugFreeHelperForm);
  Application.CreateForm(TSettingForm, SettingForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
