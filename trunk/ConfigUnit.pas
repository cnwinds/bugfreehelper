unit ConfigUnit;

interface

uses Forms, IniFiles, SysUtils, Registry;

const
  ConfigFileName = 'config.ini';
  RootKey = '\Software\BugFreeHelper';

type

  TConfig = class
  private
    FBugFreeUrl: string;
    FUserName: string;
    FUpdateIntervalSec: Integer;
    FUserPWD: string;
    FAutoStart: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property BugFreeUrl: string read FBugFreeUrl write FBugFreeUrl;
    property UserName: string read FUserName write FUserName;
    property UserPWD: string read FUserPWD write FUserPWD;
    property UpdateIntervalSec: Integer read FUpdateIntervalSec write FUpdateIntervalSec;
    property AutoStart: Boolean read FAutoStart write FAutoStart;
  end;

var
  Config: TConfig;

implementation

{ TConfig }

constructor TConfig.Create;
begin
  Load;
end;

destructor TConfig.Destroy;
begin
  Save;
  inherited;
end;


procedure TConfig.Load;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(RootKey, True);
    BugFreeUrl := Reg.ReadString('BugFreeUrl');
    UserName := Reg.ReadString('UserName');
    UserPWD := Reg.ReadString('UserPWD');
    try
      UpdateIntervalSec := Reg.ReadInteger('UpdateIntervalSec');
    except
      UpdateIntervalSec := 60;
    end;
    try
      FAutoStart := Reg.ReadBool('AutoStart');
    except
      FAutoStart := True;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TConfig.Save;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(RootKey, True);
    Reg.WriteString('BugFreeUrl', BugFreeUrl);
    Reg.WriteString('UserName', UserName);
    Reg.WriteString('UserPWD', UserPWD);
    Reg.WriteInteger('UpdateIntervalSec', UpdateIntervalSec);
    Reg.WriteBool('AutoStart', FAutoStart);
  finally
    FreeAndNil(Reg);
  end;
end;

initialization
  Config := TConfig.Create;

finalization
  Config.Free;
  
end.
