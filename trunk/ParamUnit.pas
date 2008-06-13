unit ParamUnit;

{*
  参数管理器。可以将字串中的变量替换为相应的值。
  建议使用的变量名格式：
    ${var}
{

*}

interface

uses SysUtils, StrUtils, Classes;

type

  TParamList = class(TObject)
  private
    FList: TStrings;

    function GetCount: Integer;
    procedure SetKeyValue(S: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetParam(const Key, Value: string);
    function GetParam(const Key: string; var Value: string): Boolean; overload;
    function GetParam(Index: Integer; var Key: string; var Value: string): Boolean; overload;
    function GetList(const Split: string): string;
    procedure SetList(const S, Split: string);
    procedure Clear;
    property Count: Integer read GetCount;

    function ReplaceAllParam(const S: string): string;
    class function ReplaceParam(const S, Key, Value: string): string;
    class function FindSubStr(const S, BeginS, EndS: string; var R: string): Boolean;
  end;

implementation

{ TParam }

procedure TParamList.SetKeyValue(S: string);
var
  I: Integer;
  Key, Value: string;
begin
  I := Pos('=', S);
  if I <= 0 then Exit;
  Key := LeftStr(S, I - 1);
  Value := MidStr(S, I + 1, MaxInt);
  SetParam(Key, Value);
end;

procedure TParamList.Clear;
begin
  FList.Clear;
end;

constructor TParamList.Create;
begin
  FList := TStringList.Create;
end;

destructor TParamList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TParamList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TParamList.GetList(const Split: string): string;
var
  I: Integer;
begin
  Result := '';
  if Count = 0 then Exit;

  for I := 0 to Count - 2 do
  begin
    Result := Result + FList[I] + Split;
  end;
  Result := Result + FList[Count - 1];
end;

procedure TParamList.SetList(const S, Split: string);
var
  B, E: Integer;
begin
  B := 1;
  E := 0;
  while E < Length(S) do
  begin
    E := PosEx(Split, S, E + 1);
    if E = 0 then E := Length(S) + 1;
    SetKeyValue(MidStr(S, B, E - B));
    B := E + 1;
  end;
end;

function TParamList.GetParam(Index: Integer; var Key: string; var Value: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := Pos('=', FList[Index]);
  if (I <> -1) then
  begin
    Key := LeftStr(FList[Index], I - 1);
    Value := MidStr(FList[Index], I + 1, MAXINT);
    Result := True;
  end;
end;

function TParamList.GetParam(const Key: string; var Value: string): Boolean;
var
  Index, J: Integer;
begin
  Result := False;
  Index := FList.IndexOfName(Key);
  if (Index <> -1) then
  begin
    J := Pos('=', FList[Index]);
    if (J <> 0) then
    begin
      Value := MidStr(FList[Index], J + 1, MAXINT);
      Result := True;
    end;
  end;
end;

procedure TParamList.SetParam(const Key, Value: string);
var
  I: Integer;
begin
  I := FList.IndexOfName(Trim(Key));
  if (I <> -1) then FList.Delete(I);
  FList.Add(Trim(Key) + '=' + Value);
end;

function TParamList.ReplaceAllParam(const S: string): string;
var
  I: Integer;
  Key: string;
  Value: string;
begin
  Result := S;
  for I := 0 to FList.Count - 1 do
  begin
    if GetParam(I, Key, Value) then
    begin
      Result := ReplaceParam(Result, Key, Value);
    end;
  end;
end;
   
class function TParamList.ReplaceParam(const S, Key, Value: string): string;
begin
  Result := StringReplace(S, Key, Value, [rfReplaceAll, rfIgnoreCase]);
end;

class function TParamList.FindSubStr(const S, BeginS, EndS: string; var R: string): Boolean;
var
  B, E: Integer;
begin
  Result := False;
  B := Pos(BeginS, S);
  if (B = 0) then Exit;
  B := B + Length(BeginS);
  E := PosEx(EndS, S, B);
  if (E = 0) then E := MaxInt;
  R := MidStr(S, B, E - B);
  Result := True;
end;

end.
