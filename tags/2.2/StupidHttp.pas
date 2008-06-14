unit StupidHttp;

interface

uses IdTCPClient, SysUtils, Classes, StrUtils, ParamUnit, IdURI, ConfigUnit,
  Wininet;

type

  TStupidHttp = class(TObject)
  private
    FGlobalParams: TParamList;
    FCookies: TParamList;
    procedure ReadHttpResponse(TcpClient: TIdTCPClient; var Header, Body: TStrings);

  protected
    function DoGet(const Params: TParamList): string;
    function DoPost(const Param: TParamList): string;

  public
    constructor Create(const Version: string);
    destructor Destroy; override;

    function Get(Url: string): string;

    function Post(Url: string; Params: TParamList): string;

    procedure SetIESessionCookie;
    procedure ClearIESessionCookie;
    procedure SetIECookie(const Url, Cookie: string);
  end;
implementation

const
  ParamUrl  = '${url}';
  ParamPath = '${path}';
  ParamHost = '${host}';
  ParamPort = '${port}';
  ParamCookies = '${cookies}';
  ParamVersion = '${bugfreehelper_ver}';
  
{ TWebAccess }

constructor TStupidHttp.Create(const Version: string);
begin
  FGlobalParams := TParamList.Create;
  FCookies := TParamList.Create;

  FGlobalParams.SetParam(ParamVersion, Version);
  FGlobalParams.SetParam(ParamCookies, '');
end;

destructor TStupidHttp.Destroy;
begin
  FreeAndNil(FCookies);
  FreeAndNil(FGlobalParams);
end;

procedure TStupidHttp.ReadHttpResponse(TcpClient: TIdTCPClient; var Header, Body: TStrings);
var
  Line: string;
  Cookie: string;
begin
  // read header
  Header.Clear;
  while True do
  begin
    Line := TcpClient.IOHandler.ReadLn();
    if Line = '' then Break;
    Header.Add(Line);
    if TParamList.FindSubStr(Line, 'Set-Cookie: ', ' path=/', Cookie) then
    begin
      FCookies.SetList(Cookie, ';');
      FGlobalParams.SetParam(ParamCookies, FCookies.GetList('; '));
    end;
  end;
  // read body
  Body.Clear;
  Body.Text := TcpClient.IOHandler.AllData;
end;

procedure TStupidHttp.SetIECookie(const Url, Cookie: string);
begin
  InternetSetCookie(PChar(Url), nil, PChar(Cookie));
end;

procedure TStupidHttp.SetIESessionCookie;
var
  Cookie: string;
begin
  FCookies.GetParam('PHPSESSID', Cookie);
  Cookie := 'PHPSESSID=' + Cookie + '; expires=Sun, 22-Feb-2099 08:08:08 GMT';
  SetIECookie(Config.BugFreeUrl + '/', Cookie);
end;

procedure TStupidHttp.ClearIESessionCookie;
var
  Cookie: string;
begin
  Cookie := 'PHPSESSID=0; expires=Sun, 22-Feb-2099 08:08:08 GMT';
  SetIECookie(Config.BugFreeUrl + '/', Cookie);
end;

function TStupidHttp.DoGet(const Params: TParamList): string;
const
  GetHttpHeader =
    'GET ${path} HTTP/1.1' + #13#10 +
    'Accept: application/x-shockwave-flash, image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*' + #13#10 +
    'Accept-Language: zh-cn' + #13#10 +
    'Accept-Encoding: gzip, deflate' + #13#10 +
    'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; WPS; .NET CLR 2.0.50727)' + #13#10 +
    'Host: ${host}' + #13#10 +
    'Connection: Keep-Alive' + #13#10 +
    'Cookie: bugfreehelper=${bugfreehelper_ver}; ${cookies} ' + #13#10;
var
  Host: string;
  Port: string;
  Request: string;
  Header, Body: TStrings;
  TcpClient: TIdTCPClient;
begin
  if not Params.GetParam(ParamHost, Host) then raise Exception.Create('没有提供host参数！');
  if not Params.GetParam(ParamPort, Port) then Port := '80';
  if Port = '' then Port := '80';

  Request := GetHttpHeader;
  Request := FGlobalParams.ReplaceAllParam(Request);
  Request := Params.ReplaceAllParam(Request);

  TcpClient := TIdTCPClient.Create(nil);
  Header := TStringList.Create;
  Body := TStringList.Create;
  try
    TcpClient.Connect(Host, StrToInt(Port));
    TcpClient.IOHandler.WriteLn(Request);
    TcpClient.IOHandler.WriteBufferFlush;
    ReadHttpResponse(TcpClient, Header, Body);
    Result := Body.Text;
  finally
    FreeAndNil(Header);
    FreeAndNil(Body);
    TcpClient.Disconnect;
    FreeAndNil(TcpClient);
  end;
end;

function TStupidHttp.DoPost(const Param: TParamList): string;
const
  PostHttpHeader =
    'POST ${path} HTTP/1.1' + #13#10 +
    'Accept: */*' + #13#10 +
    'Accept-Language: zh-cn' + #13#10 +
    'Referer: ${url}' + #13#10 +
    'method: POST ${url} HTTP/1.1' + #13#10 +
    'Content-Type: application/x-www-form-urlencoded' + #13#10 +
    'Accept-Encoding: gzip, deflate' + #13#10 +
    'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; WPS; .NET CLR 2.0.50727)' + #13#10 +
    'Host: www.bugfree.org.cn' + #13#10 +
    'Content-Length: ${post_length}' + #13#10 +
    'Connection: Keep-Alive' + #13#10 +
    'Cache-Control: no-cache' + #13#10 +
    'Cookie: bugfreehelper=${bugfreehelper_ver}; ${cookies} ' + #13#10 + #13#10 +
    '${post}';
var
  Host: string;
  Port: string;
  Request: string;
  I, PostLen: Integer;
  Header, Body: TStrings;
  TcpClient: TIdTCPClient;
begin
  if not Param.GetParam(ParamHost, Host) then raise Exception.Create('没有提供host参数！');
  if not Param.GetParam(ParamPort, Port) then Port := '80';
  if Port = '' then Port := '80';

  Request := PostHttpHeader;
  Request := FGlobalParams.ReplaceAllParam(Request);
  Request := Param.ReplaceAllParam(Request);
  // 计算Post部分的长度
  I := Pos(#13#10#13#10, Request);
  PostLen := Length(Request) - I - 4 + 1;
    Param.SetParam('${post_length}', IntToStr(PostLen));
  Request := Param.ReplaceAllParam(Request);

  TcpClient := TIdTCPClient.Create(nil);
  Header := TStringList.Create;
  Body := TStringList.Create;
  try
    TcpClient.Connect(Host, StrToInt(Port));
    TcpClient.IOHandler.Write(Request);
    TcpClient.IOHandler.WriteBufferFlush;
    ReadHttpResponse(TcpClient, Header, Body);
    Result := Body.Text;
  finally
    FreeAndNil(Header);
    FreeAndNil(Body);
    TcpClient.Disconnect;
    FreeAndNil(TcpClient);
  end;
end;

function TStupidHttp.Get(Url: string): string;
var
  Params: TParamList;
  Uri: TIdURI;
begin
  Params := TParamList.Create;
  Uri := TIdURI.Create(Url);
  try
    Params.SetParam(ParamHost, Uri.Host);
    Params.SetParam(ParamPath, Uri.Path + Uri.Document);
    Params.SetParam(ParamPort, Uri.Port);
    Result := DoGet(Params);
  finally
    FreeAndNil(Params);
  end;
end;

function TStupidHttp.Post(Url: string; Params: TParamList): string;
var
  Uri: TIdURI;
begin
  Uri := TIdURI.Create(Url);
  Params.SetParam(ParamUrl, Url);
  Params.SetParam(ParamHost, Uri.Host);
  Params.SetParam(ParamPath, Uri.Path + Uri.Document);
  Params.SetParam(ParamPort, Uri.Port);
  Result := DoPost(Params);
end;

end.
