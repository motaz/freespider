unit ApacheUtils;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, SpiderUtils;

type

  { TApacheRequest }

  TApacheRequest = class(TSpiderRequest)
  private
    fCookies: string;
    fPostedData: string;
    procedure ReadCookies; override;
    procedure DecodeMultiPart; override;
    function ReadContent: string; override;
    procedure ReadVariables; override;
    procedure DisplayErrorMessage(Msg: string); override;
  public
    constructor Create(aRequestMethod, aQuery, aCookies, aUserAgent, PostedData: string);
  end;


implementation

{ TApacheRequest }

procedure TApacheRequest.ReadCookies;
var
  TempStr: string;
  Line: string;
  i: Integer;
begin
  Line:='';
  TempStr:= fCookies;
  for i:= 1 to Length(TempStr) do
  if Tempstr[i] = ';' then
  begin
    fCookieList.Add(Trim(Line));
    Line:= '';
  end
  else
    Line:= Line + TempStr[i];
  if Line <> '' then
    fCookieList.Add(Trim(Line));
end;

procedure TApacheRequest.DecodeMultiPart;
begin

end;

function TApacheRequest.ReadContent: string;
begin
  Result:= fPostedData;
end;

procedure TApacheRequest.ReadVariables;
begin
{  fRequestMethod:= GetEnvironmentVariable('REQUEST_METHOD');
  fUserAgent:= GetEnvironmentVariable('HTTP_USER_AGENT');
  fRemoteAddress:= GetEnvironmentVariable('REMOTE_ADDR');
  fContentType:= GetEnvironmentVariable('CONTENT_TYPE');
  fQueryString:= GetEnvironmentVariable('QUERY_STRING');}
end;

procedure TApacheRequest.DisplayErrorMessage(Msg: string);
begin
  raise Exception.Create(Msg);
end;

constructor TApacheRequest.Create(aRequestMethod, aQuery, aCookies, aUserAgent, PostedData: string);
begin
  fQueryString:= aQuery;
  fRequestMethod:= aRequestMethod;
  fUserAgent:= aUserAgent;
  fCookies:= aCookies;
  fPostedData:= PostedData;
  inherited Create;
end;

end.

