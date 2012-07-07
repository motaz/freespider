unit SpiderApache;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, ApacheUtils, SpiderUtils, SpiderAction;

type

  { TSpiderApache }

  TSpiderApache = class(TComponent)
  private
    fOnRequest: TSpiderEvent;
    fRequest: TSpiderRequest;
    fResponse: TSpiderResponse;
    fPath: string;
    fPathInfo: string;
    function SearchActionInModule(APath: string; AModule: TDataModule): Boolean;

    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Init(PathInfo, ContentType, RequestMethod, Query, Cookies, UserAgent, PostedData, ContentLength: string);
    destructor Destroy; override;
    function Execute: TSpiderResponse;
    { Public declarations }
  published
    property OnRequest: TSpiderEvent read FOnRequest  write FOnRequest;

    { Published declarations }
  end;

implementation



{ TSpiderApache }

function TSpiderApache.Execute: TSpiderResponse;
var
  Found: Boolean;
begin
  // Search path in main module actions
  Found:= False;
  if fPathInfo <> '' then
  begin
    Found:= SearchActionInModule(fPathInfo, TDataModule(Owner));
    if not Found then
      fResponse.Add(fPathInfo + ' not found');
  end;

  if (not Found) and (fPathInfo = '') and (Assigned(fOnRequest)) then
  begin
    if fRequest = nil then
      raise Exception.Create('TSpiderApache: Request object is not initialized');
    if fResponse = nil then
      raise Exception.Create('TSpiderApache: Response object is not initialized');

    fOnRequest(Self, fRequest, fResponse);
    Result:= fResponse;
  end;
  Result:= fResponse;

end;

constructor TSpiderApache.Create(TheOwner: TComponent);
begin
  fResponse:= TSpiderResponse.Create;
  fPath:= '/';
  fRequest:= nil;
  inherited Create(TheOwner);
end;

procedure TSpiderApache.Init(PathInfo, ContentType, RequestMethod, Query, Cookies, UserAgent, PostedData,
  ContentLength: string);
begin
  if (PathInfo <> '') and (PathInfo[Length(PathInfo)] = '/') then
    PathInfo:= Copy(PathInfo, 1, Length(PathInfo) - 1);
  fPathInfo:= Trim(PathInfo);

  if not Assigned(fRequest) then
    fRequest:= TApacheRequest.Create(PathInfo, ContentType, RequestMethod, Query, Cookies, UserAgent, PostedData,
      ContentLength);
end;

destructor TSpiderApache.Destroy;
begin
  fRequest.Free;
  fResponse.Free;
  inherited Destroy;
end;

function TSpiderApache.SearchActionInModule(APath: string; AModule: TDataModule): Boolean;
var
  i: Integer;
begin
  if AModule = nil then
    raise Exception.Create('error: Nil module passed');
  Result:= False;
  APath:= LowerCase(APath);
  for i:= 0 to AModule.ComponentCount - 1 do
  if AModule.Components[i] is TSpiderAction then
    if LowerCase(TSpiderAction(AModule.Components[i]).Path) = APath then
    begin
      TSpiderAction(AModule.Components[i]).DoRequest(fRequest, fResponse);
      Result:= True;
      Break;
    end;
end;


end.
