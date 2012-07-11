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
    fModules: array of String;
    fPathList: array of TStringList;
    function SearchActionInModule(APath: string; AModule: TDataModule): Boolean;

    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Init(PathInfo, ContentType, RequestMethod, Query, Cookies, UserAgent, PostedData,
      ContentLength, Referer, RemoteIP: string);
    destructor Destroy; override;
    function Execute: TSpiderResponse;
    procedure AddDataModule(const ADataModuleClassName: string; APaths: array of string);
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
  i: Integer;
  dmc: TDataModuleClass;
  DataMod: TDataModule;
begin
  // Search path in main module actions
  Found:= False;
  if fPathInfo <> '' then
  begin
    Found:= SearchActionInModule(fPathInfo, TDataModule(Owner));
  end;
  DataMod:= nil;

  // Search path in other additional modules
  if (not Found) then
  for i:= 0 to High(fModules) do
  if fPathList[i].IndexOf(fPathInfo) <> -1 then
  begin
    dmc:= TDataModuleClass(FindClass(fModules[i]));
    if dmc <> nil then
    begin
      DataMod:= dmc.Create(nil);
      Found:= SearchActionInModule(fPathInfo, DataMod);
    end;
    if Found then
      Break;
  end;

  // Default path /
  if (not Found) and (fPathInfo = '') and (Assigned(fOnRequest)) then
  begin
    Found:= True;
    if fRequest = nil then
      raise Exception.Create('TSpiderApache: Request object is not initialized');
    if fResponse = nil then
      raise Exception.Create('TSpiderApache: Response object is not initialized');

    fOnRequest(Self, fRequest, fResponse);
    Result:= fResponse;
  end;

  if not Found then
    fResponse.Add(fPathInfo + ' not found');

  Result:= fResponse;

  if Assigned(DataMod) then
    DataMod.Free;

end;

constructor TSpiderApache.Create(TheOwner: TComponent);
begin
  fResponse:= TSpiderResponse.Create;
  fPath:= '/';
  fRequest:= nil;
  inherited Create(TheOwner);
end;

procedure TSpiderApache.Init(PathInfo, ContentType, RequestMethod, Query, Cookies, UserAgent, PostedData,
  ContentLength, Referer, RemoteIP: string);
begin
  if (PathInfo <> '') and (PathInfo[Length(PathInfo)] = '/') then
    PathInfo:= Copy(PathInfo, 1, Length(PathInfo) - 1);
  fPathInfo:= Trim(PathInfo);

  if not Assigned(fRequest) then
    fRequest:= TApacheRequest.Create(PathInfo, ContentType, RequestMethod, Query, Cookies, UserAgent, PostedData,
      ContentLength, Referer, RemoteIP);
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

procedure TSpiderApache.AddDataModule(const ADataModuleClassName: string; APaths: array of string);
var
  i: Integer;
begin
  SetLength(fModules, Length(fModules) + 1);
  fModules[High(fModules)]:= ADataModuleClassName;
  SetLength(fPathList, Length(fPathList) + 1);
  fPathList[High(fPathList)]:= TStringList.Create;
  for i:= 0 to High(APaths) do
    fPathList[High(fPathList)].Add(LowerCase(APaths[i]));
end;


end.
