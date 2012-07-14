{
*******************************************************************************************************************

  SpiderCGI:  Contains TSpiderCGI component which generate a CGI application. Any Free Spider CGI application should
              containe only one TSpiderCGI component
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modified: 14.July.2012

  Jul/2010 - Modified by Luiz Américo
    * Remove LCL dependency
    * Microoptimizations: add const to parameters, remove unnecessary typecast

*******************************************************************************************************************

}

unit SpiderCGI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpiderUtils, FileUtil, CGIUtils;

type

  TDataModuleClass = class of TDataModule;


  { TSpiderCGI }


  TSpiderCGI = class(TComponent)
  private
    fOnRequest: TSpiderEvent;
    fRequest: TSpiderRequest;
    fResponse: TSpiderResponse;
    fPath: string;
    fModules: array of String;
    fPathList: array of TStringList;

    function SearchActionInModule(const APath: string; AModule: TDataModule): Boolean;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property Request: TSpiderRequest read fRequest;
    property Response: TSpiderResponse read fResponse;
    procedure AddDataModule(const ADataModuleClassName: string; APaths: array of string);

    { Public declarations }
  published
    { Published declarations }
    property OnRequest: TSpiderEvent read FOnRequest  write FOnRequest;
    property Path: string read fPath;
  end;

implementation

uses SpiderAction;

{ TSpiderCGI }

procedure TSpiderCGI.Execute;
var
  i: Integer;
  Found: Boolean;
  APath: string;
  dmc: TDataModuleClass;
  DataMod: TDataModule;

begin
  try
  {$IFDEF apachemodule}
    Exit;
  {$ENDIF}

    APath:= Trim(LowerCase(GetEnvironmentVariable('PATH_INFO')));
    if (APath <> '') and (APath[Length(APath)] = '/') then
      APath:= Copy(APath, 1, Length(APath) - 1);
    Found:= False;

    // Search path in main module actions
    if APath <> '' then
      Found:= SearchActionInModule(APath, TDataModule(Owner));

    // Search path in other additional modules
    if (not Found) then
    for i:= 0 to High(fModules) do
    if fPathList[i].IndexOf(APath) <> -1 then
    begin
      dmc:= TDataModuleClass(FindClass(fModules[i]));
      if dmc <> nil then
      begin
        DataMod:= dmc.Create(nil);
        Found:= SearchActionInModule(APath, DataMod);
      end;
      if Found then
        Break;
    end;

    // triger SpiderCGI action when no path action found
    if (Not Found) and (Assigned(fOnRequest)) then
        fOnRequest(Self, fRequest, fResponse);

    // Put cookies
    for i:= 0 to Response.CookieList.Count - 1 do
      Writeln(Trim(Response.CookieList[i]));

    // Put custom header
    for i:= 0 to Response.CustomHeader.Count - 1 do
      Writeln(Response.CustomHeader[i]);

    // Send page response to the browser
    Writeln('CONTENT-TYPE: ' + Response.ContentType);
    Writeln;
    Writeln(Response.Content.Text);

  except
  on e: exception do
  begin
    Writeln('CONTENT-TYPE: TEXT/HTML');
    Writeln;
    Writeln('<font color=red>' + e.message + '<font>');
  end;
  end;
end;

procedure TSpiderCGI.AddDataModule(const ADataModuleClassName: string; APaths: array of string);
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

function TSpiderCGI.SearchActionInModule(const APath: string; AModule: TDataModule): Boolean;
var
  i: Integer;
begin
  Result:= False;
  for i:= 0 to AModule.ComponentCount - 1 do
    if AModule.Components[i] is TSpiderAction then
    if LowerCase(TSpiderAction(AModule.Components[i]).Path) = APath then
    begin
      TSpiderAction(AModule.Components[i]).DoRequest(Request, Response);
      Result:= True;
      Break;
    end;
end;

constructor TSpiderCGI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fRequest:= TCGIRequest.Create;
  fResponse:= TSpiderResponse.Create;
  fPath:= '/';
end;

destructor TSpiderCGI.Destroy;
var
  i: Integer;
begin
  fRequest.Free;
  fResponse.Free;
  for i:= 0 to High(fPathList) do
    fPathList[i].Free;
  SetLength(fPathList, 0);

  inherited Destroy;
end;


end.
