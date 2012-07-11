{
*******************************************************************************************************************

  SpiderUtils:  Contains Request and Response classes for Free Spider web application for lazarus
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 7.July.2012

  Jul/2010 - Modified by Luiz Américo
    * Remove LCL dependency
    * Fix memory leaks 

*******************************************************************************************************************

}



unit SpiderUtils;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type


  TRequestFile = record
    FileName: string;
    FieldName: string;
    FileContent: string;
    ContentType: string;
    FileSize: Integer;
  end;

  TRequestFiles = array of TRequestFile;

  { TSpiderRequest }

  TSpiderRequest = class(TObject)
  protected
    fRequestMethod: string;
    fUserAgent: string;
    fRemoteAddress: string;
    fContentType: string;
    fQueryString: string;
    fContentLength: string;

    fQueryFields: TStringList;

    fContentFields: TStringList;

    fCookieList: TStringList;

    fMultiPart: Boolean;
    fBoundary: string;
    fFilesCount: Integer;
    fFiles: TRequestFiles;
    fPathInfo: string;
    fReferer: string;

    procedure ReadCookies; virtual; abstract;
    procedure DecodeMultiPart; virtual; abstract;
    function ReadContent: string; virtual; abstract;
    procedure DisplayErrorMessage(Msg: string); virtual; abstract;
    procedure ReadVariables; virtual; abstract;
    procedure DecodeRequest(AText: string; var List: TStringList);

  public
    constructor Create;
    destructor Destroy; override;
    function Query(FieldValue: string): string;
    function Form(FieldValue: string): string;
    function GetCookie(AName: string): string;

    property Queryfields: TStringList read fQueryFields;
    property ContentFields: TStringList read fContentFields;
    property RequestMethod: string read fRequestMethod;
    property UserAgent: string read fUserAgent;
    property RemoteAddress: string read fRemoteAddress;
    property ContentType: string read fContentType;
    property FilesCount: Integer read fFilesCount;
    property ContentFiles: TRequestFiles read fFiles;
    property PathInfo: string read fPathInfo;
    property Referer: string read fReferer;

    property Cookies: TStringList read fCookieList;
  end;

  { TSpiderResponse }

  TSpiderResponse = class (TObject)
  private
    fCookieList: TStringList;
    fContent: TStringList;
    fCustomHeader: TStringList;
    fContentType: string;
  public
    procedure SetCookie(AName, AValue, APath: string; ExpiresInGMT: TDateTime = -1);
    procedure DeleteCookie(AName, APath: string);
    procedure Add(HTMLText: string);

    property CookieList: TStringList read fCookieList;

    property Content: TStringList read fContent;
    property ContentType: string read fContentType write fContentType;
    property CustomHeader: TStringList read fCustomHeader;

    procedure SendRedirect(AUrl: string; RedirectionHint: string = 'Redirecting..');

    constructor Create;
    destructor Destroy; override;
  end;

  TSpiderEvent  = procedure(Sender: TObject; Request: TSpiderRequest;
    var Response: TSpiderResponse) of object;

implementation

{ TRequest }

procedure  TSpiderRequest.DecodeRequest(AText: string; var List: TStringList);
var
  i: Integer;
  Hex: string;
  Dec: Integer;
  Line: string;
begin
  Line:='';
  List.Clear;
  i:= 1;
  while i <= Length(AText) do
  begin
    if AText[i] = '%' then
    begin
      Hex:= Copy(AText, i + 1, 2);
      i:= i + 2;
      Dec:= StrToInt('$' + Hex);
      Line:= Line + Chr(Dec);
    end
    else
    if AText[i] = '+' then
      Line:= Line + ' '
    else
    if AText[i] = '&' then
    begin
      List.Add(Line);
      Line:= '';
    end
    else
      Line:= Line + AText[i];
    Inc(i);
  end;
  if Line <> '' then
    List.Add(Line);

end;



constructor TSpiderRequest.Create;
begin
  try
    ReadVariables;
    fFilesCount:= 0;
    fQueryFields:= TStringList.Create;
    fContentFields:= TStringList.Create;
    fCookieList:= TStringList.Create;

    ReadCookies;
    fMultiPart:= Pos('multipart/form-data', fContentType) > 0;
    if fMultiPart then
      fBoundary:= Trim(Copy(fContentType, Pos('boundary=', fContentType) + 10,
        Length(fContentType)));

    if LowerCase(fRequestMethod) = 'get' then
      DecodeRequest(fQueryString, fQueryFields)
    else
    if LowerCase(fRequestMethod) = 'post' then
    begin
      if fMultiPart
        then DecodeMultiPart
        else DecodeRequest(ReadContent, fContentFields);
    end; // if LowerCase..

  except
  on e: exception do
  begin
    DisplayErrorMessage(e.Message);
  end;
  end;

end;

destructor TSpiderRequest.Destroy;
begin
  fQueryFields.Free;
  fContentFields.Free;
  fCookieList.Free;
end;

function TSpiderRequest.Query(FieldValue: string): string;
begin
  Result:= fQueryFields.Values[FieldValue];
end;

function TSpiderRequest.Form(FieldValue: string): string;
begin
  Result:= fContentFields.Values[FieldValue];
end;

function TSpiderRequest.GetCookie(AName: string): string;
begin
  Result:= fCookieList.Values[AName];
end;

{ TSpiderResponse }

constructor TSpiderResponse.Create;
begin
  fContent:= TStringList.Create;
  fCookieList:= TStringList.Create;
  fCustomHeader:= TStringList.Create;
  fContentType:= 'TEXT/HTML';
end;

destructor TSpiderResponse.Destroy;
begin
  fContent.Free;
  fCookieList.Free;
  fCustomHeader.Free;
end;

procedure TSpiderResponse.SetCookie(AName, AValue, APath: string; ExpiresInGMT: TDateTime = -1);
var
  Line: string;
begin
  Line:= 'Set-Cookie: ' + Trim(AName) + '=' + AValue + '; path=' + APath;
  if ExpiresInGMT <> -1 then
    Line:= Line + '; expires=' + FormatDateTime('ddd, dd-mmm-yyyy hh:nn:ss', ExpiresInGMT) + ' GMT';
  fCookieList.Add(Line);
end;

procedure TSpiderResponse.DeleteCookie(AName, APath: string);
begin
  fCookieList.Add('Set-Cookie: ' + AName + '=; path=' + APath + '; expires=Thu, 01-Jan-1970 00:00:01 GMT');
end;

procedure TSpiderResponse.Add(HTMLText: string);
begin
  Content.Add(HTMLText);
end;

// By: Sammarco Francesco

procedure TSpiderResponse.SendRedirect(AUrl: string; RedirectionHint: string = 'Redirecting..');
begin
  with fContent do
  begin
    Clear;
    Add('<HTML>');
    Add('<HEAD>');
    Add('<META HTTP-EQUIV="REFRESH" CONTENT="0; URL=' + AUrl + '">');
    Add('</HEAD>');
    Add('<BODY>');
    Add(RedirectionHint);
    Add('</BODY>');
    Add('</HTML>');
  end;
end;

end.

