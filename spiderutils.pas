{
*******************************************************************************************************************

  SpiderUtils:  Contains Request and Response classes for Free Spider web application for lazarus
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 17.June.2012

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
  private
    fRequestMethod: string;
    fUserAgent: string;
    fRemoteAddress: string;
    fContentType: string;

    fQueryFields: TStringList;

    fContentFields: TStringList;

    fCookieList: TStringList;

    fMultiPart: Boolean;
    fBoundary: string;
    fFilesCount: Integer;
    fFiles: TRequestFiles;



    procedure ReadCookies;
    procedure DecodeMultiPart;

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

procedure  DecodeRequest(AText: string; var List: TStringList);
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

procedure TSpiderRequest.ReadCookies;
var
  TempStr: string;
  Line: string;
  i: Integer;
begin
  Line:='';
  TempStr:= GetEnvironmentVariable('HTTP_COOKIE');
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


procedure TSpiderRequest.DecodeMultiPart;
var
  Line: string;
  FieldName: string;
  Lines: string;
  IsFile: Boolean;
  FileContentType: string;
  FileName: string;
  Ch: Char;
  ReadPos: Integer;
  ContentLen: Integer;
begin
  IsFile:= False;
  ReadPos:= 0;
  FieldName:='';
  Lines:='';
  ContentLen:= StrToInt(GetEnvironmentVariable('CONTENT_LENGTH'));

  while ReadPos < ContentLen do
  begin

   Line:= '';
   repeat
     Read(Ch);
     Inc(ReadPos);
     Line:= Line + Ch;
   until (ReadPos >= ContentLen) or (Ch = #10);

    if (Pos(fBoundary, Line) > 0) then
    begin
      if FieldName <> '' then
      begin
        if IsFile then
        begin
          fFiles[High(fFiles)].FileContent:= Copy(Lines, 1, Length(Lines) - 2);
          fFiles[High(fFiles)].FieldName:= Fieldname;
          fFiles[High(fFiles)].FileSize:= Length(fFiles[High(fFiles)].FileContent);
          IsFile:= False;
        end
        else
          fContentFields.Add(FieldName + '=' + Lines);
        Lines:= '';
      end;
      Line:= '';
      repeat
        Read(Ch);
        Inc(ReadPos);
        if CH <> #10 then
        Line:= Line + Ch;
      until (ReadPos >= ContentLen) or (Ch = #10);


      if Pos('filename=', Line) > 0 then
      begin
        IsFile:= True;
        Inc(fFilesCount);
        SetLength(fFiles, Length(fFiles) + 1);
        FileName:= Copy(Line, Pos('filename=', Line) + 10, Length(Line));
        FileName:= Copy(FileName, 1, Pos('"', FileName) - 1);

        Line:= Copy(Line, 1, Pos('filename=', Line) - 1);
        FileContentType:= '';
        repeat
          Read(Ch);
          Inc(ReadPos);
          if CH <> #10 then
            FileContentType:= FileContentType + Ch;
        until (ReadPos >= ContentLen) or (Ch = #10);

        fFiles[High(fFiles)].ContentType:= FileContentType;
        fFiles[High(fFiles)].FileName:= FileName;
        repeat
          Read(Ch);
          Inc(ReadPos);
        until (ReadPos >= ContentLen) or (Ch = #10);
      end
      else
        IsFile:= False;

      FieldName:= Copy(Line, Pos('name="', Line) + 6, Length(Line));
      FieldName:= Copy(FieldName, 1, Pos('"', FieldName) - 1);
      Lines:= '';

    end
    else
      Lines:= Lines + Line;
  end;

  if (Lines <> '') then
  begin
    if IsFile then
    begin
      fFiles[High(fFiles)].FileContent:= Copy(Lines, 1, Length(Lines) - 2);
      fFiles[High(fFiles)].FieldName:= Fieldname;
      fFiles[High(fFiles)].FileSize:= Length(fFiles[High(fFiles)].FileContent);
    end
    else
    if (FieldName <> '') then
      fContentFields.Add(FieldName + '=' + Lines);
  end;


end;


constructor TSpiderRequest.Create;

  function ReadContent : string;
  var
    ContentLen : integer;
    ReadPos    : integer;
    ch : char;
  begin
    ContentLen:= StrToInt(GetEnvironmentVariable('CONTENT_LENGTH'));
    ReadPos := 0;
    Result  := '';
    while ReadPos < ContentLen
    do begin
      Read(Ch);
      inc (ReadPos);
      if (Result <> #10)
        then Result := Result + Ch;
    end;
  end;

begin
  try
    fFilesCount:= 0;
    fQueryFields:= TStringList.Create;
    fContentFields:= TStringList.Create;
    fCookieList:= TStringList.Create;

    fRequestMethod:= GetEnvironmentVariable('REQUEST_METHOD');
    fUserAgent:= GetEnvironmentVariable('HTTP_USER_AGENT');
    fRemoteAddress:= GetEnvironmentVariable('REMOTE_ADDR');
    fContentType:= GetEnvironmentVariable('CONTENT_TYPE');
    ReadCookies;
    fMultiPart:= Pos('multipart/form-data', fContentType) > 0;
    if fMultiPart then
      fBoundary:= Trim(Copy(fContentType, Pos('boundary=', fContentType) + 10,
        Length(fContentType)));

    if LowerCase(fRequestMethod) = 'get' then
      DecodeRequest(GetEnvironmentVariable('QUERY_STRING'), fQueryFields)
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
    Writeln('CONTENT-TYPE: TEXT/HTML');
    Writeln;
    Writeln('<font color=red>' + e.message + '<font>');
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

