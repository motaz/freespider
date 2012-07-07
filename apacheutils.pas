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
    constructor Create(aPathInfo, aContentType, aRequestMethod, aQuery, aCookies, aUserAgent, PostedData,
      ContentLength: string);
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
  i: Integer;
  FileC: string;
begin
  IsFile:= False;
  FieldName:= '';
  Lines:= '';
  ContentLen:= StrToInt(fContentLength);
  ReadPos:= 0;
  fFilesCount:= 0;

  while ReadPos < ContentLen do
  begin

    Line:= '';
    repeat
      ch:= fPostedData[ReadPos];
      Inc(ReadPos);
      if (CH <> #10) or (IsFile) then
        Line:= Line + Ch;
    until (ReadPos >= ContentLen) or ((Ch = #10) and not IsFile);

    if (Pos(fBoundary, Line) > 0) then
    begin
      if FieldName <> '' then
      begin
        fContentFields.Add(FieldName + '=' + Copy(Lines, 2, Length(Lines) - 2));
        Lines:= '';
      end;
      Line:= '';
      repeat
        Inc(ReadPos);
        ch:= fPostedData[ReadPos];

        if (CH <> #10) then
          Line:= Line + Ch;
      until (ReadPos >= ContentLen) or (Ch = #10);


      // file
      if Pos('filename=', Line) > 0 then
      begin
        // File header
        IsFile:= True;
        Inc(fFilesCount);
        SetLength(fFiles, Length(fFiles) + 1);
        FileName:= Copy(Line, Pos('filename=', Line) + 10, Length(Line));
        FileName:= Copy(FileName, 1, Pos('"', FileName) - 1);

        Line:= Copy(Line, 1, Pos('filename=', Line) - 1);
        FileContentType:= '';
       // Inc(ReadPos);
        Line := '';

        // Content type
        repeat
          Inc(ReadPos);
          ch:= fPostedData[ReadPos];
          if CH <> #10 then
            FileContentType:= FileContentType + Ch;
        until (ReadPos >= ContentLen) or (Ch = #10);

        fFiles[High(fFiles)].ContentType:= Trim(Copy(FileContentType, Pos(':', FileContentType) + 1,
          Length(FileContentType)));
        fFiles[High(fFiles)].FileName:= FileName;

        Line:= '';
        Lines:= '';
        Inc(ReadPos, 2);
        // File contents
        FileC:= '';
        repeat
          Inc(ReadPos);
          ch:= fPostedData[ReadPos];
          FileC:= FileC + ch;
          Line:= Line + ch;

          // File termination
          if Pos(fBoundary, Line) > 0 then
          begin
            Delete(FileC, Length(FileC) - Length(fBoundary) - 0, Length(fBoundary) + 0);
            Line:= '';
            Break;
          end;
          if ch = #10 then
            Line:= '';
        until (ReadPos >= ContentLen);

        fFiles[High(fFiles)].FileContent:= Copy(FileC, 1, Length(FileC) - 5);
        fFiles[High(fFiles)].FieldName:= Fieldname;
        fFiles[High(fFiles)].FileSize:= Length(fFiles[High(fFiles)].FileContent);
        IsFile:= False;
        Line:= '';
        Lines:= ''

      end
      else
        IsFile:= False;

      if not isFile then
      begin
        FieldName:= Copy(Line, Pos('name="', Line) + 6, Length(Line));
        FieldName:= Copy(FieldName, 1, Pos('"', FieldName) - 1);
      end;
      if not IsFile then
        Lines:= '';

    end
    else
    begin
     if not IsFile then
       Lines:= Lines + Line;
    end;
  end;

  if (Lines <> '') then
  begin
    if (FieldName <> '') then
      fContentFields.Add(FieldName + '=' + Lines);
  end;
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

constructor TApacheRequest.Create(aPathInfo, aContentType, aRequestMethod, aQuery, aCookies, aUserAgent, PostedData,
  ContentLength: string);
begin
  fPathInfo:= aPathInfo;
  fContentType:= aContentType;
  fQueryString:= aQuery;
  fRequestMethod:= aRequestMethod;
  fUserAgent:= aUserAgent;
  fCookies:= aCookies;
  fPostedData:= PostedData;
  fContentLength:= ContentLength;
  inherited Create;
end;

end.

