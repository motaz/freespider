{
*******************************************************************************************************************

  CGIUtils:  Contains TCGIRequest class for CGI protocol
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 12.July.2012

*******************************************************************************************************************
}

unit CGIUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpiderUtils;

type

  { TCGIRequest }

  TCGIRequest = class(TSpiderRequest)
    private
      procedure ReadCookies; override;
      procedure DecodeMultiPart; override;
      function ReadContent: string; override;
      procedure ReadVariables; override;
      procedure DisplayErrorMessage(Msg: string); override;
    public

  end;

implementation


procedure TCGIRequest.ReadCookies;
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

procedure TCGIRequest.DecodeMultiPart;
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

function TCGIRequest.ReadContent : string;
var
  ContentLen : integer;
  ReadPos    : integer;
  ch : char;
begin
  ContentLen:= StrToInt(GetEnvironmentVariable('CONTENT_LENGTH'));
  ReadPos := 0;
  Result  := '';
  while ReadPos < ContentLen do
  begin
    Read(Ch);
    inc (ReadPos);
    if (Result <> #10)
      then Result := Result + Ch;
  end;
end;

procedure TCGIRequest.ReadVariables;
begin
  fRequestMethod:= GetEnvironmentVariable('REQUEST_METHOD');
  fUserAgent:= GetEnvironmentVariable('HTTP_USER_AGENT');
  fRemoteAddress:= GetEnvironmentVariable('REMOTE_ADDR');
  fContentType:= GetEnvironmentVariable('CONTENT_TYPE');
  fQueryString:= GetEnvironmentVariable('QUERY_STRING');
  fURI:= LowerCase(GetEnvironmentVariable('REQUEST_URI'));
  fWebServerSoftware:= LowerCase(GetEnvironmentVariable('SERVER_SOFTWARE'));

  fPathInfo:= Trim(GetEnvironmentVariable('PATH_INFO'));
  if (fPathInfo <> '') and (fPathInfo[Length(fPathInfo)] = '/') then
    fPathInfo:= Copy(fPathInfo, 1, Length(fPathInfo) - 1);

  fReferer:= Trim(LowerCase(GetEnvironmentVariable('HTTP_REFERER')));

  fIsApache:= False;
  fIsCGI:= True;
end;

procedure TCGIRequest.DisplayErrorMessage(Msg: string);
begin
  Writeln('CONTENT-TYPE: TEXT/HTML');
  Writeln;
  Writeln('<font color=red>' + Msg + '<font>');
end;



end.

