unit apacheadapter;

{
*******************************************************************************************************************

  ApaacheApapter:  Decodes Apache Module reqeuest and response, triggers ApacheModule component
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 12.July.2012

*******************************************************************************************************************
}


{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, httpd, apr, SpiderApache, SpiderUtils;

function ProcessHandler(r: Prequest_rec; WebModule: TDataModuleClass; ModuleName, HandlerName: string): Integer;

implementation

function ProcessHandler(r: Prequest_rec; WebModule: TDataModuleClass; ModuleName, HandlerName: string): Integer;
var
  RequestedHandler: string;
  Buf: array [0 .. 20024] of Char;
  NumRead: Integer;
  Line: string;
  Head: Papr_array_header_t;
  Access: Phtaccess_result;
  Web: TDataModule;
  i: Integer;
  SpiderApacheObj: TSpiderApache;
  aResponse: TSpiderResponse;
  ContentType: string;
  j: Integer;
  PostedData: string;
  Data: Pointer;
  DataLen: Integer;
begin
  RequestedHandler := r^.handler;

  { We decline to handle a request if hello-handler is not the value of r->handler }
  if not SameText(RequestedHandler, HANDLERNAME) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { The following line just prints a message to the errorlog }
  ap_log_error(PChar(MODULENAME), 54, APLOG_NOERRNO or APLOG_NOTICE,
   {$ifndef Apache1_3}0,{$endif} r^.server,
   'mod_hello: %s', [PChar('Before content is output')]);

  { We set the content type before doing anything else }

  ap_set_content_type(r, 'text/html');


  { If the request is for a header only, and not a request for
   the whole content, then return OK now. We don't have to do
   anything else. }
  if (r^.header_only <> 0) then
  begin
    Result := OK;
    Exit;
  end;

  try
    Line:= '';
    PostedData:= '';
    // read posted data
    if (r^.method = 'POST') then
    begin
      ap_setup_client_block(r, REQUEST_CHUNKED_DECHUNK);
      repeat
         NumRead:= ap_get_client_block(r, Buf, SizeOf(Buf));
        SetLength(Line, NumRead);
        Move(Buf, Pointer(Line)^, NumRead);
        PostedData:= PostedData + Line;
      until NumRead = 0;
    end;

    web:= WebModule.Create(nil);
    with web do
    for i:= 0 to ComponentCount - 1 do
    if Components[i] is TSpiderApache then
    begin
      aResponse:= nil;
      SpiderApacheObj:= Components[i] as TSpiderApache;
      ContentType:= apr_table_get(r^.headers_in, 'CONTENT-TYPE');
      if Trim(ContentType) = '' then
        ContentType:= r^.content_type;
      SpiderApacheObj.Init(r^.path_info, ContentType, r^.method, r^.args, apr_table_get(r^.headers_in, 'COOKIE'),
      apr_table_get(r^.headers_in, 'User-Agent'), Posteddata, apr_table_get(r^.headers_in, 'Content-Length'),
        apr_table_get(r^.headers_in, 'REFERER'), r^.connection^.remote_ip, r^.uri, ap_get_server_version);
      aResponse:= SpiderApacheObj.Execute;
      if Assigned(aResponse) then
      begin
        ap_set_content_type(r, PChar(aResponse.ContentType));
        with aResponse.CustomHeader do
        for j:= 0 to Count - 1 do
          apr_table_set(r^.headers_out, PChar(Copy(Strings[j], 1, Pos(':', Strings[j]) - 1)),
            PChar(Copy(Strings[j], Pos(':', Strings[j]) + 1, Length(Strings[j])) ));
        DataLen:= Length(aResponse.Content.Text);
        Data:= GetMem(DataLen);
        Move(Pointer(aResponse.Content.Text)^, Data^, DataLen);
        ap_rwrite(Data, DataLen, r);
      end;
      Break;
    end;

  except
    on e: exception do
    ap_rputs(PChar('<br/> Error in WebModule : <font color=red>' + e.Message + '</font>'), r);
  end;
  web.Free;

 { ap_rputs(PChar('query: <font color=gray>' + r^.args + '</font>'), r);
  ap_rputs(PChar('<br/>Hostname: <font color=gray>' +  r^.hostname + '</font>'), r);
  ap_rputs(PChar('<br/>Method: <font color=gray>' +  r^.method + '</font>'), r);
  ap_rputs(PChar('<br/>the request: <font color=gray>' +  r^.the_request + '</font>'), r);
  ap_rputs(PChar('<br/>Protocol: <font color=gray>' +  r^.protocol + '</font>'), r);
  ap_rputs(PChar('<br/>Content Type: <font color=gray>' +  r^.content_type + '</font>'), r);
  ap_rputs(PChar('<br/>Encoding: <font color=gray>' +  r^.content_encoding + '</font>'), r);
  ap_rputs(PChar('<br/>Auth type: <font color=gray>' +  r^.ap_auth_type + '</font>'), r);
  ap_rputs(PChar('<br/>User: <font color=gray>' +  r^.user + '</font>'), r);
  ap_rputs(PChar('<br/>Status Line: <font color=gray>' +  r^.status_line + '</font>'), r);
  ap_rputs(PChar('<br/>VList Validator: <font color=gray>' +  r^.vlist_validator + '</font>'), r);
  ap_rputs(PChar('<br/>URI: <font color=gray>' +  r^.uri+ '</font>'), r);
  ap_rputs(PChar('<br/>Range: <font color=gray>' +  r^.range+ '</font>'), r);
  ap_rputs(PChar('<br/>Path Info: <font color=gray>' +  r^.path_info+ '</font>'), r);
  ap_rputs(PChar('<br/>Canonical Filename: <font color=gray>' +  r^.canonical_filename+ '</font>'), r);
  ap_rputs(PChar('<br/>Handler: <font color=gray>' +  r^.handler + '</font>'), r);
  ap_rputs(PChar('<br/>show_mpm: <font color=gray>' + ap_show_mpm + '</font>'), r);
  ap_rputs(PChar('<br/>auth_type: <font color=gray>' +  ap_auth_type(r) + '</font>'), r);
  ap_rputs(PChar('<br/>auth_name: <font color=gray>' +  ap_auth_name(r) + '</font>'), r);
  ap_rputs(PChar('<br/>server description: <font color=gray>' +  ap_get_server_description + '</font>'), r);
  ap_rputs(PChar('<br/>remote longname: <font color=gray>' +  ap_get_remote_logname(r) + '</font>'), r);
  ap_rputs(PChar('<br/>Status: <font color=gray>' +  IntToStr(r^.status) + '</font>'), r);

  ap_rputs(PChar('<br/>User-Agent: <font color=gray>' +  apr_table_get(r^.headers_in, 'User-Agent') + '</font>'), r);

  // Read cookies:
  ap_rputs(PChar('<br/>Cookies: <font color=gray>' +  apr_table_get(r^.headers_in, 'COOKIE') + '</font>'), r);

  // Set cookies:
  apr_table_set(r^.headers_out, 'Set-Cookie', 'sessionid=122');


  // Retrieve posted data
  if (r^.method = 'POST') then
  begin
    ap_setup_client_block(r, REQUEST_CHUNKED_ERROR);
    ap_rputs('contents:', r);
    Buf:= StrAlloc(1024);
    repeat
       NumRead:= ap_get_client_block(r, Buf, 1024);
       ap_rputs(PChar(Copy(Buf, 0, NumRead)), r);
    until NumRead = 0;

  end;}


  Result:= Ok;
end;


end.

