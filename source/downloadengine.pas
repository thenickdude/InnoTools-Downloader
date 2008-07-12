unit downloadengine;

interface


{ DownloadEngine.pas - The code that actually performs download/size check
                       operations on HTTP and FTP servers. Requires Synapse.

  By Nicholas Sherlock - http://www.sherlocksoftware.org }


uses SysUtils, blcksock, classes, windows, wininet, proceduretomethod, httpsend,
  synautil, messages, ftpsend, math;

type
  TDownloadStage = (dsStartingDownload, dsDownloading, dsDone, dsError);

  TDownloadProgressEvent = function(stage: TDownloadStage; downbytes, bytestotal: longword; tag: TObject): boolean of object;

  TInetErrorEvent = procedure(code: cardinal; message: string) of object;

  TFTPGetThread = class(TThread)
  private
    foperationdone, foperationsuccess: boolean;
    furl: string;
    fFtp: TFTPSend;
  protected
    procedure Abort;
    constructor create(const url: string; ftp: TFTPSend);
    procedure Execute; override;

    property OperationDone: boolean read foperationdone;
    property OperationSuccess: boolean read foperationsuccess;
  end;

  TGetThread = class(TThread)
  private
    foperationdone, foperationsuccess: boolean;
    furl: string;
    fhttp: THTTPSend;
  protected
    procedure Abort;
    constructor create(const url: string; http: THTTPSend);
    procedure Execute; override;

    property OperationDone: boolean read foperationdone;
    property OperationSuccess: boolean read foperationsuccess;
  end;

  TFTPSizeThread = class(TThread)
  private
    foperationdone, foperationsuccess: boolean;
    fsize: integer;
    furl: string;
    fFtp: TFTPSend;
  protected
    procedure Abort;
    constructor create(const url: string; ftp: TFTPSend);
    procedure Execute; override;

    property Size: integer read fsize;
    property OperationDone: boolean read foperationdone;
    property OperationSuccess: boolean read foperationsuccess;
  end;

  THeadThread = class(TThread)
  private
    foperationdone, foperationsuccess: boolean;
    fsize: integer;
    furl: string;
    fhttp: THTTPSend;
  protected
    procedure Abort;
    constructor create(const url: string; http: THTTPSend);
    procedure Execute; override;

    property Size: integer read fsize;
    property OperationDone: boolean read foperationdone;
    property OperationSuccess: boolean read foperationsuccess;
  end;

  TPostThread = class(TThread)
  private
    foperationdone, foperationsuccess: boolean;
    furl, fData: string;
    fhttp: THTTPSend;
  protected
    procedure Abort;
    constructor create(const url, data: string; http: THTTPSend);
    procedure Execute; override;

    property OperationDone: boolean read foperationdone;
    property OperationSuccess: boolean read foperationsuccess;
  end;

  TDownloadEngine = class
  private
    fDownloadFileWritten: cardinal;
    fDownloadTag: TObject;

    fHttp: THTTPSend;
    fFtp: TFTPSend;

    fagent: string;
    fTimeOut: integer;

    fLastError: string;

    fOnINetError: TINetErrorEvent;

    fProgress: TDownloadProgressEvent;

    function GetFTPSize(const url: string; out size: longword): boolean;
    function GetHTTPSize(const url: string; out size: longword): boolean;

    procedure HTTPSocketProgress(Sender: TObject; Reason: THookSocketReason; const Value: string);
    procedure FTPSocketProgress(Sender: TObject; Reason: THookSocketReason; const Value: string);

    function DownloadHTTPFileToStream(const url: string; stream: TStream;
      tag: TObject): boolean;
    function DownloadFTPFileToStream(const url: string; stream: TStream;
      tag: TObject): boolean;
  public

    constructor Create;
    destructor Destroy; override;

    function PostPage(const url, data: string; out resultbuffer: string): boolean;

    function GetWebFileSize(const url: string; out size: longword): boolean;
    function DownloadWebFileToStream(const url: string; stream: TStream;
      tag: TObject): boolean;

    property Agent: string read fagent write fagent;
    property TimeOut: integer read ftimeout write ftimeout;
    property OnINetError: TInetErrorEvent read fonINetError write fonINetError;
    property Progress: TDownloadProgressEvent read fProgress write fProgress;
    property LastError: string read fLastError;
  end;

implementation

uses liteui;

function InternetQueryString(option: cardinal): string;
var buf: string;
  len: cardinal;
begin
  setlength(buf, 1000);

  len := length(buf);

  if InternetQueryOption(nil, option, @buf[1], len) then begin
    setlength(buf, len);
    result := buf;
  end else
    result := '';
end;

{Unpack a 'key=value;key=value;' or 'key=value key=value' or 'value' list
 of proxy addresses from INTERNET_PROXY_INFO. 'value' is an 'all' proxy, for all protocols}

procedure UnpackProxyList(list: string; parts: TStringList);
var sep: integer;
  eq, semi, space: integer;
  piece: string;
begin
  while (length(list) > 0) do begin

    semi := pos(';', list);
    space := pos(' ', list);

    if (semi = 1) or (space = 1) then begin
  //Leading separators
      Delete(list, 1, 1);
      continue;
    end;

    if (semi > 0) or (space > 0) then begin
  //More than one piece to process

  //Get the first separator
      if (semi <> 0) and (space <> 0) then
        sep := min(semi, space)
      else if (semi <> 0) then
        sep := semi
      else
        sep := space;

  //Grab that first chunk
      piece := copy(list, 1, sep - 1);
      list := copy(list, sep + 1, length(list));
    end else begin
 //Only one piece remaining
      piece := list;
      list := '';
    end;

 //Process the piece

    eq := pos('=', piece);
    if eq > 0 then begin
    //Key/value pair
      parts.Values[lowercase(copy(piece, 1, eq - 1))] := copy(piece, eq + 1, length(piece));
    end else begin
     //No key/value
      parts.Values['all'] := piece;
    end;
  end;
end;

//Set up the proxy information from IE settings

procedure SetProxy(http: THTTPSend);
var
  buffer: array[0..4095] of byte;
  info: TInternetProxyInfo absolute buffer;
  len: cardinal;
  host: string;
  list: TStringList;
begin
 { InternetQueryOption fails with ERROR_INSUFFICIENT_BUFFER if the buffer is
   SizeOf(TInternetProxyInfo) bytes, had to make it big to have it work. MSDN
   shows the same thing in their example code. Weird.}
  len := sizeof(buffer);
  if InternetQueryOption(nil, INTERNET_OPTION_PROXY, @info, len) then begin
    if info.dwAccessType = INTERNET_OPEN_TYPE_PROXY then begin

      host := info.lpszProxy;

      list := TStringList.create;
      try
        UnpackProxyList(host, list);

        if length(list.Values['http']) > 0 then
          host := list.values['http']
        else if length(list.Values['all']) > 0 then
          host := list.values['all']
        else exit; //no proxy for this protocol

      finally
        list.free;
      end;
      if RPos(':', host) > 0 then begin
      //Includes a port specification
        http.ProxyPort := Copy(host, RPos(':', host) + 1, length(host));
        http.ProxyHost := Copy(host, 1, RPos(':', host) - 1);
      end else begin
        http.ProxyHost := host;
      end;

      http.ProxyUser := InternetQueryString(INTERNET_OPTION_PROXY_USERNAME);
      http.ProxyPass := InternetQueryString(INTERNET_OPTION_PROXY_PASSWORD);
    end;
  end {else if GetLastError=ERROR_INSUFFICIENT_BUFFER then
   showmessage('Not enough buf');}

end;

function GetLocationHeader(headers: TStringList): string;
var i: integer;
begin
  for I := 0 to headers.Count - 1 do
    if Pos('LOCATION:', uppercase(headers[i])) = 1 then begin
      result := trim(copy(Headers[i], length('LOCATION:') + 1, length(headers[i])));
      exit;
    end;
  result := '';
end;

{Connect the given TFTPSend to the server that the resource specified with
 the given URL resides on}

function ConnectToServer(ftp: TFTPSend; const url: string): boolean;
var prot, user, pass, host, port, path, para: string;
begin
  ParseURL(url, prot, user, pass, host, port, path, para);

  if (Ftp.UserName <> user) or (Ftp.Password <> pass) or (Ftp.TargetHost <> host) or (Ftp.targetport <> port) then begin
     //need to connect/reconnect

    Ftp.Logout;

    Ftp.UserName := user;
    Ftp.Password := pass;
    Ftp.TargetHost := host;
    Ftp.TargetPort := port;
    try
      if not Ftp.Login then begin
        ftp.TargetHost := ''; //so that we'll know we need to reconnect in future
        result := false;
        exit;
      end;
    except on e: exception do begin
        OutputDebugString(PChar(e.Message));
        FTP.TargetHost := ''; //Reconnect next time
        result := false;
        exit;
      end;
    end;
  end;
  result := true; //success
end;

function TDownloadEngine.GetWebFileSize(const url: string; out size: longword): boolean;
var protocol, user, password, host, port, path, para: string;
begin
  ParseURL(url, protocol, user, password, host, port, path, para);

  protocol := uppercase(protocol);

  if protocol = 'HTTP' then
    result := GetHTTPSize(url, size)
  else if protocol = 'FTP' then
    result := GetFTPSize(url, size)
  else result := false;
end;

procedure TDownloadEngine.HTTPSocketProgress(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  if reason = HR_ReadCount then begin
    fDownloadFileWritten := fDownloadFileWritten + cardinal(StrToIntDef(value, 0));

    if Assigned(progress) then

      if not progress(dsDownloading, fDownloadFileWritten, fHttp.DownloadSize, fDownloadTag) then begin
        fhttp.Abort;
      end;
  end;
end;

function TDownloadEngine.PostPage(const url, data: string; out resultbuffer: string): boolean;
var thread: TPostThread;
begin
  fhttp := THTTPSend.Create;
  try
    SetProxy(FHTTP);
    fhttp.Sock.OnStatus := HTTPSocketProgress;
    fHttp.TimeOut := fTimeOut;

    fDownloadFileWritten := 0;

    thread := TPostThread.Create(url, data, fhttp);
    try
      thread.Resume;
      while not thread.operationDone do begin
        handlemessage;
        if terminated then
          thread.abort;
      end;

      if thread.operationsuccess then begin
        result := true;
        setlength(resultbuffer, fHTTP.Document.size);
        fHTTP.Document.Read(resultBuffer[1], length(resultbuffer));
      end else begin
        result := false;
        fLastError := 'HTTP error code ' + inttostr(fHttp.ResultCode);
      end;
    finally
      thread.free;
    end;

  finally
    fhttp.free;
  end;
end;

function TDownloadEngine.DownloadWebFileToStream(const url: string; stream: TStream;
  tag: TObject): boolean;
var protocol, user, password, host, port, path, para: string;
begin
  ParseURL(url, protocol, user, password, host, port, path, para);

  protocol := uppercase(protocol);
  if protocol = 'HTTP' then
    result := DownloadHTTPFileToStream(url, stream, tag)
  else
    if protocol = 'FTP' then
      result := DownloadFTPFileToStream(url, stream, tag)
    else
      result := false; //Bad protocol!

end;

procedure TDownloadEngine.FTPSocketProgress(Sender: TObject;
  Reason: THookSocketReason; const Value: string);
begin
  if reason = HR_ReadCount then begin
    fDownloadFileWritten := fDownloadFileWritten + cardinal(StrToIntDef(value, 0));

    if Assigned(progress) then

      if not progress(dsDownloading, fDownloadFileWritten, 0, fDownloadTag) then begin
        fftp.Abort;
      end;
  end;
end;

destructor TDownloadEngine.Destroy;
begin
  fFTP.free;
  inherited;
end;

function TDownloadEngine.DownloadFTPFileToStream(const url: string;
  stream: TStream; tag: TObject): boolean;
var thread: TFTPGetThread;
begin
  fFtp.DSock.OnStatus := FTPSocketProgress;
  fFtp.Timeout := fTimeOut;

  if assigned(progress) then
    progress(dsStartingDownload, 0, 0, tag);

  fDownloadFileWritten := 0;
  fDownloadTag := tag;

  thread := TFTPGetThread.Create(url, fFtp);
  try
    thread.Resume;
    while not thread.operationDone do begin
      handlemessage;
      if terminated then
        thread.abort;
    end;

    if thread.operationsuccess then begin
      stream.CopyFrom(fFtp.DataStream, 0);
      result := true;
      if Assigned(progress) then begin
        progress(dsDone, fDownloadFileWritten, 0, tag);
      end;
    end else begin
      result := false;
      fLastError := 'FTP error code ' + inttostr(fFtp.ResultCode);
      if Assigned(progress) then
        progress(dsError, fDownloadFileWritten, 0, tag);
    end;
  finally
    thread.free;
  end;
end;


function TDownloadEngine.DownloadHTTPFileToStream(const url: string;
  stream: TStream; tag: TObject): boolean;
var thread: TGetThread;
begin
  fhttp := THTTPSend.Create;
  try
    SetProxy(FHTTP);
    fhttp.Sock.OnStatus := HTTPSocketProgress;
    fHttp.TimeOut := fTimeOut;

    if assigned(progress) then
      progress(dsStartingDownload, 0, 0, tag);

    fDownloadFileWritten := 0;
    fDownloadTag := tag;

    thread := TGetThread.Create(url, fhttp);
    try
      thread.Resume;
      while not thread.operationDone do begin
        handlemessage;
        if terminated then
          thread.abort;
      end;

      if thread.operationsuccess then begin
        stream.CopyFrom(fhttp.Document, 0);
        result := true;
        if Assigned(progress) then begin
          progress(dsDone, fDownloadFileWritten, 0, tag);
        end;
      end else begin
        result := false;
        fLastError := 'HTTP error code ' + inttostr(fHttp.ResultCode);
        if Assigned(progress) then
          progress(dsError, fDownloadFileWritten, 0, tag);
      end;
    finally
      thread.free;
    end;

  finally
    fhttp.free;
  end;
end;

constructor TDownloadEngine.Create;
begin
  fFTP := TFTPSend.Create;
  fFTP.PassiveMode := false;
end;

function TDownloadEngine.GetFTPSize(const url: string; out size: longword): boolean;
var thread: TFTPSizeThread;
begin
  fFtp.Timeout := fTimeOut;
  thread := TFTPSizeThread.Create(url, fFtp);
  thread.Resume;
  while not thread.operationDone do begin
    handlemessage;
    if terminated then
      thread.abort;
  end;

  if thread.operationsuccess then begin
    size := thread.size;
    result := true;
  end else begin
    result := false;
  end;
end;

function TDownloadEngine.GetHTTPSize(const url: string; out size: longword): boolean;
var thread: THeadThread;
begin
  fhttp := THTTPSend.Create;
  try
    SetProxy(FHTTP);
    fhttp.Timeout := fTimeOut;
    thread := THeadThread.Create(url, fhttp);
    thread.Resume;
    while not thread.operationDone do begin
      handlemessage;
      if terminated then
        thread.abort;
    end;

    if thread.operationsuccess then begin
      size := thread.size;
      result := true;
    end else begin
      result := false;
    end;
  finally
    fhttp.free;
  end;
end;


procedure TFTPGetThread.Abort;
begin
  fftp.Abort;
end;

constructor TFTPGetThread.create(const url: string; ftp: TFTPSend);
begin
  inherited create(true);
  fFtp := ftp;
  fUrl := url;
end;

procedure TFTPGetThread.Execute;
var prot, user, pass, host, port, path, para: string;
begin
  try
    try
      fOperationSuccess := false;

      if ConnectToServer(fFtp, fUrl) then begin
        fFtp.DirectFile := false;

        ParseURL(fUrl, prot, user, pass, host, port, path, para);
        fOperationSuccess := fFtp.RetrieveFile(path, false);
      end;
    except on e: exception do
        outputdebugstring(pchar(e.Message));
    end;
  finally
    foperationdone := true;
    PostMessage(0, WM_USER, 0, 0); //wake up the message loop
  end;
end;

{ TGetThread }

procedure TGetThread.Abort;
begin
  fhttp.Abort;
end;

constructor TGetThread.create(const url: string; http: THTTPSend);
begin
  inherited create(true);
  fUrl := url;
  fHttp := http;
  foperationdone := false;
  foperationsuccess := false;
end;

procedure TGetThread.Execute;
var requestSuccess: boolean;
label startAgain;
begin
  try
    foperationsuccess := false;
    startAgain:
  //So that we don't retransmit responses from the server with redirects
    fhttp.Headers.Clear;
    requestSuccess := fhttp.HTTPMethod('GET', furl);

    if requestSuccess then begin
    { The operation completed, but we might still have got an error reply
      from the server }

      if (fhttp.resultcode >= 300) and (fhttp.resultcode < 400) then begin
    //Redirection request
        fURL := GetLocationHeader(fhttp.headers);
        if fURL <> '' then
          goto startAgain;
      //Redirect with no location field! Fall through..
      end;

    //Successful download iff we got a 200 success code.
      foperationsuccess := (fhttp.resultcode >= 200) and (fhttp.resultCode < 300);
    end;
  finally
    foperationdone := true;
    PostMessage(0, WM_USER, 0, 0); //wake up the message loop
  end;
end;

{ THeadThread }

procedure THeadThread.Abort;
begin
  fhttp.abort;
end;

constructor THeadThread.create(const url: string; http: THTTPSend);
begin
  inherited create(true);
  furl := url;
  fhttp := http;
  foperationdone := false;
  foperationsuccess := false;
end;

procedure THeadThread.Execute;
label startAgain;
begin
  try
 //So that we don't retransmit responses from the server with redirects
    startAgain:
    fhttp.Headers.Clear;
    foperationSuccess := fhttp.HTTPMethod('HEAD', furl);

    if foperationsuccess then begin
    { The operation completed, but we might still have got an error reply
      from the server }

      if (fhttp.resultcode >= 300) and (fhttp.resultcode < 400) then begin
    //Redirection request
        fURL := GetLocationHeader(fhttp.headers);
        if fURL <> '' then
          goto startAgain;
      //Redirect with no location field! Fall through..
      end;

      if not ((fhttp.resultcode >= 200) and (fhttp.resultCode < 300)) then
        foperationsuccess := false;
      fsize := fhttp.DownloadSize;
    end else
      fsize := 0;

  finally
    foperationdone := true;
    PostMessage(0, WM_USER, 0, 0); //wake up the message loop
  end;
end;

{ TFTPSizeThread }

procedure TFTPSizeThread.Abort;
begin
  fftp.abort;
end;

constructor TFTPSizeThread.create(const url: string; ftp: TFTPSend);
begin
  inherited create(true);
  fUrl := url;
  fFtp := ftp;
end;

procedure TFTPSizeThread.Execute;
var prot, user, pass, host, port, path, para: string;
begin
  try
    foperationsuccess := false;

    if ConnectToServer(fFtp, fUrl) then begin
      ParseURL(fUrl, prot, user, pass, host, port, path, para);
      fsize := fFtp.FileSize(path);
      foperationsuccess := true;
    end;
  finally
    foperationdone := true;
    PostMessage(0, WM_USER, 0, 0); //wake up the message loop
  end;
end;

{ TPostThread }

procedure TPostThread.Abort;
begin
  fhttp.abort;
end;

constructor TPostThread.create(const url, data: string; http: THTTPSend);
begin
  inherited create(true);
  furl := url;
  fdata := data;
  fHttp := http;
end;

procedure TPostThread.Execute;
begin
  try
    fHTTP.Document.Clear;
    fHTTP.Document.Write(fdata[1], length(fdata));
    fHTTP.MimeType := 'Application/octet-stream';
    foperationsuccess := fHTTP.HTTPMethod('POST', fURL);
  finally
    foperationdone := true;
    PostMessage(0, WM_USER, 0, 0); //wake up the message loop
  end;
end;

end.

