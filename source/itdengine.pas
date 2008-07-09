unit itdengine;


{ ITDEngine.pas - The engine that users of the DLL interact with, constructs the
                  GUI and calls the download engine to do the downloading

  By Nicholas Sherlock - http://www.sherlocksoftware.org }


interface

uses sysutils, windows, Classes, contnrs, liteui, downloadengine,
  itdstrings;

const
  ITDERR_SUCCESS = 0;
  ITDERR_USERCANCEL = 1;
  ITDERR_ERROR = 3;

  itdversion = '0.3 alpha';

  //GUI consts
  second_column = 140;
  vert_spacing =3;
  box_width = 400;

var
  resultbuffer:string; //for returning huge strings

type
  EErrorcode = class(exception)
  public
    errcode: integer;
    constructor Create(code: Integer); reintroduce;
  end;

  TDLFile = class
  public
    urls: array of string;
    filename: string;
    size: cardinal;
    sizeunknown: boolean; //if, while downloading, we find that the predicted size is incorrect
    procedure addMirror(const url: string);
    constructor create(const url: string; const filename: string);
    procedure querysize(wine: TDownloadEngine);
  end;

  TUI = class
  private
    fDetailsMode: boolean;
    fparent: hwnd;

    fStrings: TITDStrings;

    btnDetails: TLiteButton;

    lblTotalProgress, valTotalProgress: TLiteLabel;

    pnlContainer: TLitePanel;

    barCurrent, barTotal: TLiteProgress;

    lblFile, lblSpeed, lblStatus, lblElapsedTime, lblRemainingTime, lblCurrent, 
      valFile, valSpeed, valStatus, valElapsedTime, valRemainingTime, valCurrent: tlitelabel;

    procedure DetailsClick(sender: TObject);
    procedure SetStatus(const value: string);
    procedure SetFilename(const value: string);
    procedure SetDetailsMode(const Value: boolean);
  public
    property DetailedMode: boolean read fDetailsMode write SetDetailsMode;
    property Status: string write setstatus;
    property Filename: string write setfilename;
    constructor Create(parent: hwnd; strings: TITDStrings);
    destructor Destroy; override;
  end;

  TITDEngine = class
  private
    fWE: TDownloadEngine;

    fTotalStartTime, fFileStartTime: TDateTime;
    fTotalBytesWritten: cardinal; //Total bytes of all completely downloaded files so far

    fFiles: tobjectlist;

    fTotalSizeUnknown: boolean;

    fLastProgress: TDateTime;

    fdownloaddelay: integer;
    fcancel: boolean;
    fsmooth, fdebugmessages: boolean;

    options: tstringlist;
    fuis: TObjectList;
    fui: TUI; //currently selected UI
    fstrings: TITDStrings; //localization strings
    function getfile(index: integer): TDLFile;
    function downloadlist(files: tobjectlist): integer;
    function shouldcancel: Boolean;
    function getUI: TUI;
    procedure setUI(value: TUI);
    function DownloadProgress(stage: TDownloadStage;
      downbytes, bytestotal: longword; tag: TObject): boolean;

    function CalcTotalFilesSize: cardinal;

  public
    function UIByHandle(handle: hwnd): TUI;

    property UI: TUI read getUI write setUI; //currently selected UI
    property Strings: TITDStrings read fstrings write fstrings;

    function Count: integer;

    function PostPage(const url, data:string; out resultbuffer:string):boolean;

    procedure Cancel;
    procedure CreateUI(hosthwnd: hwnd);
    procedure AddFile(const url, filename: string; size: integer = 0);
    procedure AddMirror(const url, filename: string);
    procedure ClearFiles;
    procedure SetOption(const option, value: string);
    function GetOption(const option: string): string;
    function DownloadFile(const url, filename: string): integer;
    function DownloadFiles(surface: hwnd): integer;

    property Files[index: integer]: TDLFile read getfile;
    property DownloadDelay: integer read fdownloaddelay write fdownloaddelay;
    property DebugMessages: boolean read fDebugMessages write fDebugMessages;

    constructor Create;
    destructor Destroy; override;
  end;

var engine: TITDEngine;

implementation

uses formatting;

constructor eerrorcode.create(code: Integer);
begin
  inherited create('');
  errcode := code;
end;

procedure TUI.SetDetailsMode(const Value: boolean);
begin
  fDetailsMode := Value;

  barCurrent.visible := value;
  lblFile.visible := value;
  lblSpeed.visible := value;
  lblStatus.visible := value;
  lblElapsedTime.visible := value;
  lblRemainingTime.visible := value;
  lblCurrent.visible := value;

  valFile.visible := value;
  valSpeed.visible := value;
  valStatus.visible := value;
  valElapsedTime.visible := value;
  valRemainingTime.visible := value;
  valCurrent.visible := value;

  barTotal.visible := true;

  if DetailedMode then begin
    btnDetails.Caption := fStrings[IS_HideDetails];
    btnDetails.Top := lblRemainingTime.top+lblRemainingTime.height+8;

    lblTotalProgress.Caption:=fStrings[IS_TotalProgress];  //WRONG
  end else begin
    btnDetails.Caption := fStrings[IS_ShowDetails];
    btnDetails.Top := barTotal.top+ barTotal.height+8;

    lblTotalProgress.Caption:=valStatus.caption;
  end;

  pnlContainer.height := btnDetails.Top+btnDetails.height+8;
end;

procedure tui.setfilename(const value: string);
begin
  valfile.caption := value;
end;

procedure tui.setstatus(const value: string);
begin
  valstatus.caption := value;
  if not DetailedMode then
    lblTotalProgress.Caption := value;
end;

constructor tui.create(parent: hwnd; strings: TITDStrings);
begin
  liteui_init;
  fparent := parent;
  fstrings := strings;

  pnlContainer := TLitePanel.create(parent);
  pnlContainer.ParentFont:=false;
  pnlContainer.font.name := 'MS Shell Dlg';
  pnlContainer.font.size := 14;
  pnlContainer.width := box_width;

  //The parent of all controls from now on is the main panel
  parent := pnlcontainer.handle;

  lblTotalProgress := TLiteLabel.create(parent);
  lblTotalProgress.AutoSize := true;
  pnlContainer.addChild(lblTotalProgress);

  valTotalProgress := TLiteLabel.create(parent);
  valTotalProgress.align := laRight;
  valTotalProgress.AutoSize := false;
  valTotalProgress.Width := 150;
  valTotalProgress.Left := box_width - valTotalProgress.Width;
  pnlContainer.addChild(valTotalProgress);

  barTotal := TLiteProgress.create(parent);
  barTotal.width := box_width;
  barTotal.Top := lblTotalProgress.Top + lblTotalProgress.Height + vert_spacing;
  pnlContainer.addchild(barTotal);

  lblCurrent := TLiteLabel.create(parent);
  lblCurrent.caption := Strings[IS_CurrentFile];
  lblCurrent.top := barTotal.top + barTotal.height + 8;
  pnlContainer.addchild(lblCurrent);

  valCurrent := TLiteLabel.create(parent);
  valCurrent.width:=150;
  valCurrent.align := laRight;
  valCurrent.AutoSize:=false;
  valCurrent.caption := '';
  valCurrent.Top := lblCurrent.top;
  valCurrent.left := box_width - valcurrent.width;
  pnlContainer.addchild(valCurrent);

  barCurrent := TLiteProgress.create(parent);
  barCurrent.width := box_width;
  barCurrent.Top := lblCurrent.top+ lblCurrent.height+ vert_spacing;
  pnlContainer.addchild(barCurrent);

  lblFile := tlitelabel.create(parent);
  lblFile.autosize := true;
  lblFile.caption := Strings[IS_File];
  lblFile.top := barCurrent.top+barCurrent.height+vert_spacing*3;
  pnlContainer.addchild(lblFile);

  valFile := TLiteLabel.create(parent);
  valFile.caption := '';
  valFile.left := second_column;
  valFile.top:=lblFile.top;
  pnlContainer.addchild(valFile);

  lblSpeed := TLiteLabel.create(parent);
  lblSpeed.caption := Strings[IS_Speed];
  lblSpeed.top := lblFile.top + lblFile.height + vert_spacing;
  pnlContainer.addchild(lblSpeed);

  valSpeed := TLiteLabel.create(parent);
  valSpeed.caption := '';
  valSpeed.left := second_column;
  valSpeed.top := lblSpeed.top;
  pnlContainer.addchild(valSpeed);

  lblStatus := TLiteLabel.create(parent);
  lblStatus.caption := strings[IS_Status];
  lblStatus.top := valSpeed.top + valSpeed.height + vert_spacing;
  pnlContainer.addchild(lblStatus);

  valStatus := TLiteLabel.create(parent);
  valStatus.caption := '';
  valStatus.left := second_column;
  valStatus.top := lblStatus.top;
  pnlContainer.addchild(valstatus);

  lblelapsedtime := TLiteLabel.create(parent);
  lblelapsedtime.caption := Strings[IS_ElapsedTime];
  lblelapsedtime.top := lblStatus.top+ lblStatus.height + vert_spacing;
  pnlContainer.addchild(lblElapsedTime);

  valElapsedtime := TLiteLabel.create(parent);
  valElapsedTime.caption := '';
  valElapsedTime.left := second_column;
  valelapsedtime.top := lblElapsedTime.top;
  pnlContainer.addchild(valElapsedTime);

  lblRemainingTime := TLiteLabel.create(parent);
  lblRemainingtime.caption := Strings[IS_RemainingTime];
  lblremainingtime.top := lblElapsedTime.top+ lblElapsedTime.height + vert_spacing;
  pnlContainer.addchild(lblRemainingTime);

  valRemainingtime := TLiteLabel.create(parent);
  valRemainingtime.caption := '';
  valremainingtime.left := second_column;
  valRemainingTime.Top := lblRemainingTime.top;
  pnlContainer.addchild(valRemainingTime);

  btnDetails := TLiteButton.create(parent);
  btnDetails.Left := box_width - btnDetails.Width;
  btnDetails.OnClick := DetailsClick;
  pnlContainer.addchild(btnDetails);

  DetailedMode := false;

  processmessages;
end;

destructor tui.destroy;
begin
  pnlContainer.free;

  inherited;
end;


procedure TUI.DetailsClick(sender: TObject);
begin
  DetailedMode := not DetailedMode;
end;

procedure TDLFile.addMirror(const url: string);
begin
  setlength(urls, length(urls) + 1);
  urls[high(urls)] := url;
end;

constructor TDLFile.create(const url, filename: string);
begin
  setlength(urls, 1);
  urls[0] := url;
  self.filename := filename;
end;

procedure tdlfile.querysize(wine: TDownloadEngine);
var swap: string;
  i: integer;
begin
  for i := 0 to high(urls) do
    if wine.getwebfilesize(urls[i], size) then begin
      { Since this mirror is the one which is the most likely
        to have a successful download, move it to the front of the mirror
        list }
      if i <> 0 then begin
        swap := urls[0];
        urls[0] := urls[i];
        urls[i] := swap;
      end;
      exit; //Success
    end;

  //Failed to resolve size
  size := 0;
end;

constructor TITDEngine.create;
begin
  inherited;
  fWE := TDownloadEngine.Create;

  fWE.TimeOut := 10000;
  fWE.Agent := 'InnoTools_Downloader';
  fWE.progress := DownloadProgress;

  fstrings := TITDStrings.create;
  fFiles := TObjectList.Create;
  fuis := tobjectlist.create;
  options := tstringlist.create;
  fcancel := false;
  fdownloaddelay := 0;
  fsmooth := false;
end;

procedure TITDEngine.AddMirror(const url, filename: string);
var i: integer;
begin
  for i := 0 to ffiles.Count - 1 do begin

    if AnsiCompareText(TDlFile(ffiles[i]).filename, filename) = 0 then begin
      TDlFile(ffiles[i]).addMirror(url);
    end;
  end;

end;

function TITDEngine.calcTotalFilesSize: cardinal;
var t1: integer;
begin
  fTotalSizeUnknown := false;
  result := fTotalBytesWritten; //Since these are removed from ffiles list
  for t1 := 0 to ffiles.count - 1 do begin
    result := result + TDLFile(ffiles[t1]).size;

    if TDLFile(files[t1]).size = 0 then
      fTotalSizeUnknown := true;
  end;
end;


destructor TITDEngine.destroy;
begin
  fwe.free;
  fuis.free;
  fFiles.free;
  options.free;
  fstrings.free;
  inherited;
end;

function TITDEngine.UIByHandle(handle: hwnd): TUI;
var t1: integer;
begin
  for t1 := 0 to fuis.count - 1 do
    if tui(fuis[t1]).fparent = handle then begin
      result := tui(fuis[t1]);
      exit;
    end;
  result := TUI.create(handle, fstrings);
  fuis.Add(result);
end;

function TITDEngine.getUI: TUI;
begin
  result := fui;
end;

function TITDEngine.PostPage(const url, data: string; out resultbuffer:string): boolean;
begin
 result:=fWE.PostPage(url,data, resultbuffer);
end;

procedure TITDEngine.setUI(value: TUI);
begin
  fui := value;
end;

function TITDEngine.shouldcancel: Boolean;
begin
  result := terminated or fcancel;
end;

function TITDEngine.getfile(index: integer): TDLFile;
begin
  result := TDLFile(fFiles[index]);
end;

procedure TITDEngine.Cancel;
begin
  fcancel := true;
end;

function TITDEngine.Count: integer;
begin
  result := fFiles.count;
end;

procedure TITDEngine.AddFile(const url, filename: string; size: integer = 0);
var f: TDLFile;
begin
  f := TDLFile.Create(url, filename);
  f.size := size;
  ffiles.Add(f);
end;

function TITDEngine.GetOption(const option: string): string;
begin
  if AnsiCompareText(option, 'ITD_Version') = 0 then
    result := itdversion else
    if AnsiCompareText(option, 'Debug_DownloadDelay') = 0 then
      result := inttostr(fdownloaddelay) else
      if AnsiCompareText(option, 'ITD_NoCache') = 0 then begin
        result := '0'; //Depreceated
      end else
        if AnsiCompareText(option, 'UI_SmoothBars') = 0 then begin
          if fsmooth then result := '1' else result := '0';
        end else
          if AnsiCompareText(option, 'Debug_Messages') = 0 then begin
            if fdebugmessages then result := '1' else result := '0'
          end else
            result := '';
end;

procedure TITDEngine.SetOption(const option, value: string);
var t1: integer;
begin
  if AnsiCompareText(option, 'Debug_DownloadDelay') = 0 then begin
    fdownloaddelay := strtoint(value);
  end else
    if AnsiCompareText(option, 'ITD_TimeOut') = 0 then begin
      fWE.timeout := strtoint(value);
    end else
      if AnsiCompareText(option, 'ITD_NoCache') = 0 then begin
        // fwe.nocache := (value = '1'); Depreceated
      end else
        if AnsiCompareText(option, 'UI_SmoothBars') = 0 then begin
          fsmooth := value = '1';
          for t1 := 0 to fuis.count - 1 do begin
            tui(fuis[t1]).barCurrent.smooth := fsmooth;
            tui(fuis[t1]).barTotal.smooth := fsmooth;
          end;
        end else
          if AnsiCompareText(option, 'Debug_Messages') = 0 then
            fdebugmessages := (value = '1');
end;

procedure TITDEngine.CreateUI(hosthwnd: hwnd);
var ui: TUI;
begin
  ui := TUI.create(hosthwnd, strings);
  ui.barCurrent.Smooth := fsmooth;
  ui.barTotal.Smooth := fsmooth;
  fuis.Add(ui);
end;

procedure TITDEngine.ClearFiles;
begin
  fFiles.Clear;
end;

function TITDEngine.DownloadFile(const url, filename: string): integer;
var f: TDLFile;
  list: tobjectlist;
begin
  f := TDLFile.Create(url, filename);
  list := TObjectList.Create;
  try
    list.Add(f);
    try
      result := downloadlist(list);
    except //don't allow our exceptions to percolate to the host process!
      on e: eerrorcode do
        Result := e.errcode;
    else
      result := ITDERR_ERROR; //unexpected and unknown error
    end;
  finally
    list.free;
  end;
end;

function TITDEngine.DownloadFiles(surface: hwnd): integer;
begin
  try
    ui := UIByHandle(surface); //set the UI to be used
    result := downloadlist(fFiles);
  except //don't allow our exceptions to percolate to the host process!
    on e: eerrorcode do
      Result := e.errcode;
  else
    result := ITDERR_ERROR; //unexpected and unknown error
  end;
end;


function max(i1, i2: double): double;
begin
  if i1 > i2 then
    result := i1 else
    result := i2;
end;

{Called by TDownloadEngine to report download progress of the file we gave it}

function TITDEngine.DownloadProgress(
  stage: TDownloadStage; downbytes, bytestotal: longword; tag: TObject): boolean;
var f: TDLFile;
  totalFileBytes: Cardinal;
  totalDownloaded: cardinal;

  procedure updateTotalBar;
  begin
    ui.barTotal.max := totalFileBytes;

    ui.barTotal.Marquee := fTotalSizeUnknown;

    if fTotalSizeUnknown then begin
      ui.valTotalProgress.caption := fileprogresstostr(TotalDownloaded, 0, true, strings);
    end else begin
      ui.valTotalProgress.caption := fileprogresstostr(TotalDownloaded, totalFileBytes, False, strings);
      ui.barTotal.position := TotalDownloaded;
      ui.barTotal.update;
    end;
  end;

  procedure UpdateCurrentBar;
  begin
    ui.barCurrent.Marquee := downbytes > f.size;
    if downbytes > f.size then begin
    //essentially, we don't know the proper size of the file, so let's not let on to the user.. :)
      ui.valcurrent.caption := fileprogresstostr(downbytes, 0, true, strings);
    end else begin
      ui.valcurrent.caption := fileprogresstostr(downbytes, f.size, false, strings);
      ui.barcurrent.position := downbytes;
      ui.barCurrent.Update;
    end;
  end;


begin
  if shouldcancel then begin
    result := false; //stop downloading!
    exit;
  end else
    result := true;

  if not assigned(UI) then exit;

  f := TDLFile(tag);

  if bytestotal <> 0 then begin
    f.size := bytestotal;
    ui.barcurrent.max := bytestotal;
  end;

   //Total downloaded is the total of completed files plus the progress on this file
  totalDownloaded := fTotalBytesWritten + downBytes;

   //Total bytes of files we are to download
  totalFileBytes := CalcTotalFilesSize;

  case stage of
    dsDone: begin
        f.size := downbytes;
        totalFileBytes := CalcTotalFilesSize; //just in case we have new information
        updateCurrentBar;
        updateTotalBar;
      end;
    dsStartingDownload: begin
        ui.status := fStrings[IS_StartingDownload];
        ui.filename := ExtractFileName(f.filename);
        ui.barcurrent.max := f.size;

        updateCurrentBar;

        ui.barCurrent.Marquee := true;
        ui.lblTotalProgress.caption := Format(Strings[IS_DownloadingSimple], [extractfilename(f.filename)]);

        processmessages;
      end;
    dsDownloading: begin

        if fdownloaddelay <> 0 then
          sleep(fdownloaddelay);

        if now < fLastProgress + 0.5 / SecsPerDay then
          exit;
        fLastProgress := now;

        if ui.DetailedMode then begin
          ui.lblTotalProgress.caption := Strings[IS_TotalProgress];
          ui.status := fStrings[IS_Downloading];
        end else
          ui.lblTotalProgress.caption := Format(Strings[IS_DownloadingSimple], [extractfilename(f.filename)]);

        if now - fFileStartTime > 0 then
          ui.valSpeed.caption := fileratetostr(round(downbytes / ((now - fFileStartTime) * secsperday)), strings);
        ui.valElapsedTime.caption := shorttimetostr(now - fTotalStartTime, strings);
        if not fTotalSizeUnknown then
          ui.valRemainingTime.caption := shorttimetostr(max((totalFileBytes / TotalDownloaded) * (now - fTotalStartTime) - (now - fTotalStartTime), 0), strings) else
          ui.valRemainingTime.caption := Strings[IS_Unknown];

        updateCurrentBar;

        updateTotalBar;

        processmessages;
      end;
  end;
end;

function TITDEngine.downloadlist(files: tobjectlist): integer;
var t1, i: integer;
  filestream: TFileStream;
  f: tdlfile;
  url: string;
  success: boolean;
begin
  fcancel := false;

  processmessages;
  if assigned(ui) then begin
    ui.status := fStrings[IS_GettingFileInformation];
    ui.lblCurrent.Visible := false;
    ui.barCurrent.visible := false;
    ui.valCurrent.Visible := false;
    ui.barTotal.visible := true;
    ui.barTotal.Marquee := true;
    ui.valElapsedTime.Caption := '';
  end;
  processmessages;

  try
    for t1 := 0 to files.count - 1 do begin
      if fdownloadDelay > 0 then begin
      { If we want a download delay, simulate a delay in connecting to the server
        to get file size}
        for i := 1 to 3 do begin
          processmessages;
          sleep(100);
        end;
      end;
      if TDLFile(files[t1]).size = 0 then
        TDLFile(files[t1]).querysize(fWE);

      processmessages;
      if ShouldCancel then
        raise EErrorcode.Create(ITDERR_USERCANCEL);
    end;

    fTotalStartTime := now;
    fTotalBytesWritten := 0;

    while files.Count > 0 do begin //Loop over all files to be downloaded
      f := tdlfile(files[0]);

      fFileStartTime := now;

      filestream := TFileStream.Create(f.filename, fmCreate or fmShareDenyNone);
      try
        success := false;
        for url in f.urls do begin
          if fWE.DownloadWebFileToStream(url, filestream, f) then begin
            success := true;
            break;
          end;
        end;
        if not success then begin //couldn't get this file from any mirror
          if terminated then begin
            raise EErrorCode.create(ITDERR_USERCANCEL);
          end;

          if fdebugmessages then begin
            showmessage('Error: ' + fwe.LastError);
          end;
          raise EErrorcode.create(ITDERR_ERROR);
        end;

        fTotalBytesWritten := fTotalBytesWritten + filestream.Size;
      finally
        filestream.free;
      end;
      files.remove(f); //we have finished this file!
    end;

    result := ITDERR_SUCCESS; //done without incident!
    if assigned(ui) then begin
      ui.status := fStrings[IS_DownloadComplete];
      ui.filename := '';

      ui.valSpeed.caption := '';
      ui.valRemainingTime.caption := '';
    end;

  except
    //Download failed!
    if assigned(ui) then begin
      ui.status := fStrings[IS_DownloadFailed];
      ui.filename := '';
      ui.valCurrent.caption := '';
      ui.valSpeed.caption := '';
      ui.valRemainingTime.caption := '';
    end;

    ui.barCurrent.Marquee:=false;
    ui.barTotal.Marquee:=false;

    ui.barCurrent.Position:=0;

    ui.barCurrent.Visible := false;
    ui.lblCurrent.visible := false;
    ui.valCurrent.Visible := false;

    ui.valTotalProgress.visible := false;
    raise;
  end;
end;

function itd_downloadfile(url: PChar; destfilename: PChar): integer; stdcall;
begin
  try
    result := engine.downloadfile(url, destfilename);
  except
    on e: EErrorCode do
      result := e.errcode;
  else
    result := ITDERR_ERROR;
  end;
end;

function itd_downloadfiles(surface: hwnd): integer; stdcall;
begin
  try
    result := engine.downloadfiles(surface);
  except
    on e: EErrorCode do
      result := e.errcode;
  else
    result := ITDERR_ERROR;
  end;
end;

procedure itd_clearfiles; stdcall;
begin
  engine.clearfiles;
end;

procedure itd_addmirror(url, destfilename: PChar); stdcall;
begin
  engine.AddMirror(url, destfilename);
end;

procedure itd_addfile(url, destfilename: PChar); stdcall;
begin
  engine.AddFile(url, destfilename);
end;

procedure itd_addfilesize(url: PChar; destfilename: PChar; size: integer); stdcall;
begin
  engine.AddFile(url, destfilename, size);
end;

procedure itd_initui(HostHwnd: dword); stdcall;
begin
  engine.createui(hosthwnd);
end;

procedure itd_cancel; stdcall;
begin
  engine.cancel;
end;

function itd_filecount: integer; stdcall;
begin
  result := engine.count;
end;

procedure itd_setoption(option, value: PChar); stdcall;
begin
  try
    engine.setoption(option, value);
  except
  end;
end;

function itd_getoption(option: PChar; buffer: PChar; length: integer): integer; stdcall;
var s: string;
begin
  s := engine.GetOption(option);
  StrLCopy(buffer, pchar(s), length - 1);
  result := StrLen(buffer);
end;

function itd_loadstrings(filename: PChar): boolean; stdcall;
begin
  try
    engine.Strings.loaddefaults;
    engine.Strings.appendfromfile(filename);
    result := true;
  except
    result := false;
  end;
end;

procedure itd_setstring(index: integer; value: pchar); stdcall;
begin
  engine.Strings[index] := value;
end;

function itd_getstring(index: integer): boolean; stdcall;
begin
 resultbuffer := engine.Strings[index];
 result:=length(resultbuffer)>0;
end;

function itd_postpage(url:pchar; buffer:pchar; length:integer):boolean; stdcall;
var data:string;
begin
  try
    setlength(data,length);
    Move(buffer^, data[1], length);
    result:=engine.PostPage(url,data, resultbuffer);
  except
    result := false;
  end;
end;

function min(a,b:integer):integer;
begin
   if a<b then
   result:=a
   else
   result:=b;
end;


procedure itd_getresultstring(buffer:pchar; maxlen:integer); stdcall;
begin
  move(resultbuffer[1], buffer^, min(length(resultbuffer),maxlen));
end;

function itd_getresultlen:integer;
begin
  result:=length(resultbuffer);
end;

exports itd_downloadfile, itd_addfile, itd_addfilesize, itd_clearfiles, itd_downloadfiles, itd_initui,
  itd_cancel, itd_setoption, itd_getoption, itd_filecount, itd_setstring, itd_getstring, itd_loadstrings,
  itd_addmirror, itd_postpage, itd_getresultstring, itd_getresultlen;

initialization
  engine := TITDEngine.create;
finalization
  engine.free;
end.

