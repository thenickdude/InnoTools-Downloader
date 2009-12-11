[Files]
#IF DEFINED(UNICODE)
Source: {#emit ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','InstallPath','')}\wide\ITDownload.dll; Flags: dontcopy; DestDir: {tmp}
#ELSE
Source: {#emit ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','InstallPath','')}\ansi\ITDownload.dll; Flags: dontcopy; DestDir: {tmp}
#ENDIF

[Code]
(*
 Inno Tools Downloader DLL
 Copyright (C) Sherlock Software 2008
 Version 0.3.6

 Contact:
  The author, Nicholas Sherlock, at nick@sherlocksoftware.org.
  Comments, questions and suggestions welcome.

 Website:
  http://www.sherlocksoftware.org/

 History:
  0.3.6 - Added callback that allows you to decide whether the install can continue
          when some downloaded files are missing.
          Fixed compatibility with Unicode Inno Setup
          Added Spanish translation by lainz (http://lainzblog.blogspot.com/)
          Added Greek translation by XhmikosR
  0.3.5 - Moved strings used in the updater example to the language file, so that they
          may be more easily translated.
          Added event functions to support the example of integration with InnoTools tray.
          Fixes bugs that could cause the download to hang until
          some mouse movements were received.
          Allows "detailed mode" to be turned on with ITD_SetOption
          Allows the HTTP agent to be set with ITD_SetOption
  0.3.4 - Added Brazilian Portuguese translation by Ronaldo Souza
  0.3.3 - The "Hilbrand Edskes" release :), lots of suggestions and corrections from him, thank you!
          The "Retry" button is now translated.
          The "Current file" progress bar is hidden if only one file is being downloaded.
          The page's title and description are updated in the case of failed downloads.
          Several updates to the translations.
          Several small GUI fixes.
  0.3.2 - Minor tweaks to the English language file and the translation example script (example4.iss)
          Added Dutch translation by Hilbrand Edskes
          Added French translation by Néo
  0.3.1 - Added language file examples, fixed several missing language strings
          Preliminary support for proxy server autodetection
          Allows the size of a file to be queried with ITD_GetFileSize
          Several small bugfixes
  0.3.0 - Properly supports timeouts.
          Fixes bug with time remaining.
          DLL is required again.
          Supports localization through ITD_LoadStrings, ITD_SetString
          Add mirrors for files
          Post HTTP documents
          Quick view and detailed view
  0.2.2 - Fixed empty strings '' in calls and added timeouts
  0.2.1 - Renamed identifiers to avoid name clashes
  0.2.0 - Converted from DLL to pure native code
*)

procedure ITD_Cancel;
  external 'itd_cancel@files:itdownload.dll stdcall';

procedure ITD_ClearFiles;
  external 'itd_clearfiles@files:itdownload.dll stdcall';

function ITD_DownloadFile(url:string; destfilename:String): integer;
  external 'itd_downloadfile@files:itdownload.dll stdcall';

function ITD_GetResultLen: integer;
  external 'itd_getresultlen@files:itdownload.dll stdcall';

procedure ITD_GetResultString(buffer: string; maxlen: integer);
  external 'itd_getresultstring@files:itdownload.dll stdcall';

function ITD_Internal_IsDownloadComplete(filename:string):boolean;
  external 'itd_isdownloadcomplete@files:itdownload.dll stdcall';

procedure ITD_Internal_InitUI(HostHwnd: dword);
  external 'itd_initui@files:itdownload.dll stdcall';

function ITD_Internal_LoadStrings(filename: string): boolean;
  external 'itd_loadstrings@files:itdownload.dll stdcall';

procedure ITD_Internal_SetOption(option, value: string);
  external 'itd_setoption@files:itdownload.dll stdcall';

function ITD_Internal_GetFileSize(url: string; var size: Cardinal): boolean;
  external 'itd_getfilesize@files:itdownload.dll stdcall';

function ITD_Internal_GetString(index: integer): boolean;
  external 'itd_getstring@files:itdownload.dll stdcall';

function ITD_Internal_GetOption(option: string; buffer: string; length: integer): integer;
  external 'itd_getoption@files:itdownload.dll stdcall';

procedure ITD_Internal_SetString(index: integer; value: string);
  external 'itd_setstring@files:itdownload.dll stdcall';

procedure ITD_Internal_AddFile(url: string; destfilename: string);
  external 'itd_addfile@files:itdownload.dll stdcall';

procedure ITD_Internal_AddMirror(url: string; destfilename: string);
  external 'itd_addmirror@files:itdownload.dll stdcall';

procedure ITD_Internal_AddFileSize(url: string; destfilename: string; size: integer);
  external 'itd_addfilesize@files:itdownload.dll stdcall';

function ITD_Internal_DownloadFiles(surface: hwnd): integer;
  external 'itd_downloadfiles@files:itdownload.dll stdcall';

function ITD_FileCount: integer;
  external 'itd_filecount@files:itdownload.dll stdcall';

function ITD_Internal_PostPage(url: String; data:AnsiString; length: integer): boolean;
  external 'itd_postpage@files:itdownload.dll stdcall';

type
  TITD_AllowContinueEvent=function:integer;

const
  ITD_Offer_Continue = 0;
  ITD_Silently_Continue = 1;
  ITD_No_Continue = 2;

  ITDERR_SUCCESS = 0;
  ITDERR_USERCANCEL = 1;
  ITDERR_ERROR = 3;

  {Constants for Language String indexes:}
  ITDS_DownloadFailed = 104;

  ITDS_TitleCaption = 200;
  ITDS_TitleDescription = 201;

  ITDS_MessageFailRetryContinue = 250;
  ITDS_MessageFailRetry = 251;

  ITDS_Retry = 502;

  {Constants for language strings of updater example}
  ITDS_Update_Caption = 600;
  ITDS_Update_Description = 601;
  ITDS_Update_Checking = 602;
  ITDS_Update_NewAvailable = 603;
  ITDS_Update_NoNewAvailable = 604;
  ITDS_Update_WantToCheck = 605;
  ITDS_Update_Failed = 606;
  ITDS_Update_WillLaunch = 607;
  ITDS_Update_WillLaunchWithPath = 608;

  ITD_Event_DownloadPageEntered = 1;
  ITD_Event_DownloadPageLeft = 2;
  ITD_Event_DownloadFailed = 3;

  ITD_Event_FileBegin = 4;
  ITD_Event_FileEnd = 5;

var
  ITD_AllowContinue: boolean;
  ITD_RetryOnBack: boolean;

  ITD_AfterSuccess: procedure(downloadPage: TWizardPage);
  ITD_EventHandler: procedure(event: integer);
  ITD_AllowContinueEvent: TITD_AllowContinueEvent;

function ITD_DownloadFiles():integer;
begin
  result:=ITD_Internal_DownloadFiles(0);
end;

procedure ITD_AddFile(const URL, filename: String);
begin
  ITD_Internal_AddFile(URL, filename);
end;

procedure ITD_AddMirror(const URL, filename: String);
begin
  ITD_Internal_AddMirror(URL, filename);
end;

procedure ITD_AddFileSize(const URL, filename: String; size: integer);
begin
  ITD_Internal_AddFileSize(URL, filename, size);
end;

function ITD_HandleSkipPage(sender: TWizardPage): boolean;
begin
  result := (itd_filecount = 0);
end;

function ITD_IsDownloadComplete(const filename:String):boolean;
begin
  result:=ITD_Internal_IsDownloadComplete(filename);
end;

procedure ITD_SetString(index: integer; value: string);
begin
  itd_internal_setstring(index, value);
end;

function ITD_GetFileSize(const url: String; var size: cardinal): boolean;
begin
  result := itd_internal_getfilesize(url, size);
end;

function ITD_LoadStrings(const filename: string): boolean;
begin
  result := itd_internal_loadstrings(filename);
end;

function ITD_GetString(index: integer): String;
begin
  ITD_Internal_GetString(index);
  SetLength(result, ITD_GetResultLen);
  ITD_GetResultString(result, length(result));
end;

procedure ITD_NowDoDownload(sender: TWizardPage);
var err: integer;
 allowcontinue:boolean;
begin
  wizardform.backbutton.enabled := false;
  wizardform.nextbutton.enabled := false;

  sender.caption := ITD_GetString(ITDS_TitleCaption);
  sender.description := ITD_GetString(ITDS_TitleDescription);

  err := ITD_Internal_DownloadFiles(sender.surface.handle);

  case err of
    ITDERR_SUCCESS: begin
        wizardform.nextbutton.enabled := true;
        wizardform.nextbutton.onclick(nil);

        if itd_aftersuccess <> nil then
          itd_aftersuccess(sender);
      end;
    ITDERR_USERCANCEL: ; //Don't show a message, this happens on setup close and cancel click
  else begin
    //Some unexpected error, like connection interrupted
      wizardform.backbutton.caption := ITD_GetString(ITDS_Retry);
      wizardform.backbutton.enabled := true;
      wizardform.backbutton.show();
      itd_retryonback := true;

      if ITD_EventHandler <> nil then
        ITD_EventHandler(ITD_Event_DownloadFailed);

      if (ITD_AllowContinueEvent <> nil) then begin
        case ITD_AllowContinueEvent() of
          ITD_Offer_Continue: allowcontinue:=true;
          ITD_Silently_Continue:begin
            wizardform.nextbutton.enabled := true;
            wizardform.nextbutton.onclick(nil);

            if itd_aftersuccess <> nil then
              itd_aftersuccess(sender);

            exit;
          end;
          ITD_No_Continue: allowcontinue:=false;
        end;
      end else
        allowcontinue:=itd_allowcontinue;

      if allowcontinue then begin
        wizardform.nextbutton.enabled := true;

        //Download failed, we can retry, continue, or exit
        sender.caption := ITD_GetString(ITDS_DownloadFailed);
        sender.description := ITD_GetString(ITDS_MessageFailRetryContinue);

        MsgBox(ITD_GetString(ITDS_MessageFailRetryContinue), mbError, MB_OK);
      end else begin //Download failed, we must retry or exit setup
        wizardform.nextbutton.enabled := false;

        sender.caption := ITD_GetString(ITDS_DownloadFailed);
        sender.description := ITD_GetString(ITDS_MessageFailRetry);

        MsgBox(ITD_GetString(ITDS_MessageFailRetry), mbError, MB_OK);
      end;
    end;
  end;
end;

procedure ITD_HandleShowPage(sender: TWizardPage);
begin
  wizardform.nextbutton.enabled := false;
  wizardform.backbutton.hide();

  if ITD_EventHandler <> nil then
    ITD_EventHandler(ITD_Event_DownloadPageEntered);

  itd_nowdodownload(sender);
end;

function ITD_HandleBackClick(sender: TWizardpage): boolean;
begin
  result := false;
  if itd_retryonback then begin
    itd_retryonback := false;
    wizardform.backbutton.hide();
    itd_nowdodownload(sender);
  end;
end;

function ITD_HandleNextClick(sender: TWizardpage): boolean;
begin
  if ITD_EventHandler <> nil then
    ITD_EventHandler(ITD_Event_DownloadPageLeft);

  result := true;
end;

procedure ITD_Init;
begin
  //Currently a NOP, don't count on it in future!
end;

function ITD_PostPage(const url, data: String; out response: String): boolean;
begin
  result := ITD_Internal_PostPage(url, data, length(data));

  if result then begin
    setlength(response, ITD_GetResultLen);
    ITD_GetResultString(response, length(response));
  end;
end;

function ITD_DownloadAfter(afterID: integer): TWizardPage;
var itd_downloadPage: TWizardPage;
begin
  itd_downloadpage := CreateCustomPage(afterID, ITD_GetString(ITDS_TitleCaption), ITD_GetString(ITDS_TitleDescription));

  itd_downloadpage.onactivate := @itd_handleshowpage;
  itd_downloadpage.onshouldskippage := @itd_handleskippage;
  itd_downloadpage.onbackbuttonclick := @itd_handlebackclick;
  itd_downloadpage.onnextbuttonclick := @itd_handlenextclick;

  itd_internal_initui(itd_downloadpage.surface.handle);

  result := itd_downloadpage;
end;

procedure ITD_SetOption(const option, value: string);
begin
  //The options which call ITD_SetString are depreciated, use ITD_SetString directly
  if comparetext(option, 'UI_Caption') = 0 then
    ITD_SetString(ITDS_TitleCaption, value)
  else if comparetext(option, 'UI_Description') = 0 then
    ITD_SetString(ITDS_TitleDescription, value)
  else if comparetext(option, 'UI_FailMessage') = 0 then
    ITD_SetString(ITDS_MessageFailRetry, value)
  else if comparetext(option, 'UI_FailOrContinueMessage') = 0 then
    ITD_SetString(ITDS_MessageFailRetryContinue, value)
  else if comparetext(option, 'UI_AllowContinue') = 0 then
    ITD_AllowContinue := (value = '1')
  else
    ITD_Internal_SetOption(option, value);
end;

function ITD_GetOption(const option: AnsiString): AnsiString;
begin
  setlength(result, 500);
  setlength(result, itd_internal_getoption(option, result, length(result)));
end;
