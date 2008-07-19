[Files]
Source: {#emit ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','InstallPath','')}\itdownload.dll; Flags: dontcopy; DestDir: {tmp}

[Code]
(*
 Inno Tools Downloader DLL
 Copyright (C) Sherlock Software 2006
 Version 0.3.1 Alpha

 Contact:
  The author, Nicholas Sherlock, at nick@sherlocksoftware.org.
  Comments, questions and suggestions welcome.

 Website:
  http://www.sherlocksoftware.org

 History:
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

function ITD_DownloadFile(url: PChar; destfilename: PChar): integer;
  external 'itd_downloadfile@files:itdownload.dll stdcall';

function ITD_GetResultLen: integer;
  external 'itd_getresultlen@files:itdownload.dll stdcall';

procedure ITD_GetResultString(buffer:pchar; maxlen:integer);
  external 'itd_getresultstring@files:itdownload.dll stdcall';

procedure ITD_Internal_InitUI(HostHwnd: dword);
  external 'itd_initui@files:itdownload.dll stdcall';

function ITD_Internal_LoadStrings(filename:PChar):boolean;
  external 'itd_loadstrings@files:itdownload.dll stdcall';

procedure ITD_Internal_SetOption(option, value: PChar);
  external 'itd_setoption@files:itdownload.dll stdcall';

function ITD_Internal_GetFileSize(url:pchar; var size:Cardinal): boolean;
  external 'itd_getfilesize@files:itdownload.dll stdcall';

function ITD_Internal_GetString(index:integer): boolean;
  external 'itd_getstring@files:itdownload.dll stdcall';

function ITD_Internal_GetOption(option: PChar; buffer: PChar; length: integer): integer;
  external 'itd_getoption@files:itdownload.dll stdcall';

procedure ITD_Internal_SetString(index:integer; value:PChar);
  external 'itd_setstring@files:itdownload.dll stdcall';

procedure ITD_Internal_AddFile(url: PChar; destfilename: PChar);
  external 'itd_addfile@files:itdownload.dll stdcall';

procedure ITD_Internal_AddMirror(url: PChar; destfilename: PChar);
  external 'itd_addmirror@files:itdownload.dll stdcall';

procedure ITD_Internal_AddFileSize(url: PChar; destfilename: PChar; size:integer);
  external 'itd_addfilesize@files:itdownload.dll stdcall';

function ITD_Internal_DownloadFiles(surface:hwnd): integer;
  external 'itd_downloadfiles@files:itdownload.dll stdcall';

function ITD_FileCount: integer;
  external 'itd_filecount@files:itdownload.dll stdcall';

function ITD_Internal_PostPage(url, buffer:PChar; length:integer): boolean;
  external 'itd_postpage@files:itdownload.dll stdcall';


const
  ITDERR_SUCCESS = 0;
  ITDERR_USERCANCEL = 1;
  ITDERR_ERROR = 3;

  {Constants for Language String indexes:}
  ITDS_TitleCaption=200;
  ITDS_TitleDescription=201;

  ITDS_MessageFailRetryContinue=250;
  ITDS_MessageFailRetry=251;

var
  itd_allowcontinue: boolean;
  itd_retryonback: boolean;
  
  ITD_AfterSuccess:procedure(downloadPage:TWizardPage);

procedure ITD_DownloadFiles();
begin
 ITD_Internal_DownloadFiles(0);
end;

procedure ITD_AddFile(const URL,filename:string);
begin
 ITD_Internal_AddFile(URL,filename);
end;

procedure ITD_AddMirror(const URL,filename:string);
begin
 ITD_Internal_AddMirror(URL,filename);
end;

procedure ITD_AddFileSize(const URL,filename:string; size:integer);
begin
 ITD_Internal_AddFileSize(URL,filename,size);
end;

function ITD_HandleSkipPage(sender: TWizardPage): boolean;
begin
  result := (itd_filecount = 0);
end;

procedure ITD_SetString(index:integer; value:string);
begin
  itd_internal_setstring(index,value);
end;

function ITD_GetFileSize(const url:string; var size:cardinal):boolean;
begin
  result:=itd_internal_getfilesize(PChar(url), size);
end;

function ITD_LoadStrings(const filename:string):boolean;
begin
 result:=itd_internal_loadstrings(filename);
end;

function ITD_GetString(index:integer):string;
begin
  itd_internal_getstring(index);
  setlength(result, ITD_GetResultLen);
  ITD_GetResultString(PChar(result), length(result));
end;

procedure ITD_NowDoDownload(sender:TWizardPage);
var err: integer;
begin
  wizardform.backbutton.enabled := false;
  wizardform.nextbutton.enabled := false;

  err := ITD_Internal_DownloadFiles(sender.surface.handle);

  case err of
    ITDERR_SUCCESS: begin
        wizardform.nextbutton.enabled := true;
        wizardform.nextbutton.onclick(nil);
        
        if itd_aftersuccess<>nil then
          itd_aftersuccess(sender);
      end;
    ITDERR_USERCANCEL: ; //Don't show a message, this happens on setup close and cancel click
  else begin
    //Some unexpected error
      wizardform.backbutton.caption := 'Retry';
      wizardform.backbutton.enabled := true;
      wizardform.backbutton.show();
      itd_retryonback := true;

      if itd_allowcontinue then begin //Download failed, we can retry, continue, or exit
        wizardform.nextbutton.enabled := true;
        MsgBox(ITD_GetString(ITDS_MessageFailRetryContinue), mbError, MB_OK)
      end else begin //Download failed, we must retry or exit setup
        MsgBox(ITD_GetString(ITDS_MessageFailRetry), mbError, MB_OK)
      end;
    end;
  end;
end;

procedure ITD_HandleShowPage(sender: TWizardPage);
begin
  wizardform.nextbutton.enabled := false;
  wizardform.backbutton.hide();

  itd_nowdodownload(sender);
end;

function ITD_HandleBackClick(sender: TWizardpage): boolean;
begin
  result := false;
  if itd_retryonback then begin
    itd_retryonback := false;
    itd_nowdodownload(sender);
  end;
end;

procedure ITD_Init;
begin
 //Currently a NOP. Don't count on it in future.
end;

function ITD_PostPage(const url, data:string; out response:string):boolean;
begin
  result:=ITD_Internal_PostPage(PChar(url), PChar(data), length(data));
  
  if result then begin
    setlength(response, ITD_GetResultLen);
    ITD_GetResultString(PChar(response), length(response));
  end;
end;

function ITD_DownloadAfter(afterID:integer):TWizardPage;
var itd_downloadPage:TWizardPage;
begin
  itd_downloadpage := CreateCustomPage(afterID, ITD_GetString(ITDS_TitleCaption), ITD_GetString(ITDS_TitleDescription));

  itd_downloadpage.onactivate := @itd_handleshowpage;
  itd_downloadpage.onshouldskippage := @itd_handleskippage;
  itd_downloadpage.onbackbuttonclick := @itd_handlebackclick;

  itd_internal_initui(itd_downloadpage.surface.handle);
  
  result:=itd_downloadpage;
end;

procedure ITD_SetOption(const option, value: string);
begin
  //The options which call ITD_SetString are depreciated, use ITD_SetString directly
  if comparetext(option, 'UI_Caption') = 0 then
    ITD_SetString(ITDS_TitleCaption, value)
  else
    if comparetext(option, 'UI_Description') = 0 then
      ITD_SetString(ITDS_TitleDescription, value)
    else
      if comparetext(option, 'UI_FailMessage') = 0 then
        ITD_SetString(ITDS_MessageFailRetry, value)
      else
        if comparetext(option, 'UI_FailOrContinueMessage') = 0 then
          ITD_SetString(ITDS_MessageFailRetryContinue, value)
        else
          if comparetext(option, 'UI_AllowContinue') = 0 then
            ITD_AllowContinue := (value = '1')
          else
            itd_internal_setoption(option, value);
end;

function ITD_GetOption(const option: string): string;
begin
  setlength(result, 500);
  setlength(result, itd_internal_getoption(pchar(option), pchar(result), length(result)));
end;
