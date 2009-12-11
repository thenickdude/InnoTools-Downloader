#define MyAppName "My Program"
#define MyAppVerName "My Program 1.5"
#define MyAppPublisher "My Company, Inc."
#define MyAppURL "http://www.mycompany.com"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
OutputBaseFilename=example6
Compression=lzma
SolidCompression=true
CreateAppDir=true
ShowLanguageDialog=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','ScriptPath','');

[Code]
function MyAllowContinueEvent:integer;
begin
 //Allow installation to continue only if the critical file downloaded successfully
 if ITD_IsDownloadComplete(expandconstant('{tmp}\dogz5.zip')) then
   result:=ITD_Silently_Continue
 else
   result:=ITD_No_Continue;
   
 // (The other possibility is ITD_Offer_Continue)
end;

procedure InitializeWizard();
begin
 itd_init();

 //Let's download two zipfiles from my website..
 itd_addfile('http://www.sherlocksoftware.org/petz/files/dogz5.zip',expandconstant('{tmp}\dogz5.zip'));
 itd_addfile('http://www.sherlocksoftware.org/petz/files/petz4.zap',expandconstant('{tmp}\petz4.zip'));

 {One of these files is critical and the installation should not continue if
 the file cannot be downloaded. The other is non-critical and the user should
 have the option of continuing setup if the file did not download.
 
 The second file here is non-critical, and in fact I've misspelled the URL so the download will fail.

 We'll install a custom handler to decide if continuing is allowed}
 itd_allowcontinueevent:=@MyAllowContinueEvent;

 //Start the download after the "Ready to install" screen is shown
 itd_downloadafter(wpReady);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
 if CurStep=ssInstall then begin //Lets install those files that were downloaded for us
  filecopy(expandconstant('{tmp}\dogz5.zip'),expandconstant('{app}\dogz5.zip'),false);
  filecopy(expandconstant('{tmp}\petz4.zip'),expandconstant('{app}\petz4.zip'),false);
 end;
end;
