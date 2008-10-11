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
OutputBaseFilename=example1
Compression=lzma
SolidCompression=true
CreateAppDir=true
ShowLanguageDialog=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','ScriptPath','');
#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Tray','ScriptPath','');

[Code]
procedure MyITDEventHandler(event:integer);
begin
 {Extend ITD's default handling of events to include
  handling for a tray icon}

 case event of
  ITD_Event_DownloadPageEntered:begin

      //Only enable minimizing to tray from the download page
	  ITT_SetMinimizesToTray(true);

	  ITT_MinimizeToTray();

	  ITT_ShowBalloon('Setup is downloading files...',
	  'You can continue to use your computer while setup '+
	  'is downloading files.',10);
  end;

  ITD_Event_DownloadPageLeft:begin
    ITT_RestoreFromTray;
	ITT_SetMinimizesToTray(false);
  end;

  ITD_Event_DownloadFailed:begin
	ITT_RestoreFromTray; //Get the user to do something about the error
  end;

 end;
end;

procedure InitializeWizard();
begin
 itd_init;
 itt_init;

 itd_EventHandler:=@MyITDEventHandler;

 //Let's download two zipfiles from my website..
 itd_addfile('http://www.sherlocksoftware.org/petz/files/dogz5.zip',expandconstant('{tmp}\dogz5.zip'));
 itd_addfile('http://www.sherlocksoftware.org/petz/files/petz4g.zip',expandconstant('{tmp}\petz4.zip'));

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
