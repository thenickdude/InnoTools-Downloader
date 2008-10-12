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
OutputBaseFilename=example5
Compression=lzma
SolidCompression=true
CreateAppDir=true
ShowLanguageDialog=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','ScriptPath','');
#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Tray','ScriptPath','');

[Code]
{
	Example 5

	This example demonstrates how to integrate InnoTools Downloader with
	InnoTools Tray to have setup minimize to the tray while downloading.

	You must have InnoTools Tray installed to compile this example.
}

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
	ITT_SetMinimizesToTray(false);

	if ITT_IsInTray then begin

		{ITT_ShowBalloon('Setup has finished downloading files',
		   'Click here to continue installation',10);}

		ITT_RestoreFromTray;

	end;
  end;

  ITD_Event_DownloadFailed:begin
	ITT_RestoreFromTray; //Get the user to do something about the error
  end;

 end;
end;

procedure InitializeWizard();
begin
 itd_init;
 itt_init; //Important! Create (but don't display yet) the tray icon

 itt_sethint('Downloading files...');

 itd_EventHandler:=@MyITDEventHandler;

 //Let's download two zipfiles from my website..
 itd_addfile('http://www.sherlocksoftware.org/petz/files/dogz5.zip',expandconstant('{tmp}\dogz5.zip'));
 itd_addfile('http://www.sherlocksoftware.org/petz/files/petz4.zip',expandconstant('{tmp}\petz4.zip'));

 {While we're at it, let's change the default appearance
  from the "simple mode" to the "detailed mode" that appears
  when you click the "Details" button}
 itd_setoption('UI_DetailedMode', '1');

 {And let's change the agent string that is used to identify
  the "browser" that is making HTTP requests}
 itd_setoption('ITD_Agent', 'They cut the hard line, it''s a trap. Get out!');

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
