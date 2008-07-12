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
OutputBaseFilename=example4
Compression=lzma
SolidCompression=true
CreateAppDir=true
ShowLanguageDialog=yes

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: leet; MessagesFile: compiler:Languages\French.isl

#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','ScriptPath','');

[Files]
Source: languages\itd_leet.ini; Flags: dontcopy
Source: languages\itd_en.ini; Flags: dontcopy

[Code]
{ EXAMPLE 4

  In this example, a translation for the UI is read from a file.

  The second language I've defined in [Languages] is French, but
  (since ITD doesn't have a French translation yet) I've renamed
  it to "leet", to use the "leetspeak" translation I wrote.. :)}

procedure InitializeWizard();
var lang:string;
begin
 itd_init;

 {Extract the correct ITD language file for InnoSetup's
  chosen language.}

 lang:=ExpandConstant('{language}');

 if (lang<>'leet') and (lang<>'en') then
  lang:='en';

 ExtractTemporaryFile('itd_'+lang+'.ini');
 itd_loadstrings(expandconstant('{tmp}\itd_'+lang+'.ini'));

 //Let's download two zipfiles from my website..
 itd_addfile('http://www.sherlocksoftware.org/petz/files/dogz5.zip',expandconstant('{tmp}\dogz5.zip'));
 itd_addfile('http://www.sherlocksoftware.org/petz/files/petz4.zip',expandconstant('{tmp}\petz4.zip'));

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
