build : makedemos

makedemos : install
	ISCC example1.iss
	ISCC example2.iss
	ISCC "example3 1.0.iss"
	ISCC "example3 2.0.iss"
	ISCC example4.iss
	ISCC example5.iss
	ISCC example6.iss
	
install : innotoolsdownloader.exe
	innotoolsdownloader.exe /SILENT

innotoolsdownloader.exe : innotoolsdownloader.iss example1.iss example2.iss example3\ 1.0.iss example3\ 2.0.iss example4.iss example5.iss example6.iss wide\itdownload.dll ansi\itdownload.dll it_download.iss ITDHelp.chm languages\*.ini
	ISCC innotoolsdownloader.iss

ITDHelp.chm : ITDHelp.hnd
	helpndoc "c:\sherlocksoftware\innotools\itd\ITDHelp.hnd" /sxh /oxh="webhelp/" /sxc /oxc="ITDHelp.chm" /c

clean :
	rm innotoolsdownloader.exe ITDHelp.chm