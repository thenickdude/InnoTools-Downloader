HELPNDOC_PATH = /mnt/c/Program\ Files\ \(x86\)/IBE\ Software/HelpNDoc\ 5/hnd5.exe

EXAMPLES = example1.iss example2.iss example3\ 1.0.iss example3\ 2.0.iss example4.iss example5.iss example6.iss

.PHONY : build release

build : makedemos

release : build help/html/index.html

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

innotoolsdownloader.exe : innotoolsdownloader.iss $(EXAMPLES) itdownload.dll\
						  it_download.iss help/chm/ITDHelp.chm languages\*.ini
	ISCC innotoolsdownloader.iss

help/chm/ITDHelp.chm : help/ITDHelp.hnd
	mkdir -p help/chm
	# Appears to be broken at the moment, build it using the HelpNDoc GUI instead
	cd help && $(HELPNDOC_PATH) ITDHelp.hnd build -verysilent \
		-only="Build CHM documentation" -output="Build CHM documentation:chm/ITDHelp.chm"

help/html/index.html : help/ITDHelp.hnd
	mkdir -p help/html
	$(HELPNDOC_PATH) help/ITDHelp.hnd build -verysilent \
		-only="Build html documentation" -output="Build HTML documentation:help/html"

clean :
	rm -f innotoolsdownloader.exe itdownload.dll
	rm -rf help/chm
	rm -rf help/html
