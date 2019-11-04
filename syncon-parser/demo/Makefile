out.html: lang.synconc *.source
	@syncon-parser parse lang.synconc --html=out.html *.source

lang.synconc: *.syncon
	@syncon-parser compile --output=lang.synconc *.syncon

watch:
	ls *.source *.syncon | entr -r -c make --no-print-directory
