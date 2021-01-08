.PHONY: html

html:
	(cd site; hugo --minify --cleanDestinationDir)
