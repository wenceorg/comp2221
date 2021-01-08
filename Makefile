.PHONY: html

html: allcode
	(cd site; hugo --minify --cleanDestinationDir)

allcode:
	rsync --delete -rupm code/ site/static/code/ --filter '+ *.hs' --filter '- *'
