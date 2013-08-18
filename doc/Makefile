manual.html: manual.md Makefile
	pandoc --to=html5 --standalone --css=manual.css --table-of-contents --variable=lang:en --mathjax $< > $@
