manual.html: manual.md Makefile
	pandoc --to=html5 --standalone --css=manual.css --table-of-contents --variable=lang:en \
	  --mathjax="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML" $< > $@
