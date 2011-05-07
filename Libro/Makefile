all:
	mkdir output ;\
	cp -R src/* output ;\
	cd output ;\
	pdflatex thesis.tex ;\
	bibtex thesis.aux ;\
	pdflatex thesis.tex ;\
	pdflatex thesis.tex ;\
	cd .. ;\
	cp output/thesis.pdf . ;\
	rm -R output
