all:
	mkdir output ;\
	cp -R src/* output ;\
	cd output ;\
	pdflatex thesis.tex ;\
	bibtex myrefs.bib ;\
	pdflatex thesis.tex ;\
	cd .. ;\
	cp output/thesis.pdf . ;\
	rm -R output
