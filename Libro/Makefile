all:
	mkdir output ;\
	cp -R src/* output ;\
	cd output ;\
	pdflatex  -shell-escape thesis.tex ;\
	bibtex thesis.aux ;\
	pdflatex  -shell-escape thesis.tex ;\
	pdflatex  -shell-escape thesis.tex ;\
	cd .. ;\
	cp output/thesis.pdf . ;\
	rm -R output
