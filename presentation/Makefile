presentation.pdf: presentation.tex ../code/lt.R
	R CMD BATCH ../code/lt.R
	pdflatex presentation.tex

clean:
	rm -f presentation.aux presentation.log presentation.nav presentation.out presentation.pdf presentation.snm presentation.toc
