# name of the tex file, without extension
FILENAME=manual
# output format (used by dvips)
PSFORMAT=manual

all: pdf crop

dvi:
	latex $(FILENAME).tex
	bibtex $(FILENAME)
	latex $(FILENAME).tex
	latex $(FILENAME).tex

ps:	dvi
	dvips -o $(FILENAME).ps -t $(PSFORMAT) $(FILENAME).dvi

lat:
	latex $(FILENAME).tex

#pdf: pdflatex
#	ps2pdf13 -dPDFSETTINGS=/prepress $(FILENAME).ps $(FILENAME).pdf

clean:
	rm -f *.dvi *.ps *.aux *.log *.bbl *.blg *~ *.out *.dia~ *.bak *.pdf

spellcheck:
	aspell check --lang=en_US $(FILENAME).tex

show:
	xdvi -bg White $(FILENAME).dvi &

pdf:
	pdflatex $(FILENAME).tex
	bibtex $(FILENAME)
	pdflatex $(FILENAME).tex
	pdflatex $(FILENAME).tex
	pdflatex -dPDFSETTINGS=/prepress $(FILENAME).tex

crop:
	 pdfcrop --margins "15 15 15 15" $(FILENAME).pdf


#prn:
#	dvips secubat.dvi
#	lpr -h -Php5ps secubat.ps
