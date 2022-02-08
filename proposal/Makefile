FILENAME = proposal

$(FILENAME).pdf: $(FILENAME).tex refs.bib
	pdflatex $^ && bibtex $(FILENAME) && pdflatex $^ && pdflatex $^

clean:
	rm -rf *.aux *.bbl *.blg *.log
