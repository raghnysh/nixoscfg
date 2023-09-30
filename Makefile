.PHONY: all
all: configuration.pdf configuration-noweb.nix

configuration.pdf: configuration.tex
	latexmk -pdf -file-line-error -halt-on-error -interaction=nonstopmode -synctex=1 configuration.tex
	cp configuration.synctex.gz configuration.synctex.orig.gz && gunzip -c configuration.synctex.orig.gz | sed 's@configuration\.tex@configuration.nw@' | gzip > configuration.synctex.gz

configuration.tex: configuration.nw configuration.bib
	noweave -delay -index configuration.nw > configuration.tex

configuration-noweb.nix: configuration.nw
	notangle -Rconfiguration-noweb.nix configuration.nw > configuration-noweb.nix

.PHONY: clean
clean:
	if test -e configuration.tex; then latexmk -C -pdf -bibtex -e '$$clean_ext="run.xml synctex.gz synctex.orig.gz %R-noweb.nix"' configuration.tex; rm configuration.tex; fi


