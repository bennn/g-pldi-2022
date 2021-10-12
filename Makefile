paper: compile
	scribble --dest ./ --pdf ++style texstyle.tex ++extra def.tex paper.scrbl ; open paper.pdf

tex: compile
	raco scribble --dest ./ --latex ++style texstyle.tex ++extra def.tex paper.scrbl

pdf: compile
	scribble --latex paper.scrbl && \
	pdflatex paper.tex && \
	open paper.pdf 

install:
	raco pkg install --auto scribble-abbrevs pict-abbrevs gtp-plot gtp-util with-cache

compile:
	raco make -v paper.scrbl

clean: 
	rm -Rf paper.pdf

mf: paper
	mutool poster -y 2 paper.pdf draft.pdf
	rmapi mkdir /experience-draft
	rmapi put draft.pdf /experience-draft/

