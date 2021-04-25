paper:
	scribble --dest ./ --pdf ++style texstyle.tex ++extra def.tex paper.scrbl ; open paper.pdf

tex:
	raco scribble --dest ./ --latex ++style texstyle.tex ++extra def.tex paper.scrbl

pdf: 
	scribble --latex paper.scrbl && \
	pdflatex paper.tex && \
	open paper.pdf 

install:
	raco pkg install --auto scribble-abbrevs pict-abbrevs gtp-plot gtp-util with-cache

clean: 
	rm -Rf paper.pdf

mf: paper
	mutool poster -y 2 paper.pdf draft.pdf
	rmapi mkdir /experience-draft
	rmapi put draft.pdf /experience-draft/

