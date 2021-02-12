paper:
	scribble --dest ./ --pdf ++style texstyle.tex paper.scrbl ; open paper.pdf

tex:
	raco scribble --dest ./ --latex ++style texstyle.tex paper.scrbl

pdf: 
	scribble --latex paper.scrbl && \
	pdflatex paper.tex && \
	open paper.pdf 

clean: 
	rm -Rf paper.pdf

mf: paper
	mutool poster -y 2 paper.pdf draft.pdf
	rmapi mkdir /experience-draft
	rmapi put draft.pdf /experience-draft/

