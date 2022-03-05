paper: compile
	raco scribble --dest ./ --pdf ++style texstyle.tex ++extra def.tex paper.scrbl ; open paper.pdf

tex: compile
	raco scribble --dest ./ --latex ++style texstyle.tex ++extra def.tex paper.scrbl

pdf: compile
	raco scribble --latex ++style texstyle.tex ++extra def.tex paper.scrbl && \
	pdflatex paper.tex && \
	open paper.pdf 

compile:
	raco make -v paper.scrbl

clean: 
	rm -Rf paper.pdf

fast: tex
	git checkout -- acmart.cls
	pdflatex paper

full: fast
	bibtex paper
	pdflatex paper
	pdflatex paper


