paper: compile
	scribble --dest ./ --pdf ++style texstyle.tex ++extra def.tex paper.scrbl ; open paper.pdf

tex: compile
	raco scribble --dest ./ --latex ++style texstyle.tex ++extra def.tex paper.scrbl

pdf: compile
	scribble --latex paper.scrbl && \
	pdflatex paper.tex && \
	open paper.pdf 

install:
	cd code; raco pkg update --link ./source-syntax/ ./typed-racket-compatibility/ ./typed-racket-doc/ ./typed-racket-lib/ ./typed-racket-more/ ./typed-racket-test/ ./typed-racket/
	cd code; raco pkg install ./gtp-measure ./gtp-plot ./gtp-util ./require-typed-check
	#raco pkg install

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

mf: paper
	mutool poster -y 2 paper.pdf draft.pdf
	rmapi mkdir /experience-draft
	rmapi put draft.pdf /experience-draft/

init:
	git submodule update --init code/gtp-measure/ code/gtp-plot/ code/gtp-util/ code/require-typed-check/ code/typed-racket/

