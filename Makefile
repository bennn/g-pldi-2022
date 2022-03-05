init:
	git submodule update --init code/gtp-measure/ code/gtp-plot/ code/gtp-util/ code/require-typed-check/ code/typed-racket/

install:
	cd code/typed-racket; raco pkg update --link ./source-syntax/ ./typed-racket-compatibility/ ./typed-racket-doc/ ./typed-racket-lib/ ./typed-racket-more/ ./typed-racket/
	cd code; raco pkg install --auto ./gtp-measure ./gtp-plot ./gtp-util ./require-typed-check
	raco pkg install --auto ./g-pldi-2022

pdf:
	cd g-pldi-2022 && \
	raco scribble --latex ++style texstyle.tex ++extra def.tex paper.scrbl && \
	pdflatex paper && \
	bibtex paper && \
	pdflatex paper && \
	pdflatex paper

