TALK=talk.ss
raco=/Applications/Racket-8.3-CS/bin/raco

#install:
#	${raco} pkg install --auto pict-abbrevs scribble-abbrevs gtp-pict gtp-plot

compile:
	${raco} make -v ${TALK}

pict: compile
	${raco} pict ${TALK}

preview: compile
	${raco} slideshow --widescreen --right-half-screen ${TALK}

show: compile
	${raco} slideshow --widescreen ${TALK}

pdf: compile
	${raco} slideshow --widescreen --condense --pdf ${TALK}

