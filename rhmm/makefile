SHELL := /bin/bash

default: runhmm

runhmm:
	time ./run
#	R CMD BATCH run.R

upload:
	git add .
	git commit -a -m all
	git push
