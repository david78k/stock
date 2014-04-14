SHELL := /bin/bash

default: runhmm

runhmm:
	R CMD BATCH run.R

upload:
	git add .
	git commit -a -m all
	git push
