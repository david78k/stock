shell := /bin/bash

default: run 

run:
	R CMD BATCH run.R

upload:
	git add .
	git commit -a -m all
	git push
