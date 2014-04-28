SHELL := /bin/bash

default: runscript

runscript:
	time ./run

arma:
	R CMD BATCH arma.R

ema:
	R CMD BATCH ema.R

stat:
	egrep "\[1\] \"\[Stat\]" *.Rout > stat
