SHELL := /bin/bash

default: runscript getstat

runscript:
	time ./run

arma:
	R CMD BATCH arma.R

ema:
	R CMD BATCH ema.R

getstat:
	rm stat
	egrep "\[1\] \"\[Stat\]" *.Rout > stat
	cat stat
