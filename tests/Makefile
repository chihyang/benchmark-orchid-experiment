MK_PATH:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
export GUILE_LOAD_PATH := $(realpath $(MK_PATH)../)

test:
	guile -s unification.scm
	guile -s records.scm
	guile -s language.scm
	guile -s lists.scm
	guile -s peano.scm
	guile -s goat.scm
	guile -s dcg.scm

colors:
	guile -s four-color.scm

expunif:
	guile -s exponential-unification.scm

incremental:
	echo $$GUILE_LOAD_PATH
	guile -s four-color-incremental.scm
