R=R-2.14

.SUFFIXES : .tex .testresult .Rnw

.Rnw.tex:
	echo "library(weaver); Sweave(\"$<\", driver=weaver())" | $(R) --no-save --no-restore

.tex.testresult:
	@echo -n `./cmptex.sh $< ./expected/$<` > $@
	@echo "    $<" >> $@

RNW=$(wildcard *.Rnw)
TRESULT=$(RNW:.Rnw=.testresult)
TEX=$(RNW:.Rnw=.tex)

all: testall


testdoc1:
	-rm doc1.tex doc1.testresult
	$(MAKE) doc1.testresult
	@cat doc1.testresult

testdoc2:
	-rm doc2.tex doc2.testresult
	$(MAKE) doc2.testresult
	@cat doc2.testresult

testall:
	$(MAKE) realclean
	$(MAKE) test
	@cp test_summary.out no_cache_test.out
	$(MAKE) clean
	$(MAKE) test
	@cp test_summary.out cache_test.out
	@echo "no cache"
	@cat no_cache_test.out
	@echo "cached"
	@cat cache_test.out

test: ${TEX} ${TRESULT}
	@cat *.testresult
	@cat *.testresult > test_summary.out

${TRESULT}: ${TEX}

${TEX}: ${RNW}

foo: ${TEX}

clean:
	- rm *.tex
	- rm *.testresult

realclean: clean
	- rm -rf r_env_cache
	- rm *.out

