#$Header: /u/math/j40/cvsroot/lectures/src/Rstuff/lshorth/Makefile,v 1.3 2007/08/12 11:56:07 j40 Exp $
SUBDIRS= SIntro statnat linmod tex_cookbook dataanalysis shared
.PHONY: clean
clean:
	-rm -f *.aux *.log *.bbl *.blg *.glo \
	*.gls *.idx *.ind *.ilg *.inf *.toc *.tpt *.pdfsync
	@for d in * ; do \
	 if test -d  $${d} ; then \
	 echo directory $${d}; \
	  if (test "$${d}" != CVS); then	  (cd $${d} && $(MAKE) $@;) fi;\
	  fi \
	done

pdf:
	@for d in * ; do \
	 if test -d  $${d} ; then \
	 echo directory $${d}; \
	  if (test "$${d}" != CVS); then	  (cd $${d} && $(MAKE) $@;) fi;\
	  fi \
	done
	
check:
	R CMD check ~/Documents/lectures/src/Rstuff/lshorth

build:
	R CMD build ~/Documents/lectures/src/Rstuff/lshorth

install:
	R CMD install ~/Documents/lectures/src/Rstuff/lshorth/lshorth*.gz

remove:
	R CMD remove lshorth

#	mkdir ../packages/lshorth_temp
#	cd ../packages/lshorth_temp
