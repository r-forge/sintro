#$Header: /u/math/j40/cvsroot/lectures/src/insider/profile/Makefile,v 1.9 2012/04/03 18:02:44 j40 Exp $
.PHONY: clean
clean:
	-rm -f *.aux *.log *.bbl *.blg *.glo \
	*.gls *.idx *.ind *.ilg *.inf *.mp *.toc *.pdfsync \
	*.nav *.out *.snm *.top *.tpt *.tui *.vrb

	@for d in * ; do \
	 if test -d  "$${d}" ; then \
	 echo directory "$${d}"; \
	  if (test "$${d}" != CVS); then	  (cd $${d} && $(MAKE) $@;) fi;\
	  fi \
	done

pdf:
	pdflatex profilebeamer
	pdflatex profilebeamer
	bibtex profilebeamer
	makeindex profilebeamer || echo "## no index"
	pdflatex profilebeamer
	#makeindex profilebea-mer.nlo -s nomencl.ist -o profilebeamer.nls || echo "## no name index"
	#pdflatex profilebeamer
	thumbpdf profilebeamer
	pdflatex profilebeamer
	mv profilebeamer.pdf profilebeamer_bak.pdf
	qpdf profilebeamer_bak.pdf profilebeamer.pdf

temp:
	pdflatex profilebeamer_temp
	pdflatex profilebeamer_temp
	bibtex profilebeamer_temp
	makeindex profilebeamer_temp || echo "## no index"
	pdflatex profilebeamer_temp
	#makeindex profilebeamer_temp.nlo -s nomencl.ist -o profilebeamer_temp.nls || echo "## no name index"
	#pdflatex profilebeamer_temp
	thumbpdf profilebeamer_temp
	pdflatex profilebeamer_temp
	mv profilebeamer_temp.pdf profilebeamer_temp_bak.pdf
	qpdf profilebeamer_temp_bak.pdf profilebeamer8.pdf
