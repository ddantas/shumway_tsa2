############################################################################
#
# Makefile for shumway_tsa2
#
# ddantas 22/08/2022
#
############################################################################


URL = https://www.stat.pitt.edu/stoffer/tsa2/

DOWNLOAD_FOLDER = download
DATA_FOLDER = mydata

download:
	mkdir -p $(DOWNLOAD_FOLDER)
	mkdir -p $(DATA_FOLDER)
	#Chapter 1
	#wget $(URL)data/eq5exp6.dat.txt -P $(DATA_FOLDER)
	#All data
	wget $(URL)data.tar.gz -P $(DOWNLOAD_FOLDER)
	tar -xvzf $(DOWNLOAD_FOLDER)/data.tar.gz
	mv data/* $(DATA_FOLDER)
	rmdir data



















############################################################################

bsc:
	make Modelo-TCC-DCOMP

msc:
	make Modelo-Mestrado-DCOMP


%: %.tex
	echo "Running latex..."
	pdflatex $@.tex
	bibtex $@
	pdflatex $@.tex
	pdflatex $@.tex
	evince $@.pdf

$@.dvi: clean $@.tex
	echo "Running latex..."
	latex $@.tex
	echo "Running makeindex..."
	#makeindex $@.idx
	echo "Rerunning latex...."
	latex $@.tex
	latex_count=5 ; \
	while egrep -s 'Rerun (LaTeX|to get cross-references right)' refman.log && [ $$latex_count -gt 0 ] ;\
	    do \
	      echo "Rerunning latex...." ;\
	      latex $(FILENAME).tex ;\
	      latex_count=`expr $$latex_count - 1` ;\
	    done

clean:
	rm -f *.ps *.dvi *.aux *.toc *.idx *.ind *.ilg *.log *.out *.brf *.blg *.bbl *.loa *.loc *.lof *.loq *.lot
