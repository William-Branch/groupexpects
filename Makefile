TEXFILE= main
RDIR= results_R
RDIR2 = ./
FIGDIR= figs

# list R files
#RFILES = $(wildcard $(RDIR)/*.R)

RFILES = $(wildcard $(RDIR)/*.R)
# pdf figures created by R
PDFFIGS := $(wildcard $(FIGDIR)/*.pdf)
# Indicator files to show R file has run
OUT_FILES:= $(RFILES:.R=.Rout)



# Indicator files to show pdfcrop has run
CROP_FILES:= $(PDFFIGS:.pdf=.pdfcrop)

all: $(TEXFILE).pdf $(OUT_FILES) $(CROP_FILES)




# RUN EVERY R FILE

$(RDIR)/%.Rout : $(RDIR)/%.R
	R CMD BATCH $< $@


# CROP EVERY PDF FIG FILE
$(FIGDIR)/%.pdfcrop: $(FIGDIR)/%.pdf
	pdfcrop $< $< && touch $@

#latexmk -pdf -quiet $(TEXFILE)
# Compile main tex file and show errors
$(TEXFILE).pdf: $(TEXFILE).tex $(OUT_FILES) $(CROP_FILES)
	latexmk -xelatex -quiet $(TEXFILE)

# Run R files
R: $(OUT_FILES)

# View main tex file
view: $(TEXFILE).pdf
	evince $(TEXFILE).pdf &

# Clean up stray files
clean:
	rm -fv $(OUT_FILES) 
	rm -fv $(CROP_FILES)
	rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz
	rm -fv *.out *.bcf *blx.bib *.run.xml
	rm -fv *.fdb_latexmk *.fls
	rm -fv $(TEXFILE).pdf

.PHONY: all clean

## databuild : Runs programs that generate .csv files for datasets.
.Phony : databuild 
databuild : 
	R CMD BATCH data_build_R/data_build.R  
	R CMD BATCH data_build_R/data_build_with_pe5.R
	R CMD BATCH data_build_R/data_build_stateCPI.R	
	rm -f data_build.Rout data_build_with_pe5.Rout data_build_stateCPI.Rout 