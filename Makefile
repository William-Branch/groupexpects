# Makefile to run data_build.R, main2sls.R, mainWeights.R, descriptive_plots.R, and IRF.R scripts

# Define the paths
FUNCTIONS_DIR := functions
SCRIPTS_DIR := scripts


# Define the R command
R := Rscript --vanilla

# Define the install libraries script
INSTALL_LIBRARIES_SCRIPT := $(SCRIPTS_DIR)/install_libraries.R

# Input files required for data_build.R
INPUT_FILES := data/Michigan_220524.csv data/cps_00002.csv

# Output files from data_build.R
DATA_BUILD_OUTPUTS := data/dataBuild/paneldata.csv data/dataBuild/paneldataLag.csv \
                      data/dataBuild/paneldataFirstLag.csv data/dataBuild/paneldataSmallLag.csv \
                      data/dataBuild/paneldataFirst.csv data/dataBuild/paneldataSmall.csv \
                      data/dataBuild/regiondata.csv data/dataBuild/masterdata.csv \
                      data/dataBuild/masterdataFirst.csv data/dataBuild/masterdataSmall.csv \
                      data/dataBuild/localdata.csv data/dataBuild/localdataFirst.csv \
                      data/dataBuild/localdataSmall.csv data/dataBuild/globaldata.csv \
                      data/dataBuild/globaldataFirst.csv data/dataBuild/globaldataSmall.csv \
                      data/dataBuild/localdataBig.csv data/dataBuild/localdataBigSmall.csv \
                      data/dataBuild/panelcps.csv

# Output files from main2sls.R
MAIN2SLS_OUTPUTS := figs/tex/redform.tex figs/tex/firststage.tex figs/tex/redform_cps.tex \
                    figs/tex/firststage_cps.tex tables/base1year.tex tables/base1yearStage1.tex \
                    tables/biascorrections.tex

# Output files from descriptive_plots.R
DESCR_PLOTS_OUTPUTS := figs/tex/weightsDiverse.tex figs/tex/regions.tex figs/tex/menwomen.tex \
                       figs/aggIndComp.pdf figs/weightsTop10comp.pdf figs/weightsTime.pdf

# Output files from mainWeights.R
MAIN_WEIGHTS_OUTPUTS := tables/weightsout.tex tables/weightsCPS.tex tables/testIdent.tex tables/table_markups.tex figs/weightsout.pdf figs/markups.pdf

# Output files from main_IRF.R
MAIN_IRF_OUTPUTS := figs/tex/IRFout.tex figs/tex/IRFcps.tex

# Output files from altEstimates.R
ALT_ESTIMATES_OUTPUTS := tables/2slscomponents.tex tables/biascorrectionsCCE.tex  # Add appropriate output files here

# List of specific TikZ figures to be compiled to PDF
SPECIFIC_TIKZ_FIGURES := figs/tex/firststage figs/tex/firststage_cps figs/tex/redform figs/tex/redform_cps

# Output PDF figures from specific TikZ figures
SPECIFIC_PDF_FIGURES := $(addsuffix .pdf, $(SPECIFIC_TIKZ_FIGURES))

# Default target
all: install_libraries $(DATA_BUILD_OUTPUTS) $(MAIN2SLS_OUTPUTS) $(DESCR_PLOTS_OUTPUTS) $(MAIN_WEIGHTS_OUTPUTS) $(MAIN_IRF_OUTPUTS) $(ALT_ESTIMATES_OUTPUTS) paper

# Target to install missing libraries 
install_libraries:
	$(R) $(INSTALL_LIBRARIES_SCRIPT)


# Target to source common libraries and functions, and run data_build.R
$(DATA_BUILD_OUTPUTS): $(SCRIPTS_DIR)/data_build.R $(FUNCTIONS_DIR)/functions_data.R \
                       $(SCRIPTS_DIR)/common_libraries.R $(INPUT_FILES)
	$(R) -e "source('$(SCRIPTS_DIR)/common_libraries.R'); \
	         source('$(FUNCTIONS_DIR)/functions_data.R'); \
	         source('$(SCRIPTS_DIR)/data_build.R')"

# Target to run main2sls.R, depends on data_build.R outputs
$(MAIN2SLS_OUTPUTS): $(SCRIPTS_DIR)/main2sls.R $(DATA_BUILD_OUTPUTS)
	$(R) -e "source('$(SCRIPTS_DIR)/common_libraries.R'); \
	         source('$(FUNCTIONS_DIR)/functions_data.R'); \
	         source('$(SCRIPTS_DIR)/main2sls.R')"

# Target to run mainWeights.R, depends on data_build.R outputs and functions_results.R
$(MAIN_WEIGHTS_OUTPUTS): $(SCRIPTS_DIR)/mainWeights.R $(FUNCTIONS_DIR)/functions_results.R $(DATA_BUILD_OUTPUTS)
	$(R) -e "source('$(SCRIPTS_DIR)/common_libraries.R'); \
	         source('$(FUNCTIONS_DIR)/functions_results.R'); \
	         source('$(SCRIPTS_DIR)/mainWeights.R')"

# Target to run descriptive_plots.R, depends on data_build.R outputs and functions_results.R
$(DESCR_PLOTS_OUTPUTS): $(SCRIPTS_DIR)/descriptive_plots.R $(FUNCTIONS_DIR)/functions_results.R $(DATA_BUILD_OUTPUTS)
	$(R) -e "source('$(SCRIPTS_DIR)/common_libraries.R'); \
	         source('$(FUNCTIONS_DIR)/functions_results.R'); \
	         source('$(SCRIPTS_DIR)/descriptive_plots.R')"

# Target to run main_IRF.R, depends on data_build.R outputs
$(MAIN_IRF_OUTPUTS): $(SCRIPTS_DIR)/IRF.R $(DATA_BUILD_OUTPUTS)
	$(R) -e "source('$(SCRIPTS_DIR)/common_libraries.R'); \
	         source('$(FUNCTIONS_DIR)/functions_results.R'); \
	         source('$(SCRIPTS_DIR)/IRF.R')"
	         
# Target to run altEstimates.R, depends on data_build.R outputs and functions_extensions.R
$(ALT_ESTIMATES_OUTPUTS): $(SCRIPTS_DIR)/altEstimates.R $(FUNCTIONS_DIR)/functions_extensions.R $(DATA_BUILD_OUTPUTS)
	$(R) -e "source('$(SCRIPTS_DIR)/common_libraries.R'); \
	         source('$(FUNCTIONS_DIR)/functions_extensions.R'); \
	         source('$(SCRIPTS_DIR)/altEstimates.R')"




# Target to compile specific TikZ to PDF
$(SPECIFIC_PDF_FIGURES): %.pdf: %.tex
	lualatex -jobname=$(basename $@) "\def\tikzfile{$(basename $<)}\input{compile_figure.tex}"

# Make the 'paper' target depend on specific PDF figures
paper: $(MAIN2SLS_OUTPUTS) $(DESCR_PLOTS_OUTPUTS) $(MAIN_WEIGHTS_OUTPUTS) $(MAIN_IRF_OUTPUTS) $(ALT_ESTIMATES_OUTPUTS) $(SPECIFIC_PDF_FIGURES)
	lualatex main.tex && \
	bibtex main && \
	lualatex main.tex && \
	lualatex main.tex
	# Remove auxiliary files generated during compilation
	rm -f *.aux *.log *.out *.toc *.blg *.bbl
	
#Target to push to GitHub
update:
	cp main.pdf docs/main.pdf && \
	git add .
	git commit -m "Updated on $(shell date)"
	git push origin main
# Target to clean any intermediate or output files
clean:
	rm -f $(DATA_BUILD_OUTPUTS) $(MAIN2SLS_OUTPUTS) $(DESCR_PLOTS_OUTPUTS) $(MAIN_WEIGHTS_OUTPUTS) $(ALT_ESTIMATES_OUTPUTS)
	rm -f *.pdf *.aux *.log *.out *.toc *.blg *.bbl *.auxlock
