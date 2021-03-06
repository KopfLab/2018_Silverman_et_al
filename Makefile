# tools for active package development

all: index growth_curves_analysis heterocyst_analysis isotope_calibration isotope_analysis pH_O2_calculations other_controls

index:
	Rscript -e "rmarkdown::render('index.Rmd', output_dir = 'docs')"

growth_curves_analysis:
	Rscript -e "rmarkdown::render('growth_curves_analysis.Rmd', output_dir = 'docs')"
	
heterocyst_analysis:
	Rscript -e "rmarkdown::render('heterocyst_analysis.Rmd', output_dir = 'docs')"
	
isotope_calibration:
	Rscript -e "rmarkdown::render('isotope_calibration.Rmd', output_dir = 'docs')"
	
isotope_analysis:
	Rscript -e "rmarkdown::render('isotope_analysis.Rmd', output_dir = 'docs')"
	
pH_O2_calculations:
	Rscript -e "rmarkdown::render('pH_O2_calculations.Rmd', output_dir = 'docs')"
	
other_controls:
	Rscript -e "rmarkdown::render('other_controls.Rmd', output_dir = 'docs')"