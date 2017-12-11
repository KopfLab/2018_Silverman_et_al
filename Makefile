# tools for active package development

all: index growth_curves_analysis heterocyst_analysis

index:
	Rscript -e "rmarkdown::render('index.Rmd', output_dir = 'docs')"

growth_curves_analysis:
	Rscript -e "rmarkdown::render('growth_curves_analysis.Rmd', output_dir = 'docs')"
	
heterocyst_analysis:
	Rscript -e "rmarkdown::render('heterocyst_analysis.Rmd', output_dir = 'docs')"