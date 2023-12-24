out_md=README.md
Rscript -e "rmarkdown::render('generateREADME.rmd', output_file=\"$out_md\", output_format = \"github_document\")"