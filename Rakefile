desc "Build files for packaging"
task :default do
    sh 'Rscript -e "devtools::document()"'
    sh 'Rscript -e "knitr::knit(\"README.Rmd\")"'
    sh 'pandoc -o README.html README.md'
    sh 'Rscript -e "library(grid); pkgdown::build_site()"'
end
