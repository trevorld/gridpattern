desc "Build files for packaging"
task :default do
    sh 'Rscript -e "devtools::document()"'
    sh 'pandoc -o README.md README.rst'
    sh 'rst2html.py README.rst README.html'
    sh 'Rscript -e "pkgdown::build_site()"'
end
