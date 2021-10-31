assert_suggested <- function(package, pattern) {
    if (!requireNamespace(package, quietly = TRUE)) {
        abort(c(glue("The suggested package {{{package}}} must be installed ",
                   'in order to use the "{pattern}" pattern.'),
               i = glue('Install with the command `install.packages("{package}")`')))
    }
}
