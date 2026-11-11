# Registers `method` as the S3 method of this package's `generic` for `class`
# (as another package would for its own class) until `.local_envir` exits.
local_s3_method <- function(generic, class, method, .local_envir = parent.frame()) {
	ns <- asNamespace("gridpattern")
	registerS3method(generic, class, method, envir = ns)
	table <- get(".__S3MethodsTable__.", envir = ns)
	withr::defer(rm(list = paste(generic, class, sep = "."), envir = table), envir = .local_envir)
	invisible(method)
}
