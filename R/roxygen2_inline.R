# Shared documentation inserted with inline R code e.g. `r r2i_registered_patterns_df`

r2i_registered_patterns_df <- paste(
	"a data frame with columns",
	"\\describe{",
	"  \\item{name}{The pattern name.}",
	"  \\item{owner}{The package that registered the pattern or `\"R_GlobalEnv\"` for the user.}",
	"  \\item{kind}{Either `\"geometry\"` or `\"array\"`.}",
	"  \\item{source}{One of `\"builtin\"`, `\"register_pattern\"`, or `\"options\"`.}",
	"  \\item{active}{Whether the bare `name` currently resolves to this pattern.}",
	"}",
	sep = "\n"
)
