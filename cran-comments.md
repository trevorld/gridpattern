Re-submission with the following changes requested by Gregor Seyer:

* Now documents a return value in `mean_col.Rd` and `star_scale.Rd`.
* Examples that may throw an ERROR in certain situations that 
  were previously wrapped by `dontrun{}` are now wrapped by `try({})`.

## Test environments

* appveyor (windows, R devel)
* appveyor (windows, R release)
* local (linux, R 4.1.0)
* rhub (macos, R release)
* travis-ci (linux, R devel)
* travis-ci (linux, R release)
* win-builder (windows, R devel)

## R CMD check --as-cran results

1 NOTE (a new submission with MIT LICENSE file)
