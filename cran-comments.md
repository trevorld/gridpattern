## Test environments

* local (linux, R 4.2.0)
* github actions (windows, R release)
* github actions (osx, R release)
* win-builder (windows, R devel)

## R CMD check --as-cran results

1 NOTE on 'oldrel':

```
create_gradient_as_geometry: no visible global function definition for
 ‘linearGradient’
create_gradient_as_geometry: no visible global function definition for
 ‘radialGradient’
Undefined global functions or variables:
 linearGradient radialGradient 
```

The `linearGradient()` and `radialGradient()` functions are only available
in the `{grid}` package bundled with R 4.1 and later.  
This package checks the R version and if the R version is too old to support 
these new functions it falls back to an alternative.
Hence this package should not depend on R (>= 4.1).
