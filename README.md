# dtree


This is a simple R package implementing a variation of the DOT language for specifying decision tree models in health economics, and including utilities for visualisation and applying model to arbitrary PSA data.

See vignette: markdown version in vignettes/; html version in inst/doc/. Vignette can also be viewed post-installation by loading dtree typing vignette('dtree').

Package can be installed using devtools. (devtools::install_github('petedodd/dtree',dependencies=TRUE,build_vignettes=TRUE))

I'm deprecating this because:

- `dtree` is already taken as a package name on CRAN
- my attention has been drawn to data.tree, which may enable greater flexibility

See `HEdtree` for ongoing work on this even thinner layer.
