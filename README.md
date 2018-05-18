# sjmisc - Data and Variable Transformation Functions <img src="man/figures/logo.png" align="right" />

[![DOI](https://zenodo.org/badge/32622582.svg)](https://zenodo.org/badge/latestdoi/32622582)

Collection of miscellaneous utility functions, supporting data transformation tasks like recoding, dichotomizing or grouping variables, setting and replacing missing values. The data transformation functions also support labelled data.

The functions of **sjmisc** are designed to work together seamlessly with other packes from the tidyverse, like **dplyr**. For instance, you can use the functions from **sjmisc** both within a pipe-workflow to manipulate data frames, or to create new variables with `mutate()`. See `vignette("design_philosophy", "sjmisc")` for more details.

## Installation

### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("strengejacke/sjmisc")
```

Please note the package dependencies when installing from GitHub. The GitHub version of this package may depend on latest GitHub versions of my other packages, so you may need to install those first, if you encounter any problems. Here's the order for installing packages from GitHub:

[sjlabelled](https://github.com/strengejacke/sjlabelled) &rarr; [sjmisc](https://github.com/strengejacke/sjmisc) &rarr; [sjstats](https://github.com/strengejacke/sjstats) &rarr; [ggeffects](https://github.com/strengejacke/ggeffects) &rarr; [sjPlot](https://github.com/strengejacke/sjPlot)

### Officiale, stable release

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjmisc)](https://cran.r-project.org/package=sjmisc)
&#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/sjmisc)](http://cranlogs.r-pkg.org/)
&#160;&#160;
[![total](http://cranlogs.r-pkg.org/badges/grand-total/sjmisc)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjmisc")
```

## References, documentation and examples

A cheatsheet can be downloaded [from here](http://strengejacke.de/sjPlot/sjmisc-cheatsheet.pdf) (PDF) or from the [RStudio cheatsheet collection](https://www.rstudio.com/resources/cheatsheets/).

For more examples, see package vignettes (`browseVignettes("sjmisc")`).

## Citation

In case you want / have to cite my package, please use `citation('sjmisc')` for citation information. 

[![DOI](https://zenodo.org/badge/32622582.svg)](https://zenodo.org/badge/latestdoi/32622582)
