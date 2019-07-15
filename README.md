# sjmisc - Data and Variable Transformation Functions <img src="man/figures/logo.png" align="right" />

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjmisc)](https://cran.r-project.org/package=sjmisc) &#160;&#160; [![DOI](http://joss.theoj.org/papers/10.21105/joss.00754/status.svg)](https://doi.org/10.21105/joss.00754) &#160;&#160; [![Documentation](https://img.shields.io/badge/documentation-sjmisc-orange.svg?colorB=E91E63)](https://strengejacke.github.io/sjmisc/) &#160;&#160; [![downloads](http://cranlogs.r-pkg.org/badges/sjmisc)](http://cranlogs.r-pkg.org/) &#160;&#160; [![total](http://cranlogs.r-pkg.org/badges/grand-total/sjmisc)](http://cranlogs.r-pkg.org/)

Data preparation is a common task in research, which usually takes the most amount of time in the analytical process. Packages for data preparation have been released recently as part of the _tidyverse_, focussing on the transformation of data sets. Packages with special focus on transformation of _variables_, which fit into the workflow and design-philosophy of the tidyverse, are missing.

**sjmisc** tries to fill this gap. Basically, this package complements the **dplyr** package in that **sjmisc** takes over data transformation tasks on variables, like recoding, dichotomizing or grouping variables, setting and replacing missing values, etc. A distinctive feature of **sjmisc** is the support for labelled data, which is especially useful for users who often work with data sets from other statistical software packages like _SPSS_ or _Stata_.

The functions of **sjmisc** are designed to work together seamlessly with other packages from the tidyverse, like **dplyr**. For instance, you can use the functions from **sjmisc** both within a pipe-workflow to manipulate data frames, or to create new variables with `mutate()`. See `vignette("design_philosophy", "sjmisc")` for more details.

## Contributing to the package

Please follow [this guide](https://github.com/strengejacke/sjmisc/blob/master/CONTRIBUTING.md) if you like to contribute to this package.

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

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjmisc")
```

## References, documentation and examples

A cheatsheet can be downloaded [from here](http://strengejacke.de/sjmisc-cheatsheet.pdf) (PDF) or from the [RStudio cheatsheet collection](https://www.rstudio.com/resources/cheatsheets/).

For more examples, see package vignettes (`browseVignettes("sjmisc")`).

Please visit [https://strengejacke.github.io/sjmisc/](https://strengejacke.github.io/sjmisc/) for documentation and vignettes.

## Citation

In case you want / have to cite my package, please cite as (see also `citation('sjmisc')`): 

Lüdecke D (2018). sjmisc: Data and Variable Transformation Functions. _Journal of Open
Source Software_, *3*(26), 754. doi: 10.21105/joss.00754

[![DOI](http://joss.theoj.org/papers/10.21105/joss.00754/status.svg)](https://doi.org/10.21105/joss.00754)
