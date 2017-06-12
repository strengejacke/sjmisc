# sjmisc - Data and Variable Transformation Functions <img src="man/figures/logo.png" align="right" />

Collection of miscellaneous utility functions, supporting data transformation tasks like recoding, dichotomizing or grouping variables, setting and replacing missing values. The data transformation functions also support labelled data.

The functions of **sjmisc** are designed to work together seamlessly with other packes from the tidyverse, like **dplyr**. For instance, you can use the functions from **sjmisc** both within a pipe-worklflow to manipulate data frames, or to create new variables with `mutate()`. See `vignette("design_philosophy", "sjmisc")` for more details.

## Installation

### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("strengejacke/sjmisc")
```

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

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


## Citation

In case you want / have to cite my package, please use `citation('sjmisc')` for citation information. 
