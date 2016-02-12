sjmisc - Data Transformation and Labelled Data Utility Functions
------------------------------------------------------------------------------
This package contains utility functions that are useful when carrying out data analysis or basic statistical tests, performing common recode and data transformation tasks or working with labelled data (especially intended for people coming from 'SPSS' and/or who are new to R).

Basically, this package covers four domains of functionality:

* reading and writing data between other statistical packages (like 'SPSS') and R, based on the haven and foreign packages
* hence, this package also includes functions to make working with labelled data easier
* frequently used statistical tests and computation of statistical coefficients, or at least convenient wrappers for such test functions
* frequently applied recoding and variable transformation tasks


## Installation

### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/sjmisc")
```

### Officiale, stable release

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjmisc)](http://cran.r-project.org/package=sjmisc)
&#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/sjmisc)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjmisc")
```

## References, documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


## Citation

In case you want / have to cite my package, please use `citation('sjmisc')` for citation information. 
