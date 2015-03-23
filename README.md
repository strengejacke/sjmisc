sjmisc - Miscellaneous Data Management Tools
------------------------------------------------------------------------------
This package contains some tools that are useful when carrying out data analysis or interpreting data (especially intended for people coming from SPSS and/or who are new to R). These tool functions support reading and writing data (SPSS, SAS and STATA), variable recoding and weighting, statistical tests, reliability tests and much more.


### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/sjmisc")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjmisc")
```

### References, documentation and exmaples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


### Citation

In case you want / have to cite my package, please use `citation('sjmisc')` for citation information. 


### Changelog of current development build 1.0.0-1

#### Bug fixes
* `mwu` did not accept factors as grouping levels - fixed.
* `mwu` did not work when value range of `grp` vector was not continuously - fixed.


### Changelog of current stable build 1.0.0

#### General
* First release of this package - it contains all utility, recode and statistical test functions from the [sjPlot package](https://github.com/sjPlot/devel/).

#### New functions
* `rec` to recode variables.
* `cv` to compute coefficient of variance.
* `rmse` to compute root-mean-square error.

#### Changes to former sjPlot functions
* Improved internal management of imported data via `haven` and `foreign` packages, so users don't need to care of structure and classed of data read with either haven, foreign or sjPlot's read-functions.
