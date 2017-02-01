\encoding{UTF-8}
\name{sjmisc-package}
\alias{sjmisc-package}
\alias{sjmisc}
\docType{package}
\title{Data Transformation and Labelled Data Utility Functions}
\description{

\strong{Purpose of this package}

Collection of miscellaneous utility functions (especially intended for people coming from other statistical software packages like 'SPSS', and/or who are new to R), supporting following common tasks when working with data:
\itemize{
  \item Reading and writing data between R and other statistical software packages like 'SPSS', 'SAS' or 'Stata' and working with labelled data; this includes easy ways to get and set label attributes, to convert labelled vectors into factors (and vice versa), or to deal with multiple declared missing values etc.
  \item Data transformation tasks like recoding, dichotomizing or grouping variables, setting and replacing missing values. The data transformation functions also support labelled data.
}

\strong{Design philosophy - consistent api}

The design of this package follows, where appropriate, the \emph{tidyverse-approach}, with the first argument of a function always being the data (either a data frame or vector), followed by variable names that should be processed by the function. If no variables are specified as argument, the function applies to the complete data that was indicated as first function argument.

There are two types of function designs:
\describe{
  \item{\emph{transformation/recoding functions}}{
    Functions like \code{rec()} or \code{dicho()}, which transform or recode variables, typically do \emph{not} return the complete data frame that was given as first argument, but only the transformed and recoded variables specified in the \code{...}-ellipses argument. The variables usually get a suffix, so you can bind these variables as new columns to a data frame.
  }
  \item{\emph{coercing/converting functions}}{
    Functions like \code{to_factor()} or \code{add_labels()}, which convert variables into other types or add additional information like variable or value labels as attribute, typically return the complete data frame that was given as first argument. The variables specified in the \code{...}-ellipses argument are converted, all other variables remain unchanged.
  }
}
}
\author{
Daniel Lüdecke \email{d.luedecke@uke.de}
}
