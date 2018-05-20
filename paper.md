---
title: 'sjmisc - Data and Variable Transformation Functions'
tags:
  - R
  - data transformation
  - data wrangling
  - variable recoding
authors:
  - name: Daniel Lüdecke
    orcid: 0000-0002-8895-3206
    affiliation: 1
affiliations:
  - name: University Clinical Center Hamburg-Eppendorf
    index: 1
date: 18 May 2018
bibliography: paper.bib
---

# Summary

Data preparation is a common task in research, which usually takes the most amount of time in the analytical process. There are typically two types of data transformation: arraning and reshaping data sets (like filtering observations or selecting variables, combining data sets etc.) and recoding and converting variables. Statistical software packages should provide convenient tools to fulfil these tasks.

For the _R Project for Statistical Computing_, packages have been released recently that are known to be part of the _tidyverse_. Some of those packages focus on the transformation of data sets. Packages with special focus on transformation of _variables_, which fit into the workflow and design-philosophy of the tidyverse, are missing.

``sjmisc`` is a package for the statistical progamming language **R**, which tries to fill this gap. Basically, this package complements the ``dplyr`` package [@wickham_dplyr_2016] in that ``sjmisc`` takes over data transformation tasks on variables, like recoding, dichotomizing or grouping variables, setting and replacing missing values, etc.

The data transformation functions in this package all support _labelled data_ (or labelled vectors), which is a common data structure in other statistical environments to store meta-information about variables, like variable names, value labels or multiple defined missing values. Working with labelled data is featured by packages like ``haven`` [@haven] or ``sjlabelled`` [@daniel_ludecke_2018_1249216].

## The design of data transformation functions

The design of data transformation functions in this package follows, where appropriate, the tidyverse-approach, with the first argument of a function always being the data (either a data frame or vector), followed by variable names that should be processed by the function. If no variables are specified as argument, the function applies to the complete data that was indicated as first function argument. This design-philosophy makes it possible to combine functions from ``sjimisc`` and the "pipe-workflow", i.e. to create chains of function calls connected with ``magrittr``s pipe-operator.

## Conversion of Variable Types

There are also functions that convert variable types, e.g. from factors to numeric (or vice versa). These functions mimic R base functions, but also share the previously mentioned advantages of supporting labelled data and integrating seamlessly into the well-known pipe-workflow from tidyverse-packages.

The source code for ``sjmisc`` has been archived to Zenodo with the linked DOI: [@daniel_ludecke_2018_1249192]

# References
