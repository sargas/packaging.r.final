# packaging.r.final

[![Travis build status](https://travis-ci.org/sargas/packaging.r.final.svg?branch=master)](https://travis-ci.org/sargas/packaging.r.final)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/sargas/packaging.r.final?branch=master&svg=true)](https://ci.appveyor.com/project/sargas/packaging.r.final)

The goal of packaging.r.final is to study reported fatalities in the Fatality Analysis Reporting System datasets provided by the US National Highway Traffic Safety Administration.

## Installation

You can install the this version of packaging.r.final from [GitHub](https://github.com/sargas/packaging.r.final) with:

``` r
devtools::install_github("sargas/packaging.r.final")
```

## Example

This is a basic example for plotting the traffic fatalities recorded in California in 2013:

``` r
library(packaging.r.final)
fars_map_state(6, 2013)
```

