### Research compendium for "descr2" by Sveinung Arnesen, Dominik Duell, Mikael Poul Johannesson, and Yvette Peters.

[![Travis-CI Build Status](https://travis-ci.org/mikajoh/descr2.svg?branch=master)](https://travis-ci.org/mikajoh/descr2)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mikajoh/descr2?branch=master&svg=true)](https://ci.appveyor.com/project/mikajoh/descr2)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/mikajoh/descr2/master/LICENSE)

This work in progress.

Comments, questions, and suggestions are very welcomed! Please contact me at [mikael.johannesson@uib.no](mailto:mikael.johannesson@uib.no).

#### Install

You can install the reasearch compendium by running (note that you need the `devtools` package installed):

```r
library(devtools)
install_github("mikajoh/descr2")
```

#### Includes

The compendium included several functions used in the analysis (see for instance`?descr2::amce`).

In addition, it includes:

- `analysis/`: R-code needed to reproduce the results
  -  `analysis/01_data.R`: Prepares the raw data from all countries and outputs `eips.csv`.
  -  `analysis/02_analysis.R`: Takes `eips.csv` and outputs the figures and tables used in the paper.
- `analysis/vault/`: The encrypted data (`eips.csv`). The raw original data cannot be shared at this point.
- `analysis/output/figs/`: The figures. 

----

*Note: `eips.csv` cannot be shared openly at this moment. Please [contact me](emailto:mikael.johannesson@uib.no) by email if you wish to obtain a key to decrypt the data.*
