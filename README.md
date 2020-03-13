  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/hchang23/firstpackage.svg?branch=master)](https://travis-ci.com/hchang23/firstpackage)
  
  [![codecov](https://codecov.io/gh/hchang23/firstpackage/branch/master/graph/badge.svg)](https://codecov.io/gh/hchang23/firstpackage)
  <!-- badges: end -->

## Installation

To download the firstpackage package, use the code below:

```{r}
# install.packages("devtools")
devtools::install_github("hchang23/firstpackage")
library(firstpackage)
```

## Use

The vignette explains and gives tutorials for the main functions. To see the 
vignette, use the code below:

```{r}
# install.packages("devtools")
devtools::install_github("hchang23/firstpackage", build_vignette = TRUE, build_opts = c())
library(firstpackage)

# use this to view the vignette in the firstpackage HTML help
help(package = "firstpackage", help_type = "html")

# use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "firstpackage")
```
