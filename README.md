
[![Build Status](https://travis-ci.org/BillShipley/CauseAndCorrelation.svg?branch=master)](https://travis-ci.org/BillShipley/CauseAndCorrelation)

# CauseAndCorrelation
Package of functions for the path analysis [summer school course](http://www.billshipley.recherche.usherbrooke.ca/summer%20school%20path%20analysis.htm) by [Bill Shipley](http://www.billshipley.recherche.usherbrooke.ca/) and companion to the book [*Cause and Correlation in Biology: A User's Guide to Path Analysis, Structural Equations and Causal Inference in R, 2nd edition*](http://www.cambridge.org/catalogue/catalogue.asp?isbn=9781107442597).

## Installation

First, make sure that you have the [latest version of R](https://cran.r-project.org/) on your computer, or at least a very recent version. To install the package from GitHub, type the following commands in your [R](https://cran.r-project.org/)  or [RStudio](https://www.rstudio.com/products/RStudio/#Desktop) console:

```r
# install package dependencies first
install.packages("lavaan")
install.packages("ggm")

# install and load devtools to be able to install packages from GitHub with install_github
install.packages("devtools")
library(devtools)

# install CauseAndCorrelation from Bill's GitHub
install_github("BillShipley/CauseAndCorrelation")
library(CauseAndCorrelation)
?Causal.Inference

```
If everything worked, the last command should have opened the help file for the `Causal.Inference` function. 


## If things do not work


### Step 1

Read the error messages and make sure all packages dependencies are installed and loaded, especially package `lavaan` and `ggm`. If a message says that a package could not be loaded, try installing it manually by typing:

```r
# manually installing a dependencies
install.packages("packagename")
```
until all packages are installed. A current bug in `install_github` on Windows prevents the installation of package dependencies of dependencies (`lavaan` and `ggm`).


### Step 2

Although this should not be required, for certain packages with compiled code, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and [MiKTeX](https://miktex.org/) need to be installed on Windows to be able to build source packages. For Mac users, Xcode is required and can be installed through the apple store. Here is a more detailed list of [prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) for building source packages for Windows, Mac and Linux.


### Step 3

In case something goes wrong with the package installation and the previous instructions do not work, here is the code for every function in the package. The code can be copied and pasted in the R console to get the definition of all functions. However, in this case, help files won't be available.


```r
