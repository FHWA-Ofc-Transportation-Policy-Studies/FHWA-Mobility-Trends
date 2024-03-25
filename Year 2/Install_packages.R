
# FHWA Mobility Trends Year 2 package requirements
# Date: March 20, 2024
# Software Version: 1.0.0
# Creator Name: Zach Pate
# Summary: The below packages are required to run the Mobility Trends R code.
# History of Modification:
#         N/A

#R version = 4.2.2

#User needs 'Devtools' package in order to install specific version of packages
install.packages("devtools")
require(devtools)

install.version('car', '3.1.2', repos = "http://cran.us.r-project.org")
install.version('caret', '6.0.94', repos = "http://cran.us.r-project.org")
install.version('tidyr', '1.3.1', repos = "http://cran.us.r-project.org")
install.version('glmnet', '4.1.8', repos = "http://cran.us.r-project.org")
install.version('corrplot', '0.92', repos = "http://cran.us.r-project.org")
install.version('ggplot2', '3.4.4', repos = "http://cran.us.r-project.org")
install.version('tidyverse', '2.0.0', repos = "http://cran.us.r-project.org")
install.version('omnibus', '1.2.9', repos = "http://cran.us.r-project.org")


library(car)
library(caret)
library(tidyr)
library(glmnet) #documentation at: https://glmnet.stanford.edu/articles/glmnet.html
library(corrplot)
library(ggplot2)
library(tidyverse)
library(omnibus) 


