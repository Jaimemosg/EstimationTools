## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, warning=FALSE, message=FALSE---------------------------------
if (!require('readr')) install.packages('readr')
library(readr)

urlRemote <- "https://raw.githubusercontent.com/"
pathGithub <- 'Jaimemosg/EstimationTools/master/extra/'
filename <- 'sim_wei.csv'
myURL <- paste0(urlRemote, pathGithub, filename)
data_sim <- read_csv(myURL)

data_sim$group <- as.factor(data_sim$group)
head(data_sim)

## ----example1, message=FALSE, warning=FALSE-----------------------------------
library(EstimationTools)

# Formulas with linear predictors
formulas <- list(scale.fo = ~ 1, shape.fo = ~ group)

# The model
fit_wei <- maxlogLreg(formulas, data = data_sim,
                      y_dist = Surv(Time, status) ~ dweibull,
                      link = list(over = c("shape", "scale"),
                                  fun = rep("log_link", 2)))
summary(fit_wei)

## ----example2, message=FALSE, warning=FALSE-----------------------------------
x <- rnorm( n = 10000, mean = 160, sd = 6 )
fit <- maxlogL( x = x, dist = "dnorm", link = list(over = "sd", fun = "log_link") )
summary(fit)

