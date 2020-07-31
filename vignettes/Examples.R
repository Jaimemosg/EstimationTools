## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, warning=FALSE, message=FALSE---------------------------------
library(RCurl)
library(foreign)

url_data <- "https://raw.githubusercontent.com/Jaimemosg/EstimationTools/master/extra/sim_wei.csv"
wei_data <- getURL(url_data)
data <- read.csv(textConnection(wei_data), header = TRUE)
data$group <- as.factor(data$group)
head(data)

## ----example1, message=FALSE, warning=FALSE-----------------------------------
library(EstimationTools)

# Formulas with linear predictors
formulas <- list(scale.fo = ~ 1, shape.fo = ~ group)

# The model
fit_wei <- maxlogLreg(formulas, data = data,
                      y_dist = Surv(Time, status) ~ dweibull,
                      link = list(over = c("shape", "scale"),
                                  fun = rep("log_link", 2)))
summary(fit_wei)

## ----example2, message=FALSE, warning=FALSE-----------------------------------
x <- rnorm( n = 10000, mean = 160, sd = 6 )
fit <- maxlogL( x = x, dist = "dnorm", link = list(over = "sd", fun = "log_link") )
summary(fit)

