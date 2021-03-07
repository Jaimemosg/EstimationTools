test_that("running 'maxlogLreg'", {

   failures <- c(55, 187, 216, 240, 244, 335, 361, 373, 375, 386)
   fails <- c(failures, rep(500, 10))
   status <- c(rep(1, length(failures)), rep(0, 10))
   Wei_data <- data.frame(fails = fails, status = status)

   # Formulas with linear predictors
   formulas <- list(scale.fo=~1, shape.fo=~1)

   # Bounds for optimization. Upper bound set with default values (Inf)
   start <- list(
     scale = list(Intercept = 100),
     shape = list(Intercept = 10)
   )
   lower <- list(
     scale = list(Intercept = 0),
     shape = list(Intercept = 0)
   )
   mod_weibull <- maxlogLreg(formulas, y_dist = Surv(fails, status) ~ dweibull,
                             start = start,
                             lower = lower, data = Wei_data)
  expect_equal( is.maxlogL(mod_weibull), TRUE )

})
