## ------------------------------------------------------------------------
library(bioinactivation)

## ------------------------------------------------------------------------
data(isothermal_inactivation)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
library(ggplot2)
ggplot(data = isothermal_inactivation) +
    geom_point(aes(x = time, y = log_diff, col = as.factor(temp))) +
    ggtitle("Example dataset: isothermal_inactivation")


## ------------------------------------------------------------------------
data(dynamic_inactivation)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
ggplot(data = dynamic_inactivation) +
    geom_point(aes(x = time, y = log10(N))) +
    ggtitle("Example dataset: dynamic_inactivation. Observations")

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
ggplot(data = dynamic_inactivation) +
    geom_point(aes(x = time, y = temperature))  +
    ggtitle("Example dataset: dynamic_inactivation. Temperature profile")

## ------------------------------------------------------------------------
data(laterosporus_iso)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
ggplot(data = laterosporus_iso) +
    geom_point(aes(x = time, y = log_diff, col = as.factor(temp))) +
    ggtitle("Example dataset: laterosporus_iso")


## ------------------------------------------------------------------------
data(laterosporus_dyna)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
ggplot(data = laterosporus_dyna) +
    geom_point(aes(x = time, y = logN)) +
    ggtitle("Example dataset: laterosporus_dyna. Observations")

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
ggplot(data = laterosporus_dyna) +
    geom_point(aes(x = time, y = temp))  +
    ggtitle("Example dataset: laterosporus_dyna. Temperature profile")

## ---- echo=FALSE, fig.width=6, fig.height=4, fig.align='center'----------
Temperature <- seq(30, 80, length=100)
b <- log( 1 + exp(0.3*(Temperature - 60)))
b_1 <- log( 1 + exp(0.2*(Temperature - 60)))
plot(Temperature, b, type = "l", col="red")
lines(Temperature, b_1, type = "l", col = "blue")
legend("topleft", c("k = 0.3","k = 0.2"), col=c("red", "blue"), lwd=1, cex = 1)

## ------------------------------------------------------------------------
get_model_data()

## ------------------------------------------------------------------------
example_model <- "Geeraerd"

## ------------------------------------------------------------------------
times <- seq(0, 4.5, length=100)

## ------------------------------------------------------------------------
model_data <- get_model_data(example_model)
print(model_data$parameters)
print(model_data$variables)

## ------------------------------------------------------------------------
model_parms <- c(D_R = 3,
                 z = 10,
                 N_min = 1e2,
                 temp_ref = 100,
                 N0 = 1e5,
                 C_c0 = 1e1
                 )

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
temperature_profile <- data.frame(time = c(0, 4.5),
                                  temperature = c(70, 120))

plot(temperature_profile, type = "l")
title("Example temperature profile")

## ------------------------------------------------------------------------
prediction_results <- predict_inactivation(example_model, times, model_parms, temperature_profile)

## ------------------------------------------------------------------------
head(prediction_results$simulation)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
p <- plot(prediction_results)
print(p)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
library(ggplot2)
p + theme_light() + xlab("time (min)")

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(prediction_results, make_gg = FALSE,
     xlab = "Time (min)", ylab = "logN")

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
parms_no_shoulder <- c(D_R = 3,
                       z = 10,
                       N_min = 100,
                       temp_ref = 100,
                       N0 = 100000,
                       C_c0 = 0
                       )


prediction_no_shoulder <- predict_inactivation(example_model, times, parms_no_shoulder, temperature_profile)

plot(prediction_no_shoulder)


## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
parms_no_tail <- c(D_R = 3,
                   z = 10,
                   N_min = 0,
                   temp_ref = 100,
                   N0 = 100000,
                   C_c0 = 100
                   )


prediction_no_tail <- predict_inactivation(example_model, times, parms_no_tail, temperature_profile)

plot(prediction_no_tail)


## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(prediction_results, plot_temp = TRUE)

## ------------------------------------------------------------------------
data(isothermal_inactivation)

## ------------------------------------------------------------------------
get_isothermal_model_data()

## ------------------------------------------------------------------------
model_name <- "Bigelow"

## ------------------------------------------------------------------------
model_data <- get_isothermal_model_data(model_name)
model_data$params

## ------------------------------------------------------------------------
known_params = list(temp_ref = 100)

## ------------------------------------------------------------------------
starting_point <- list(z = 10,
                       D_R = 1.5
                       )

## ------------------------------------------------------------------------
iso_fit <- fit_isothermal_inactivation(model_name, isothermal_inactivation,  
                                       starting_point, known_params)

## ------------------------------------------------------------------------
summary(iso_fit$nls)
vcov(iso_fit$nls)  # Calculates variance-covariance matrix {stats}
confint(iso_fit$nls)  # Calculates confidence intervals {stats}

## ------------------------------------------------------------------------
iso_fit$parameters

## ------------------------------------------------------------------------
iso_fit$model

## ------------------------------------------------------------------------
head(iso_fit$data)

## ------------------------------------------------------------------------
plot(iso_fit, ylab = "Number of logarithmic reductions",
     xlab = "Time (min)")

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(iso_fit, make_gg = TRUE) +
    ylab("Number of logarithmic reductions") +
    xlab("Time (min)")

## ------------------------------------------------------------------------
data(dynamic_inactivation)

## ------------------------------------------------------------------------
get_model_data()

## ------------------------------------------------------------------------
simulation_model <- "Mafart"

## ------------------------------------------------------------------------
dummy_temp <- data.frame(time = dynamic_inactivation$time,
                         temperature = dynamic_inactivation$temperature)

## ------------------------------------------------------------------------
model_data <- get_model_data(simulation_model)
model_data$parameters
model_data$variables

## ------------------------------------------------------------------------
known_params = c(temp_ref = 90)

## ------------------------------------------------------------------------
starting_points <- c(delta_ref = 10,
                     p = 1,
                     z = 10,
                     logN0 = 5)
upper_bounds <- c(delta_ref = 20,
                  p = 2,
                  z = 20,
                  logN0 = Inf)

lower_bounds <- c(delta_ref = 5,
                  p = .5,
                  z = 5,
                  logN0 = 4)

## ------------------------------------------------------------------------
dynamic_fit <- fit_dynamic_inactivation(dynamic_inactivation, simulation_model, dummy_temp,  
                                        starting_points, upper_bounds, lower_bounds,  
                                        known_params)

## ------------------------------------------------------------------------
summary(dynamic_fit$fit_results)

## ------------------------------------------------------------------------
head(dynamic_fit$data)

## ------------------------------------------------------------------------
dynamic_fit$best_prediction$model_parameters

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
p <- plot(dynamic_fit)
p + theme_light()

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(dynamic_fit, plot_temp = TRUE)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(dynamic_fit, make_gg = FALSE,
     xlab = "Time (min)", ylab = "logN")

## ------------------------------------------------------------------------
set.seed(82619)

MCMC_fit <- fit_inactivation_MCMC(dynamic_inactivation, simulation_model, dummy_temp,  
                                        starting_points, upper_bounds, lower_bounds,  
                                        known_params, niter = 50)

## ------------------------------------------------------------------------
summary(MCMC_fit$modMCMC)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
p <- plot(MCMC_fit)
p + xlab("Time (min)")

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(MCMC_fit, plot_temp = TRUE)

## ---- fig.width=6, fig.height=4, fig.align='center'----------------------
plot(MCMC_fit, make_gg = FALSE,
     xlab = "Time (min)", ylab = "logN")

