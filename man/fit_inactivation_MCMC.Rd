% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_MCMC.R
\name{fit_inactivation_MCMC}
\alias{fit_inactivation_MCMC}
\title{Fitting of dynamic inactivation with MCMC}
\usage{
fit_inactivation_MCMC(
  experiment_data,
  simulation_model,
  temp_profile,
  starting_points,
  upper_bounds,
  lower_bounds,
  known_params,
  ...,
  minimize_log = TRUE,
  tol0 = 1e-05
)
}
\arguments{
\item{experiment_data}{data frame with the experimental data to be adjusted.
It must have a column named \dQuote{time} and another one named
\dQuote{N}.}

\item{simulation_model}{character identifying the model to be used.}

\item{temp_profile}{data frame with discrete values of the temperature for
each time. It must have one column named \code{time} and another named
\code{temperature} providing discrete values of the temperature at
time points.}

\item{starting_points}{starting values of the parameters for the adjustment.}

\item{upper_bounds}{named numerical vector defining the upper bounds of the
parameters for the adjustment.}

\item{lower_bounds}{named numerical vector defining the lower bounds of the
parameters for the adjustment.}

\item{known_params}{named numerical vector with the fixed (i.e., not
adjustable) model parameters.}

\item{...}{other arguments for \link[FME:modMCMC]{FME::modMCMC}.}

\item{minimize_log}{logical. If \code{TRUE}, the adjustment is based on the
minimization of the error of the logarithmic count.}

\item{tol0}{numeric. Observations at time 0 make Weibull-based models singular.
The time for observatins taken at time 0 are changed for this value.}
}
\value{
A list of class \code{FitInactivationMCMC} with the following items:
\itemize{
\item \code{modMCMC}: a list of class \code{modMCMC} with the
information of the adjustment process.
\item best_prediction: a list of class \code{SimulInactivation}
with the prediction generated by the best predictor.
\item data: a data frame with the data used for the fitting.
}
}
\description{
Fits the parameters of an inactivation model to experimental using
the Markov Chain Monte Carlo fitting algorithm implemented in
the function \link[FME:modMCMC]{FME::modMCMC}.
}
\examples{
## EXAMPLE 1 ------
data(dynamic_inactivation)  # The example data set is used.

get_model_data()  # Retrieve the valid model keys.

simulation_model <- "Peleg"  # Peleg's model will be used

model_data <- get_model_data(simulation_model)
model_data$parameters  # Set the model parameters

dummy_temp <- data.frame(time = c(0, 1.25, 2.25, 4.6),
                         temperature = c(70, 105, 105, 70))  # Dummy temp. profile

## Set known parameters and initial points/bounds for unknown ones

known_params = c(temp_crit = 100)

starting_points <- c(n = 1, k_b = 0.25, N0 = 1e+05)
upper_bounds <- c(n = 2, k_b = 1, N0 = 1e6)
lower_bounds <- c(n = 0.5, k_b = 0.1, N0 = 1e4)

MCMC_fit <- fit_inactivation_MCMC(dynamic_inactivation, simulation_model,
                                     dummy_temp, starting_points,
                                     upper_bounds, lower_bounds,
                                     known_params,
                                     niter = 100)
                                     # It is recommended to increase niter

plot(MCMC_fit)
goodness_of_fit(MCMC_fit)

## END EXAMPLE 1 -----

}
