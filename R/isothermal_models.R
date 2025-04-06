
#' Isothermal Bigelow's Model
#'
#' Returns the predicted logarithmic reduction in microbial count according
#' to Bigelow's model for the time, temperature and model parameters given.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param z numeric defining the z-value.
#' @param D_R numeric defining the D-value at the reference temperature.
#' @param temp_ref numeric defining the reference temperature.
#'
#' @return A numeric with the predicted logarithmic reduction
#'         (\eqn{log10(N/N0)}).
#'
Bigelow_iso <- function(time, temp, z, D_R, temp_ref){

    D_T <- D_R * 10^( (temp_ref - temp)/z )
    log_diff <- -time/D_T
    return(log_diff)
}

#' Isothermal Weibull-Mafart Model
#'
#' Returns the predicted logarithmic reduction in microbial count according
#' to Weibull-Mafarts's model for the time, temperature and model parameters
#' given.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param delta_ref numeric defining the delta-value at the reference temperature.
#' @param z numeric defining the z-value.
#' @param p numeric defining shape factor of the Weibull distribution.
#' @param temp_ref numeric indicating the reference temperature.
#'
#' @return A numeric with the predicted logarithmic reduction
#'         (\eqn{log10(N/N0)}).
#'
WeibullMafart_iso <- function(time, temp, delta_ref, z, p, temp_ref){

    delta <- delta_ref * 10^(- (temp - temp_ref)/z )
    log_diff <- -(time/delta)^p
    return(log_diff)
}

#' Isothermal Weibull-Peleg Model
#'
#' Returns the predicted logarithmic reduction in microbial count according
#' to Weibull-Peleg's model for the time, temperature and model parameters
#' given.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param n numeric defining shape factor of the Weibull distribution.
#' @param k_b numeric indicating the slope of the b~temp line.
#' @param temp_crit numeric with the value of the critical temperature.
#'
#' @return A numeric with the predicted logarithmic reduction
#'         (\eqn{log10(N/N0)}).
#'
WeibullPeleg_iso <- function(time, temp, n, k_b, temp_crit){

    b <- log( 1 + exp(k_b*(temp - temp_crit)))
    log_diff <- -b * time^n
    return(log_diff)

}

#' Isothermal Arrhenius model
#'
#' Returns the predicted logarithmic reduction in microbial count according
#' to Arrhenius model for the time, temperature and model parameters
#' given.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param k_ref numeric indicating the inactivation rate at the reference
#' temperature.
#' @param temp_ref numeric indicating the reference temperature.
#' @param Ea numeric indicating the activation energy.
#'
#' @return A numeric with the predicted logarithmic reduction
#'         (\eqn{log10(N/N0)}).
#'
Arrhenius_iso <- function(time, temp, k_ref, temp_ref, Ea) {

    k <- k_ref * exp(-Ea/8.31 * (1/temp - 1/temp_ref) )
    log_diff <- - log(10) * k * time
    log_diff
}

#' Isothermal Metselaar model
#'
#' Returns the predicted logarithmic reduction in microbial count according
#' to Metselaars's model for the time, temperature and model parameters
#' given.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param D_R numeric defining the delta-value at the reference temperature.
#' @param z numeric defining the z-value.
#' @param p numeric defining shape factor of the Weibull distribution.
#' @param temp_ref numeric indicating the reference temperature.
#' @param Delta numeric reparametrization factor
#'
#' @return A numeric with the predicted logarithmic reduction
#'         (\eqn{log10(N/N0)}).
#'
Metselaar_iso <- function(time, temp, D_R, z, p, Delta, temp_ref) {

    D_T <- D_R * 10^( (temp_ref - temp)/z )
    log_diff <- -Delta*(time/D_T/Delta)^p
    return(log_diff)
}

#' Isothermal Geeraerd Model
#'
#' Returns the predicted logarithmic reduction in microbial cont according
#' to Geeraerd's model.
#' The isothermal prediction is calculated by analytical integration of the
#' ode for constant temperature
#'
#' @param time numeric vector with the treatment time
#' @param temp numeric vector with the treatment temperature
#' @param logC0 model parameter describing the shoulder length
#' @param a model parameter describing the intercept of the relation
#' @param z z-value
#'
Geeraerd_iso <- function(time, temp, logC0, a, z) {

  # browser()

  b <- log(10)/z

  ## Secondary models

  SL <- log(10^logC0 + 1)/exp(a + b*temp)
  logk <- a + b*temp
  k <- exp(logk)

  logN0 <- 8

  ## Primary model

  N <- 10^logN0 * exp(-k*time) * exp(k*SL) / ( 1 + ( exp(k*SL) - 1 )*exp(-k*time) )
  log_diff <- log10(N) - logN0

  return(log_diff)

}

