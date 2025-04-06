
#' First derivative of the Arrhenius model
#'
#' Calculates the first derivative of the Arrhenius model with log-linear
#' inactivation
#' for dynamic problems at a given time for the model parameters provided and
#' the environmental conditions given.
#'
#' This function is compatible with the function
#' \code{\link{predict_inactivation}}.
#'
#' @section Model Equation:
#'
#'    \deqn{\frac{dN}{dt} = - k * N }{
#'          dN/dt = - k * n}
#'
#' @param t numeric vector indicating the time of the experiment.
#' @param x list with the value of N at t.
#' @param parms parameters for the secondary model. No explicit check of their
#'        validity is performed.
#' @param temp_profile a function that provides the temperature at a given
#'        time.
#'
#' @return The value of the first derivative of \eqn{N} at time \code{t} as a
#'         list.
#'
#' @section Model parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation,
#'          \item k_ref: inactivation rate at the ref. temp.
#'          \item Ea: Activation energy.
#'          }
#'
#' @seealso \code{\link{predict_inactivation}}
#'
dArrhenius_model <- function(t, x, parms, temp_profile)  {

    with(as.list(c(parms, x)), {

        temp <- temp_profile(t)

        k <- k_ref * exp(-Ea/8.31 * (1/temp - 1/temp_ref) )

        dN <- - N * k

        res <- c(dN)
        return(list(res))

    })
}














