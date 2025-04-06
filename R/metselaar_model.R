
#'
#' First Derivate of the Metselaar Model
#'
#' Calculates the first derivative of Metselaar model at a given time for
#' the model parameters provided and the environmental conditions given.
#'
#' The model is developed from the isothermal Metselaar model without
#' taking into
#' account in the derivation the time dependence of \eqn{\delta_T} for
#' non-isothermal temperature profiles.
#'
#' This function is compatible with the function
#' \code{\link{predict_inactivation}}.
#'
#' @section Model Equation:
#'
#'      \deqn{\frac{dN}{dt} = -N \cdot p \cdot (1/D)^p \cdot (t/Delta)^{p-1} }{
#'            dN/dt = -N * p * (1/D)^p * (t/Delta)^(p-1)}
#'
#'      \deqn{D(T) = D_{ref} \cdot 10^{- (T-T_ref)/z} }{
#'            D(T) = D_R * 10^(- (T-T_ref)/z )}
#'
#' @param t numeric vector indicating the time of the experiment.
#' @param x list with the value of N at t.
#' @param parms parameters for the secondary model. No explicit check of their validity
#'             is performed (see section \bold{Model Parameters}).
#' @param temp_profile a function that provides the temperature at a given time.
#'
#' @return The value of the first derivative of N at time \code{t} as a list.
#'
#' @section Model Parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation.
#'          \item D_R: D-value at the reference temperature.
#'          \item z: z-value.
#'          \item p: shape factor of the Weibull distribution.
#'          \item Delta: Scaling parameter
#'          }
#'
#' @section Note:
#'      For t=0, dN = 0 unless n=1. Hence, a small shift needs to be introduced
#'      to t.
#'
#' @seealso \code{\link{predict_inactivation}}
#'
dMetselaar_model<- function(t, x, parms, temp_profile)  {

    temp <- temp_profile(t)

    with(as.list(c(x, parms)),{
        D_T <- D_R * 10^( -(temp-temp_ref)/z)
        dN <- - N * p * (1/D_T)^p * (t/Delta)^(p-1) * log(10)

        res <- c(dN)
        return(list(res))
    })

}









