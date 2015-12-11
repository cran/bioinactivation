
#'
#' Test of SimulInactivation
#'
#' Tests if an object is of class \code{SimulInactivation}.
#'
#' @param x object to be checked.
#'
#' @return A logic specifying whether \code{x} is of class
#'         \code{SimulInactivation}
#'
#' @export
#'
is.SimulInactivation <- function(x) inherits(x, "SimulInactivation")

#'
#' Plot of SimulInactivation Object
#'
#' Plots the predicted evolution of the logarithmic count with time for an
#' instance of \code{SimulInactivation}.
#'
#' @param x The object of class \code{SimulInactivation} to plot.
#' @param y,... ignored
#'
#' @export
#'
#' @importFrom graphics plot
#'
plot.SimulInactivation <- function(x, y=NULL, ...) {

    plot(logN ~ time, data = x$simulation, type = "l", ...)

}

#'
#' Plot of IsoFitInactivation Object
#'
#' For each one of the temperatures studied, plots a comparison between the
#' predicted result and the experimental one for an instance of
#' \code{IsoFitInactivation}.
#'
#' @param x the object of class \code{IsoFitInactivation} to plot.
#' @param y,... ignored
#'
#' @export
#'
#' @importFrom graphics plot lines title
#'
plot.IsoFitInactivation <- function(x, y=NULL, ...) {

    death_data <- x$data
    model_data <- get_isothermal_model_data(x$model)

    for (each_temp in unique(death_data$temp)) {

        temp_indexes <- death_data$temp == each_temp
        my_data <- death_data[temp_indexes, ]

        # my_data <- subset(death_data, temp == each_temp)

        plot(log_diff ~ time, data = my_data)

        max_time <- max(my_data$time)
        times <- seq(0, max_time, length= 100)
        arguments_call <- c(list(time = times, temp = each_temp), x$parameters)

        prediction <- do.call(model_data$prediction, arguments_call)

        lines(times, prediction)
        title(paste("Temperature:", each_temp))

    }
}

#'
#' Plot of FitInactivation Object
#'
#' Plots a comparison between the experimental data provided and the prediction
#' produced by the model parameters adjusted for an instance of
#' \code{FitInactivation}.
#'
#' @param x the object of class \code{FitInactivation} to plot.
#' @param y,... ignored
#'
#' @export
#'
#' @importFrom graphics plot points
#'
plot.FitInactivation <- function(x, y=NULL, ...) {

    death_data <- x$data

    if (!("logN" %in% names(death_data))) {

        death_data$logN <- log10(death_data$N)
    }

    #- Find the limits

    min_logN_data <- min(death_data$logN, na.rm = TRUE)
    min_logN_pred <- min(x$best_prediction$simulation$logN, na.rm = TRUE)
    min_logN <- min(c(min_logN_data, min_logN_pred))

    max_logN_data <- max(death_data$logN, na.rm = TRUE)
    max_logN_pred <- max(x$best_prediction$simulation$logN, na.rm = TRUE)
    max_logN <- max(c(max_logN_data, max_logN_pred))

    ylim <- c(floor(min_logN), ceiling(max_logN))

    #- Make the plot

    plot(x$best_prediction, ylim = ylim)

    points(logN ~ time, data = death_data)

}

#'
#' Plot of FitInactivationMCMC Object
#'
#' Plots a comparison between the experimental data provided and the prediction
#' produced by the model parameters adjusted for an instance of
#' \code{FitInactivationMCMC}.
#'
#' @param x the object of class \code{FitInactivation} to plot.
#' @param y,... ignored
#'
#' @export
#'
plot.FitInactivationMCMC <- function(x, y=NULL, ...) {

    plot.FitInactivation(x)

}

