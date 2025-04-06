
#'
#' Plot of SimulInactivation Object
#'
#' Plots the predicted evolution of the logarithmic count with time for an
#' instance of \code{SimulInactivation}.
#'
#' @param x The object of class \code{SimulInactivation} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param make_gg logical. If \code{TRUE}, the plot is created using
#' \code{ggplot2}. Otherwise, the plot is crated with base \code{R}.
#' \code{TRUE} by default.
#' @param plot_temp logical. Whether the temperature profile will
#' be added to the plot. \code{FALSE} by default.
#' @param label_y1 Label of the principal y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param ylims Numeric vector of length 2 with the Limits of the
#' y-axis. \code{NULL} by default (0, max_count).
#'
#' @return If \code{make_gg = FALSE}, the plot is created. Otherwise, an
#'         an instance of \code{ggplot} is generated, printed and returned.
#'
#' @export
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot geom_line aes_string sec_axis scale_y_continuous
#' @importFrom ggplot2 ylab ylim
#' @importFrom rlang .data
#'
plot.SimulInactivation <- function(x, y=NULL, ...,
                                   make_gg = TRUE, plot_temp = FALSE,
                                   label_y1 = "logN",
                                   label_y2 = "Temperature",
                                   ylims = NULL) {

    # time <- NULL
    # temperature <- NULL

    if (make_gg) {

        if (plot_temp) {

            min_time <- min(x$simulation$time)
            max_time <- max(x$simulation$time)

            max_count <- max(x$simulation$logN, na.rm = TRUE)

            tt <- seq(min_time, max_time, length = 100)
            min_temp <- min(x$temp_approximations$temp(tt))
            max_temp <- max(x$temp_approximations$temp(tt))

            if (max_temp == min_temp) {  # Isothermal profile

                max_temp <- max_temp + 1
                min_temp <- min_temp - 1

            }

            slope <- (max_count)/(max_temp - min_temp)
            intercept <- (min_temp * (max_count-0)/(max_temp - min_temp) - 0)

            p <- x$simulation %>%
                mutate(temperature = x$temp_approximations$temp(.data$time),
                       fake_temp = .data$temperature * slope - intercept) %>%
                ggplot() +
                    geom_line(aes_string(x = "time", y = "logN"), linetype = 2) +
                    geom_line(aes_string(x = "time", y = "fake_temp"))

            if (is.null(ylims)) {
                ylims <- c(0, max_count)
            }

            p + scale_y_continuous(limits = ylims,
                                   name = label_y1,
                                   sec.axis = sec_axis(~(. + intercept)/slope,
                                                       name = label_y2))

        } else {

            p <- ggplot(x$simulation) +
                geom_line(aes_string(x = "time", y = "logN")) +
                ylab(label_y1)

            if (!is.null(ylims)) {
                p <- p + ylim(ylims)
            }

            p

        }

    } else {

        plot(logN ~ time, data = x$simulation, type = "l", ...)
    }
}

#'
#' Plot of IsoFitInactivation Object
#'
#' For each one of the temperatures studied, plots a comparison between the
#' predicted result and the experimental one for an instance of
#' \code{IsoFitInactivation}.
#'
#' @param x the object of class \code{IsoFitInactivation} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param make_gg logical. If \code{TRUE}, the plot is created using
#' \code{ggplot2}. Otherwise, the plot is crated with base \code{R}.
#' \code{FALSE} by default.
#'
#' @export
#'
#' @importFrom graphics plot lines title
#' @importFrom dplyr mutate %>% ungroup
#' @importFrom lazyeval interp
#' @importFrom ggplot2 aes_string ggplot geom_point geom_line facet_wrap
#' @importFrom stats predict
#'
plot.IsoFitInactivation <- function(x, y=NULL, ..., make_gg = FALSE) {

    if (!make_gg) {

        death_data <- x$data
        model_data <- get_isothermal_model_data(x$model)

        for (each_temp in unique(death_data$temp)) {

            temp_indexes <- death_data$temp == each_temp
            my_data <- death_data[temp_indexes, ]

            # my_data <- subset(death_data, temp == each_temp)

            plot(log_diff ~ time, data = my_data, ...)

            max_time <- max(my_data$time)
            times <- seq(0, max_time, length= 100)
            arguments_call <- c(list(time = times, temp = each_temp), x$parameters)

            prediction <- do.call(model_data$prediction, arguments_call)

            lines(times, prediction)
            title(paste("Temperature:", each_temp))

        }

    } else {
        x$data %>%
            ungroup() %>%
            mutate(prediction = predict(x$nls, newdata = x$data)) %>%
            ggplot(aes_string(x = "time")) +
                geom_point(aes_string(y = "log_diff")) +
                geom_line(aes_string(y = "prediction"), linetype = 2) +
                facet_wrap("temp", scales = "free")
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
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param make_gg logical. If \code{TRUE}, the plot is created using
#' \code{ggplot2}. Otherwise, the plot is crated with base \code{R}.
#' \code{TRUE} by default.
#' @param plot_temp logical. Whether the temperature profile will
#' be added to the plot. \code{FALSE} by default.
#' @param label_y1 Label of the principal y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param ylims Numeric vector of length 2 with the Limits of the
#' y-axis. \code{NULL} by default (0, max_count).
#'
#' @return If \code{make_gg = FALSE}, the plot is created. Otherwise, an
#'         an instance of \code{ggplot} is generated, printed and returned.
#'
#' @export
#'
#' @importFrom graphics plot points
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes_string ylim
#'
plot.FitInactivation <- function(x, y=NULL, ..., make_gg = TRUE,
                                 plot_temp = FALSE,
                                 label_y1 = "logN",
                                 label_y2 = "Temperature",
                                 ylims = NULL) {

    death_data <- x$data

    if (!("logN" %in% names(death_data))) {

        death_data$logN <- log10(death_data$N)
    }

    if (make_gg) {

        maxlogN0 <- max(c(x$data$logN,
                          log10(x$best_prediction$model_parameters$N0)))

        if (is.null(ylims)) {
            ylims <- c(0, ceiling(maxlogN0))
        }

        pred_plot <- plot(x$best_prediction, plot_temp = plot_temp,
                          label_y1 = label_y1, label_y2 = label_y2,
                          ylims = ylims)
        p <- pred_plot +
            geom_point(data = death_data, aes_string(x = "time", y = "logN")) # +
            # ylim(0, ceiling(maxlogN0))
        return(p)

    } else {

        #- Find the limits

        min_logN_data <- min(death_data$logN, na.rm = TRUE)
        min_logN_pred <- min(x$best_prediction$simulation$logN, na.rm = TRUE)
        min_logN <- min(c(min_logN_data, min_logN_pred))

        max_logN_data <- max(death_data$logN, na.rm = TRUE)
        max_logN_pred <- max(x$best_prediction$simulation$logN, na.rm = TRUE)
        max_logN <- max(c(max_logN_data, max_logN_pred))

        ylim <- c(floor(min_logN), ceiling(max_logN))

        #- Make the plot

        plot(x$best_prediction, ylim = ylim, make_gg = FALSE, ...)

        points(logN ~ time, data = death_data)
    }

}

#'
#' Plot of FitInactivationMCMC Object
#'
#' Plots a comparison between the experimental data provided and the prediction
#' produced by the model parameters adjusted for an instance of
#' \code{FitInactivationMCMC}.
#'
#' @param x the object of class \code{FitInactivation} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#'
#' @param make_gg logical. If \code{TRUE}, the plot is created using
#' \code{ggplot2}. Otherwise, the plot is crated with base \code{R}.
#' \code{TRUE} by default.
#' @param plot_temp logical. Whether the temperature profile will
#' be added to the plot. \code{FALSE} by default.
#' @param label_y1 Label of the principal y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param ylims Numeric vector of length 2 with the Limits of the
#' y-axis. \code{NULL} by default (0, max_count).
#'
#' @return If \code{make_gg = FALSE}, the plot is created. Otherwise, an
#'         an instance of \code{ggplot} is generated, printed and returned.
#'
#' @export
#'
plot.FitInactivationMCMC <- function(x, y=NULL, ..., make_gg = TRUE,
                                     plot_temp = FALSE,
                                     label_y1 = "logN",
                                     label_y2 = "Temperature",
                                     ylims = NULL) {

    plot.FitInactivation(x, make_gg = make_gg, plot_temp = plot_temp,
                         label_y1 = label_y1,
                         label_y2 = label_y2,
                         ylims = ylims,
                         ...)

}

#' Plot of PredInactivationMCMC Object
#'
#' Plots the prediction interval generated by
#' \code{\link{predict_inactivation_MCMC}}.
#'
#' The plot generated in ggplot (default) generates a dashed line with the mean
#' of the MC
#' simulations. Moreover, a ribbon with the 2 first quantiles (i.e. columns 3
#' and 4) is generated.
#'
#' The plot generated with base R (make_gg = \code{FALSE}) generates a solid line
#' wth the mean of the MC simulations. Each one of the other quantiles included
#' in the data frame are added with different
#'
#' @importFrom ggplot2 ggplot aes_string
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon ylab xlab
#'
#' @param x the object of class \code{PredInactivationMCMC} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param make_gg logical. If \code{TRUE}, the plot is created using
#' \code{ggplot2}. Otherwise, the plot is crated with base \code{R}.
#' \code{TRUE} by default.
#'
#' @return If \code{make_gg = FALSE}, the plot is created. Otherwise, an
#'         an instance of \code{ggplot} is generated, printed and returned.
#'
#' @importFrom graphics legend
#'
#' @export
#'
plot.PredInactivationMCMC <- function(x, y=NULL, ..., make_gg = TRUE) {

    if (make_gg) {  # with ggplot 2

        if (!names(x)[2] == "mean"){
            stop("ggplot plots not available for PredInactivationMCMC objects without
                 quantiles calculated. Generate base plot instead.")
        }

        x$mean <- log10(x[ , 2])
        x$median <- log10(x[ , 3])
        x$lower <- log10(x[ , 4])
        x$upper <- log10(x[ , 5])

        p <- ggplot(x, aes_string(x = "times")) +
            geom_ribbon(aes_string(ymax = "upper", ymin = "lower"), alpha = 0.6, fill = "#56B4E9") +
            geom_line(aes_string(y = "mean"), linetype = 2, colour = "darkblue") +
            geom_line(aes_string(y = "median"), linetype = 3, colour = "darkblue") +
            ylab("logN") + xlab("time")
        return(p)

    } else {

        max_N <- max(x[1, 2:ncol(x)])
        min_N <- min(x[nrow(x), 2:ncol(x)])
        y_lim <- c(floor(log10(min_N)), ceiling(log10(max_N)))

        if ("mean" %in% names(x)) {

            plot(log10(mean) ~ times, data = x, type = 'l',
                 ylab = "logN", ylim = y_lim, ...)

        } else {
            plot(x$times, log10(x[ , 2]), type = "l", ...)
        }

        for (i in 3:ncol(x)) {
            lines(x$times, log10(x[ , i]), type = 'l', lty = i-1, col = i-1)
        }

        if ("mean" %in% names(x)) {

            legend("topright", names(x[-1]), lty = 1:(ncol(x)-1), col = 1:(ncol(x)-1))

        }
    }
}




