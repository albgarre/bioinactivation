
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
#' @param obj The object of class \code{SimulInactivation} to plot.
#'
#' @export
#'
plot.SimulInactivation <- function(obj, y=NULL, ...) {

    plot(logN ~ time, data = obj$simulation, type = "l")

}

#'
#' Plot of IsoFitInactivation Object
#'
#' For each one of the temperatures studied, plots a comparison between the
#' predicted result and the experimental one for an instance of
#' \code{IsoFitInactivation}.
#'
#' @param obj the object of class \code{IsoFitInactivation} to plot.
#'
#' @export
#'
plot.IsoFitInactivation <- function(obj, y=NULL, ...) {

    death_data <- obj$data
    model_data <- get_isothermal_model_data(obj$model)

    for (each_temp in unique(death_data$temp)) {

        my_data <- subset(death_data, temp == each_temp)

        plot(log_diff ~ time, data = my_data)

        max_time <- max(my_data$time)
        times <- seq(0, max_time, length= 100)
        arguments_call <- c(list(time = times, temp = each_temp), obj$parameters)

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
#' @param obj the object of class \code{FitInactivation} to plot.
#'
#' @export
#'
plot.FitInactivation <- function(obj, y=NULL, ...) {

    plot(obj$best_prediction)

    death_data <- obj$data

    if (!("logN" %in% names(death_data))) {

        death_data$logN <- log10(death_data$N)
    }

    points(logN ~ time, data = death_data)

}

