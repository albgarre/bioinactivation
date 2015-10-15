
#'
#' Test of SimulInactivation
#'
#' Tests if an object is of class SimulInactivation.
#'
#' @param x Object to be checked.
#'
#' @return a boolean specifying whether \code{x} is of class SimulInactivation
#'
#' @export
#'
is.SimulInactivation <- function(x) inherits(x, "SimulInactivation")

#'
#' Plot of SimulInactivation Object
#'
#' Plots the predicted evolution of the logarithmic count with time.
#'
#' @param obj The object of class \code{SimulInactivation} to plot.
#'
#' @export
#'
plot.SimulInactivation <- function(obj, y=NULL, ...) {

    plot(logN ~ time, data = obj$simulation, type = "l")

}

