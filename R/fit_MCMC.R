
#' Fitting of dynamic inactivation with MCMC
#'
#' Fits the parameters of an inactivation model to experimental by performing
#' a Markov Chain Monte Carlos simulation with an adaptive Metropolis
#' algorithm including a delayed rejection procedure.
#' The function \code{\link{modMCMC}} of the package \code{\link{FME}} is
#' used for the adjustment.
#'
#' @param experiment_data data frame with the experimental data to be adjusted.
#'        It must have a column named \dQuote{time} and another one named
#'        \dQuote{N}.
#' @param simulation_model character identifying the model to be used.
#' @param temp_profile data frame with discrete values of the temperature for
#'        each time.
#' @param starting_points starting values of the parameters for the adjustment.
#' @param upper_bounds named numerical vector defining the upper bounds of the
#'        parameters for the adjustment.
#' @param lower_bounds named numerical vector defining the lower bounds of the
#'        parameters for the adjustment.
#' @param known_params named numerical vector with the fixed (i.e., not
#'        adjustable) model parameters.
#' @param minimize_log logical. If \code{TRUE}, the adjustment is based on the
#'        minimization of the error of the logarithmic count.
#' @param ... other arguments for \code{\link{modMCMC}}.
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr %>%
#' @importFrom dplyr select_
#' @importFrom FME modMCMC
#' @importFrom lazyeval interp
#'
#' @return A list of class \code{FitInactivationMCMC} with the following items:
#'      \itemize{
#'          \item modMCMC: a list of class \code{modMCMC}.
#'          \item best_prediction: a list of class \code{SimulInactivation}
#'                with the results.
#'          \item data: a data frame with the data used for the fitting.
#'          }
#'
#' @export
#'
fit_inactivation_MCMC <- function(experiment_data, simulation_model, temp_profile,
                                  starting_points, upper_bounds, lower_bounds,
                                  known_params, minimize_log,
                                  ...) {

    #- Gather the information

    model_data <- get_model_data(simulation_model)

    if (minimize_log) {

        data_for_fit <- mutate_(experiment_data,
                                logN = interp(~log10(N), N=as.name("N")))

        data_for_fit <- select_(data_for_fit, as.name("time"), as.name("logN"))

    } else {
        data_for_fit <- select_(experiment_data, as.name("time"), as.name("N"))
    }

    #- Call the fitting function

    Fit <- modMCMC(f = get_prediction_cost, p = unlist(starting_points),
                  temp_profile = temp_profile,
                  data_for_fit = data_for_fit,
                  lower = unlist(lower_bounds), upper = unlist(upper_bounds),
                  known_params = known_params,
                  simulation_model = simulation_model,
                  ...)

    #- Prepare the output

    prediction_time <- seq(min(data_for_fit$time), max(data_for_fit$time), length=100)

    best_prediction <- predict_inactivation(simulation_model,
                                            prediction_time,
                                            as.list(c(Fit$bestpar, known_params)),
                                            temp_profile
                                            )

    out <- list(modMCMC = Fit,
                best_prediction = best_prediction,
                data = data_for_fit)

    class(out) <- c("FitInactivationMCMC", class(out))

    return(out)

}















