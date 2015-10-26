
#' Fitting of Dynamic Inactivation Models
#'
#' Fits the parameters of an inactivation model to experimental data.
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
#' @param fixed_parameters named numerical vector with the fixed (i.e., not
#'        adjustable) model parameters.
#' @param minimize_log logical. If \code{TRUE}, the adjustment is based on the
#'        minimization of the error of the logarithmic count.
#'
#' @importFrom deSolve ode
#' @importFrom dplyr mutate_
#' @importFrom dplyr %>%
#' @importFrom dplyr select_
#' @importFrom FME modFit
#' @importFrom lazyeval interp
#'
#' @return A list of class \code{FitInactivation} with the following items:
#'      \itemize{
#'          \item fit_results: a list of class \code{modFit}.
#'          \item best_prediction: a list of class \code{SimulInactivation}
#'                with the results.
#'          \item data: a data frame with the data used for the fitting.
#'          }
#'
#' @export
#'
#' @seealso \code{\link{modFit}}
#'
fit_dynamic_inactivation <- function(experiment_data, simulation_model, temp_profile,
                                     starting_points, upper_bounds, lower_bounds,
                                     fixed_parameters, minimize_log) {

    #- Gather the information

    temp_approximations <- build_temperature_interpolator(temp_profile)
    model_data <- get_model_data(simulation_model)

    if (minimize_log) {

        data_for_fit <- mutate_(experiment_data,
                                logN = interp(~log10(N), N=as.name("N")))

        data_for_fit <- select_(data_for_fit, as.name("time"), as.name("logN"))

    } else {
        data_for_fit <- select_(experiment_data, as.name("time"), as.name("N"))
    }

    #- Call the fitting function

    Fit <- modFit(f = get_prediction_cost, p = starting_points,
                  temp_profile = temp_profile,
                  data_for_fit = data_for_fit,
                  lower = lower_bounds, upper = upper_bounds,
                  fixed_parameters = fixed_parameters,
                  simulation_model = simulation_model
                  )

    #- Output the results

    pars_fit <- Fit$par

    prediction_time <- seq(min(data_for_fit$time), max(data_for_fit$time), length=100)
    best_prediction <- predict_inactivation(simulation_model,
                                            prediction_time,
                                            as.list(c(pars_fit, fixed_parameters)),
                                            temp_profile
                                            )

    out_results <- list(fit_results = Fit,
                        best_prediction = best_prediction,
                        data = data_for_fit
                        )

    class(out_results) <- c("FitInactivation", class(out_results))
    return(out_results)

}





