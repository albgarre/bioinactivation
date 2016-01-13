
#' Dynamic Prediction with Markov Chain Monte Carlo
#'
#' Given an MCMC adjustment of a dynamic microbial inactivation process
#' performed using \code{\link{fit_inactivation_MCMC}} calculates
#' probability intervals at each time point using a Monte Carlo method.
#'
#'
#'
#' @param MCMC_fit An object of class \code{FitInactivationMCMC} as generated
#'        by \code{\link{fit_inactivation_MCMC}}.
#' @param n_simulations a numeric indicating how many Monte Carlo simulations
#'        to perform. \code{100} by default.
#' @param times numeric vector specifying the time points when results are
#'        desired. If \code{NULL}, the times in \code{MCMC_fit$best_prediction}
#'        are used. \code{NULL} by default.
#' @param quantiles numeric vector indicating the quantiles to calculate in
#'        percentage. By default, it is set to c(5, 95) (i.e. 5% and 95%).
#'
#' @return A data frame of class \code{PredictionMCMC}. On its first column,
#'         time at which the calculation has been made is indicated.
#'         The second column provides the mean of all the Monte Carlo
#'         simulations at that time point.
#'         Following columns contain the quantiles of the results.
#'
#' @importFrom dplyr sample_n
#'
#' @export
#'
#' @examples
#' ## EXAMPLE 1 ------
#' data(dynamic_inactivation)  # The example data set is used.
#'
#' simulation_model <- "Peleg"  # Peleg's model will be used
#'
#' model_data <- get_model_data(simulation_model)
#' model_data$parameters  # Set the model parameters
#'
#' dummy_temp <- data.frame(time = c(0, 1.25, 2.25, 4.6),
#'                          temperature = c(70, 105, 105, 70))  # Dummy temp. profile
#'
#' ## Set known parameters and initial points/bounds for unknown ones
#'
#' known_params = c(temp_crit = 100)
#'
#' starting_points <- c(n = 1, k_b = 0.25, N0 = 1e+05)
#' upper_bounds <- c(n = 2, k_b = 1, N0 = 1e6)
#' lower_bounds <- c(n = 0.5, k_b = 0.1, N0 = 1e4)
#'
#' minimize_log = TRUE
#'
#' MCMC_fit <- fit_inactivation_MCMC(dynamic_inactivation, simulation_model,
#'                                   dummy_temp, starting_points,
#'                                   upper_bounds, lower_bounds,
#'                                   known_params, niter = 500)
#'
#' MCMC_prediction <- predict_inactivation_MCMC(MCMC_fit)
#' ## END EXAMPLE 1 -----
#'
predict_inactivation_MCMC <- function(MCMC_fit, n_simulations = 100,
                                      times = NULL, quantiles = c(5, 95)) {

    ## Extract fitted parameters and identify known pars

    pars <- as.data.frame(MCMC_fit$modMCMC$pars)
    fit_pars <- names(pars)

    best_pars <- MCMC_fit$best_prediction$model_parameters
    good_indexes <- !(names(best_pars) %in% fit_pars)
    known_pars <- unlist(best_pars[good_indexes])

    ## Extrat the rest of the model data

    simulation_model <- MCMC_fit$best_prediction$model

    if (is.null(times)) {
        times <- MCMC_fit$best_prediction$simulation$time
    }

    temp_values <- MCMC_fit$best_prediction$temp_approximations$temp(times)
    temp_profile <- data.frame(time = times,
                               temperature = temp_values)

    ## Sample from pars and make the simulations

    par_sample <- sample_n(pars, n_simulations, replace = TRUE)
    result_matrix <- matrix(nrow = length(times), ncol = n_simulations)

    for (i in 1:n_simulations){
        this_pars <- unlist(par_sample[i, ])
        this_prediction <- predict_inactivation(simulation_model,
                                                times,
                                                c(this_pars, known_pars),
                                                temp_profile
                                                )
        result_matrix[, i] <- this_prediction$simulation$N
    }

    ## Take means and quantiles from the results

    quantile_matrix <- apply(result_matrix, 1, quantile, probs = quantiles/100)
    mean_matrix <- rowMeans(result_matrix)
    out_matrix <- as.data.frame(cbind(times, mean = mean_matrix, t(quantile_matrix)))

    class(out_matrix) <- c("PredictionMCMC", class(out_matrix))
    return(out_matrix)
}













