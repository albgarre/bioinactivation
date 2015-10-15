
#' Fit of Isothermal Experiments
#'
#' Fits the parameters of the model chosen to the isothermal experiments
#' provided by means of nonlinear regression.
#'
#' @param model_name character specyfing the model to adjust.
#' @param death_data \code{data.frame} with the experiment data. It must have the
#'        following columns:
#'        \itemize{
#'          \item log_diff: Number of logarithmic reductions at each data point.
#'          \item temp: Temperature of the data point.
#'          \item time: time of the data point.
#'          }
#' @param starting_point Named vector with the initial values of the parameters
#'        for the adjustment.
#' @param adjust_log Boolean. If TRUE, the error of logarithmic reductions is
#'        optimized. Otherwise, the error of the total number of microorganism.
#' @param ref_temp Numeric indicating the reference temperature. This value is
#'        not used for the Weibull-Pelec model.
#'
#' @return an instance of class \code{SimulInactivation} with the results.
#'
#' @export
#'
fit_isothermal_inactivation <- function(model_name, death_data, starting_point, adjust_log, ref_temp){

    death_data <- mutate(death_data, S = 10^log_diff)

    if (grepl(model_name, "Weibull-Mafart", ignore.case = TRUE)) {

        if (adjust_log){

            adjust.results <- nls(log_diff ~ WeibullMafart_iso(time, temp, delta_ref, z, p, ref_temp),
                                  data = death_data,
                                  start = starting_point)

        } else {

            adjust.results <- nls(S ~ 10^WeibullMafart_iso(time, temp, delta_ref, z, p, ref_temp),
                                  data = death_data,
                                  start = starting_point)

        }


    } else if (grepl(model_name, "Weibull-Pelec", ignore.case = TRUE)) {

        if (adjust_log){

            adjust.results <- nls(log_diff ~ WeibullPelec_iso(time, temp, n, k_b, temp_crit),
                                  data = death_data,
                                  start = starting_point)

        } else {

            adjust.results <- nls(S ~ 10^WeibullPelec_iso(time, temp, n, k_b, temp_crit),
                                  data = death_data,
                                  start = starting_point)

        }

    } else if (grepl(model_name, "Bigelow", ignore.case = TRUE)) {

        if (adjust_log){

            adjust.results <- nls(log_diff ~ Bigelow_iso(time, temp, z, D_ref, ref_temp),
                                  data = death_data,
                                  start = starting_point)

        } else {

            adjust.results <- nls(S ~ 10^Bigelow_iso(time, temp, z, D_ref, ref_temp),
                                  data = death_data,
                                  start = starting_point)

        }

    }

    return(adjust.results)
}

#' Isothermal Model Keys
#'
#' Provides a list of all the models keys valid for the fitting of isothermal
#' inactivation data.
#'
#' @return a list whose values are the valid model keys.
#'
#' @export
#'
get_isothermal_keys <- function() {

    models_available <- list("Weibull-Mafart",
                             "Weibull-Pelec",
                             "Bigelow"
                             )
    return(models_available)
}
