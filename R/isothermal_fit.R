
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
fit_isothermal_inactivation <- function(model_name, death_data, starting_point, adjust_log, ref_temp) {

    model_data <- get_isothermal_model_data(model_name)

    if (is.null(model_data)) {
        stop(paste("Unknown model:", model_data))
    }

    if (adjust_log) {

        my_formula = as.formula( paste("log_diff ~", model_data$formula_iso) )

    } else {
        death_data <- mutate(death_data, S = 10^log_diff)
        my_formula = as.formula( paste("S ~10^", model_data$formula_iso) )

    }

    adjust.results <- nls(my_formula,
                          data = death_data,
                          start = starting_point)

    return(adjust.results)
}

#' Isothermal Model Data
#'
#' Provides information of the models implemented for fitting of isothermal data.
#'
#' @param model_name Optional string with the key of the model to use.
#' @return If \code{model_name} is missing, a list of the valid model keys.
#'         If \code{model_name} is not a valid key, NULL is returned.
#'         Otherwise, a list with the parameters of the model selected and its
#'         \code{formula} for the nonlinear adjustment.
#'
#' @export
#'
get_isothermal_model_data <- function(model_name = "valids") {

    switch(as.character(model_name),
           Weibull_Mafart = list(params = c("delta_ref", "z", "p", "temp_ref"),
                                 formula_iso = "WeibullMafart_iso(time, temp, delta_ref, z, p, ref_temp)"
                                 ),

           Weibull_Pelec = list(params = c("n", "k_b", "temp_crit"),
                                formula_iso = "WeibullPelec_iso(time, temp, n, k_b, temp_crit)"
                                ),

           Bigelow = list(params = c("z", "D_ref", "ref_temp"),
                        formula_iso = "Bigelow_iso(time, temp, z, D_ref, ref_temp)"
                        ),
           valids = c("Weibull_Mafart", "Weibull_Pelec", "Bigelow")
           )

}
