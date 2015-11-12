
#' Mapping of Simulation Model Functions
#'
#' Provides information about the function for dynamic predictions associated
#' to a valid \code{simulation_model} key.
#' If \code{simulation_model} is missing or \code{NULL}, a character vector
#' of valid model keys is provided.
#' This function is designed as an assistant for using the functions
#' \code{\link{predict_inactivation}} and
#' \code{\link{fit_dynamic_inactivation}}.
#' For the adjustment of isothermal experiments with the function
#' \code{\link{fit_isothermal_inactivation}}, use the function
#' \code{\link{get_isothermal_model_data}}.
#'
#'
#' @param simulation_model (optional) character with a valid model key or
#'        \code{NULL}.
#'
#' @return If simulation_model is \code{NULL} or missing, a character vector of
#'      possible names. Otherwise, a list including information of the relevant
#'      function:
#'      \itemize{
#'          \item ode: Pointer to the function defining the model ode.
#'          \item cost: Pointer to the function calculating the error of the
#'                approximation.
#'          \item dtemp: logical defining whether the function requires the
#'                       definition of the first derivative of temperature.
#'          \item variable: a character vector defining which entry variables
#'                          are needed by the model.
#'          \item parameters: character vector with the parameters needed by
#'                            the model.
#'          }
#'
#' @export
#'
#' @seealso \code{\link{predict_inactivation}},
#'          \code{\link{fit_dynamic_inactivation}}
#'
get_model_data <- function(simulation_model = NULL) {

    function_map <- list(Bigelow = list(ode = dBigelow_model,
                                        dtemp = FALSE,
                                        variable = "N",
                                        parameters = c("D_R", "z", "temp_ref")
                                        ),

#                          Bigelow_full = list(ode = dBigelow_model_full,
#                                                    dtemp = TRUE,
#                                                    variable = "N",
#                                                    parameters = c("D_R", "z",
#                                                                   "temp_ref")
#                                                    ),

                         Mafart = list(ode = dMafart_model,
                                             dtemp = FALSE,
                                             variable = "N",
                                             parameters = c("delta_ref",
                                                            "temp_ref", "z",
                                                            "p")
                                             ),

#                          Weibull_Mafart_full = list(ode = dMafart_model_full,
#                                                     dtemp = TRUE,
#                                                     variable = "N",
#                                                     parameters = c("delta_ref",
#                                                                    "temp_ref",
#                                                                    "z", "p")
#                                                     ),

                         Geeraerd = list(ode = dGeeraerd_model,
                                         dtemp = FALSE,
                                         variable = c("N", "C_c"),
                                         parameters = c("D_R", "z", "N_min",
                                                        "temp_ref")
                                         ),

                         Peleg = list(ode = dPeleg_model,
                                            dtemp = FALSE,
                                            variable = "logS",
                                            parameters = c("k_b", "temp_crit",
                                                           "n")
                                            )

#                          Weibull_Peleg_full = list(ode = dPeleg_model_full,
#                                                    dtemp = TRUE,
#                                                    variable = "N",
#                                                    parameters = c("k_b",
#                                                                   "temp_crit",
#                                                                   "n")
#                                                    )
                         )

    if (is.null(simulation_model)) {
        return(names(function_map))

    } else {

        function_data <- function_map[[simulation_model]]

        if (is.null(function_data)) {
            stop(paste("Unknown simulation model", simulation_model))

        } else {
            return(function_data)

        }
    }
}
