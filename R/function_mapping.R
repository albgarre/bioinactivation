
#' Mapping of Simulation Model Functions
#'
#' Given a valid simulation_model key, provides the data of the functions
#' associated to it.
#'
#' @param simulation_model character with a valid model key or NULL
#'
#' @return If simulation_model is NULL or missing, a character vector of
#'      possible names. If not, a list including all the data of the function:
#'      \itemize{
#'          \item ode: Pointer to the function defining the model ode,
#'          \item cost: Pointer to the function calculating the error of the
#'                approximation,
#'          \item dtemp: Whether the function requires the definition of dtemp,
#'          \item variable: a character vector defining which entry variables
#'                needed by the model,
#'          \item parameters: character vector with the parameters needed by
#'                            the model.
#'          }
#'
#' @export
#'
get_model_data <- function(simulation_model = NULL) {

    function_map <- list(Bigelow = list(ode = dBigelow_model,
                                        dtemp = TRUE,
                                        variable = "N",
                                        parameters = c("D_R", "z", "temp_ref")
                                        ),

                         Bigelow_linearized = list(ode = dBigelow_model_linear,
                                                   dtemp = FALSE,
                                                   variable = "N",
                                                   parameters = c("D_R", "z", "temp_ref")
                                                   ),

                         Weibull_Mafart = list(ode = dMafart_model,
                                               dtemp = FALSE,
                                               variable = "N",
                                               parameters = c("delta_ref", "temp_ref", "z", "p")
                                               ),

                         Weibull_Mafart_full = list(ode = dMafart_model_full,
                                                    dtemp = TRUE,
                                                    variable = "N",
                                                    parameters = c("delta_ref", "temp_ref", "z", "p")
                                                    ),

                         Geeraerd = list(ode = dGeeraerd_model,
                                         dtemp = FALSE,
                                         variable = c("N", "C_c"),
                                         parameters = c("D_R", "z", "N_min")
                                         ),

                         Weibull_Peleg = list(ode = dPeleg_model,
                                              dtemp = FALSE,
                                              variable = "logS",
                                              parameters = c("k_b", "temp_crit", "n")
                                              ),

                         Weibull_Peleg_full = list(ode = dPeleg_model_full,
                                                   dtemp = TRUE,
                                                   variable = "N",
                                                   parameters = c("k_b", "temp_crit", "n")
                                                   )
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
