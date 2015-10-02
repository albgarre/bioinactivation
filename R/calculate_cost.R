
#' Error of the Prediction of Microbial Inactivation
#'
#' Calculates the error of the prediction of microbial inactivation for
#' the chosen inactivation model and the given parameters with respect to
#' the experimental data provided.
#'
#' @param data_for_fit \code{data.frame} with the experimental data to fit. It
#'        must contain a column named "time" and another named "N".
#' @param temp_profile \code{data.frame} defining the temperature profile. It
#'        must have a column named "time" and another named "temperature".
#' @param simulation_model character key defining the inactivation model.
#' @param P,fixed_parameters list with the parameters of the model. Divided in
#'        two variables to be adjusted by FME::modFit
#'
#' @importFrom FME modCost
#'
#' @return an instance of \code{modCost}.
#'
get_prediction_cost <- function(data_for_fit, temp_profile,
                                simulation_model,
                                P, fixed_parameters
                                ) {

    prediction_data <- predict_inactivation(simulation_model = simulation_model,
                                            times = unique(data_for_fit$time),
                                            parms = as.list(c(P, fixed_parameters)),
                                            temp_profile = temp_profile
                                            )

    prediction <- prediction_data$simulation
    prediction <- prediction[names(data_for_fit)]  # Take only the relevant columns
    prediction <- prediction[complete.cases(prediction),]  # NAs produced by negative values in x. This is caused by numerical error when N is very small.

    cost <- modCost(prediction, data_for_fit)
    return(cost)

}
