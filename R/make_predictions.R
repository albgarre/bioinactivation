
#' Prediction of Dynamic Microbial Inactivation
#'
#' Predicts the inactivation of a microorganism for dynamic temperature
#' conditions. The characteristics of the microorganism are defined by
#' the model parameters defined.
#'
#' The value of the temperature is calculated at each value of time by
#' linear interpolation of the values provided by the input argument
#' \code{temp_profile}.
#' The function \code{\link{ode}} of the package \code{\link{deSolve}} is
#' used for the resolution of the differential equation.
#'
#' @param simulation_model character identifying the model to be used.
#' @param times numeric vector of output times.
#' @param parms list of parameters defining the parameters of the model.
#' @param temp_profile data frame with discrete values of the temperature for
#'        each time.
#'
#' @importFrom deSolve ode
#' @importFrom dplyr mutate_
#' @importFrom lazyeval interp
#'
#' @return A list of class \code{SimulInactivation} with the results. It has
#'         the following entries:
#'         \itemize{
#'           \item model: character defining the model use for the prediction.
#'           \item model_parameters: named numeric vector with the values of
#'                                   the model parameters used.
#'           \item temp_approximation: function used for the interpolation of
#'                                     the temperature. For a numeric value of
#'                                     time given, returns the value of the
#'                                     temperature.
#'           \item simulation: A data frame with the results calculated. It
#'                             includes 4 columns: time, logN, N, logS and S.
#'           }
#'
#'
#' @export
#'
#' @seealso \code{\link{ode}}
#'
predict_inactivation <- function(simulation_model, times, parms, temp_profile){

    #- Gather the information

    model_data <- get_model_data(simulation_model)
    temp_approximations <- build_temperature_interpolator(temp_profile)

    #- Build the initial value

    if (all(model_data$variable == "N")) {

        xstart <- c(N = parms[["N0"]])

    } else if (all(model_data$variable == c("N", "C_c"))) {


        xstart <- c(N = parms[["N0"]], C_c = parms[["C_c0"]])

    } else if (all(model_data$variable == "logS")) {

        xstart <- c(logS = -1e-6)
    }

    #- Call the ode function

    if (model_data$dtemp) {

        out <- ode(y = xstart, times = times,
                   func = model_data$ode, parms = parms,
                   temp_profile = temp_approximations$temp,
                   dtemp_profile = temp_approximations$dtemp)

    } else {

        out <- ode(y = xstart, times = times,
                   func = model_data$ode, parms = parms,
                   temp_profile = temp_approximations$temp)

    }

    #- Prepare result for output

    out_value <- list()
    out_value$model <- simulation_model
    out_value$model_parameters <- parms
    out_value$temp_approximations <- temp_approximations
    out <- as.data.frame(out)

    if ("N" %in% names(out)) {

        out <- mutate_(out,
                       logN = interp(~log10(N), N=as.name("N")),
                       S = interp(~N/parms[["N0"]], N=as.name("N")))

        out <- mutate_(out, logS = interp(~log10(S), S=as.name("S")))

    } else {

        out <- mutate_(out,
                       S = interp(~10^logS, logS=as.name("logS")),
                       N = interp(~S*parms[["N0"]], S=as.name("S")))

        out <- mutate_(out, logN = interp(~log10(N), N=as.name("N")))

    }

    out_value$simulation <- out

    class(out_value) <- c("SimulInactivation", class(out_value))

    return(out_value)

}



