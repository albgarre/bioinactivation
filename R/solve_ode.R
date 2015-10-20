
#' Prediction of Dynamic Microbial Inactivation
#'
#' Predicts the inactivation of a microorganism for some given dynamic conditions.
#'
#' @param simulation_model Character identifying the model to be used.
#' @param times Numeric vector of output times.
#' @param parms List of parameters defining the parameters of the model.
#' @param temp_profile data frame with discrete values of the temperature for
#'        each time.
#'
#' @importFrom deSolve ode
#' @importFrom dplyr mutate_
#' @importFrom lazyeval interp
#'
#' @return an instance of class \code{SimulInactivation} with the results.
#'
#' @export
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



