
#'
#' First Derivate of the Bigelow Model with Full Derivation.
#'
#' Calculates the first derivative of the Bigelow model. The function is
#' compatible with the ode function of the library deSolve.
#'
#' @section Model Equation:
#' N = N0 * 10^(-t/D_T)
#' D_T = D_R * 10^( (T_ref - T)/z )
#'
#' dN = - N * ln(10) * (1/D_T + t*dD_T/DT * dT/dt )
#'    = - N * ln(10) / D_T * (1 + ln(10)*t/z * dT/dt)
#'
#' @param t numeric vector indicating the time of the experiment.
#' @param x list with the value of N at t.
#' @param parms parameters for the secondary model. No explicit check of their
#'        validity is performed (see section \bold{Model Parameters}).
#' @param temp_profile a function that provides the temperature at a given time.
#' @param dtemp_profile a function that provides the derivative of the
#'        temperature at a given time.
#'
#' @return The value of the first derivative of N at time \code{t} as a list.
#'
#' @section Model parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation,
#'          \item D_R: D-value at the reference temperature,
#'          \item z: z value.
#'          }
#'
#'
#'
dBigelow_model <- function(t, x, parms, temp_profile, dtemp_profile)  {

    with(as.list(c(parms, x)), {

        temp <- temp_profile(t)
        dtemp <- dtemp_profile(t)

        D_T <- D_R * 10^( (temp_ref - temp)/z )

        correction <- abs( log(10)/z * t * dtemp + 1 )  # Correction from temp variation

        dN <- - N * log(10)/D_T * correction

        res <- c(dN)
        return(list(res))

    })
}

#'
#' First Derivate of the Linear Bigelow Model.
#'
#' Calculates the first derivative of the Bigelow model. The function is compatible with
#' the ode function of the library deSolve.
#' The equation is developed applying a linearization on D_T at time t.
#'
#' @section Model Equation:
#'    N = N0 * 10^(-t/D_T)
#'    D_T = D_R * 10^( (T_ref - T)/z )
#'
#'    dN = - N * ln(10) /D_T
#'
#' @param t numeric vector indicating the time of the experiment.
#' @param x list with the value of N at t.
#' @param parms parameters for the secondary model. No explicit check of their validity
#'             is performed.
#' @param temp_profile a function that provides the temperature at a given time.
#'
#' @return The value of the first derivative of N at time \code{t} as a list.
#'
#' @section Model parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation,
#'          \item D_R: D-value at the reference temperature,
#'          \item z: z value.
#'          }
#'
dBigelow_model_linear <- function(t, x, parms, temp_profile)  {

    with(as.list(c(parms, x)), {

        temp <- temp_profile(t)

        D_T <- D_R * 10^( (temp_ref - temp)/z )

        dN <- - N * log(10)/D_T

        res <- c(dN)
        return(list(res))

    })
}


