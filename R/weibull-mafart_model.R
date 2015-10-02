
## FUNCTIONS FOR SOLUTION OF THE ODE ------------------------------------------

#'
#' First Derivate of the Mafart Model
#'
#' Calculates the first derivative of the Mafart model. The function is compatible with
#' the ode function of the library deSolve.
#'
#' @section Model Equation:
#'      dN/dt = -N * p * (1/delta)^p * t^(p-1)
#'      delta = delta_ref * 10^(- (T-T_ref)/z )
#'
#' @param t time.
#' @param x list with the value of N at t.
#' @param parms parameters for the secondary model. No explicit check of their validity
#'             is performed (see section \bold{Model Parameters}).
#' @param temp_profile a function that provides the temperature at a given time.
#'
#' @return The value of the first derivative of N at time \code{t} as a list.
#'
#' @section Model Parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation.
#'          \item delta_ref: Value of the scale factor at the reference temperature.
#'          \item z: z-value.
#'          \item p: shape factor of the Weibull distribution.
#'          }
#'
#' @section Note:
#'      For t=0, dN = 0 unless n=1. Hence, a small shift needs to be introduced
#'      to t.
#'
dMafart_model<- function(t, x, parms, temp_profile)  {

    temp <- temp_profile(t)

    with(as.list(c(x, parms)),{
        delta <- delta_ref * 10^( -(temp-temp_ref)/z)
        dN <- - N * p * (1/delta)^p * t^(p-1)

        res <- c(dN)
        return(list(res))
    })

}

#'
#' First Derivate of the Mafart Model with Full Derivation
#'
#' Calculates the first derivative of the Mafart model developed with full
#' derivation. The function is compatible with the ode function of the library
#' deSolve.
#'
#' @section Model Equation:
#'      dN/dt = -N * p * (t/delta)^(p-1) * log(10)/delta*( 1+log(10)*t/z*dtemp )
#'      delta = delta_ref * 10^(- (T-T_ref)/z )
#'
#' @param t time.
#' @param x list with the value of N at t.
#' @param parms parameters for the secondary model. No explicit check of their validity
#'             is performed (see section \bold{Model Parameters}).
#' @param temp_profile a function that provides the temperature at a given time.
#' @param dtemp_profile a function that provides the first derivative of the
#'        temperature at a given time.
#'
#' @return The value of the first derivative of N at time \code{t} as a list.
#'
#' @section Model Parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation.
#'          \item delta_ref: Value of the scale factor at the reference temperature.
#'          \item z: z-value.
#'          \item p: shape factor of the Weibull distribution.
#'          }
#'
#' @section Note:
#'      For t=0, dN = 0 unless n=1. Hence, a small shift needs to be introduced
#'      to t.
#'
dMafart_model_full<- function(t, x, parms, temp_profile, dtemp_profile)  {

    temp <- temp_profile(t)
    dtemp <- dtemp_profile(t)

    with(as.list(c(x, parms)),{
        delta <- delta_ref * 10^( -(temp-temp_ref)/z)
        dN <- -N * p * (t/delta)^(p-1) * log(10)/delta*abs( 1+log(10)*t/z*dtemp )
        # dN <- -N * p * (t/delta)^(p-1) * log(10)/delta*( 1+log(10)*t/z*abs(dtemp) )

        res <- c(dN)
        return(list(res))
    })

}

