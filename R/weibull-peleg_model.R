
#'
#' First Derivate of the Peleg Model
#'
#' Calculates the first derivative of the Peleg model. The function is compatible with
#' the ode function of the library deSolve.
#'
#' @section Model Equation:
#'      d(log10(S))/dt = -b * n *( - log10(S)/b )^( (n-1)/n)
#'      b(T) = ln( 1 + exp( k_b*(T - T_crit) ) )
#'
#' @param t numeric vector indicating the time of the experiment.
#' @param x list with the value of logS at t.
#' @param parms parameters for the secondary model. No explicit check of their validity
#'             is performed (see section \bold{Model Parameters}).
#' @param temp_profile a function that provides the temperature at a given time.
#'
#' @return The value of the first derivative of logS at time \code{t} as a list.
#'
#' @section Model Parameters:
#'      \itemize{
#'          \item temp_crit: Temperature below which there is inactivation.
#'          \item k_b: slope of the b ~ temp line for temperatures above the
#'                     critical one.
#'          \item n: shape factor of the Weibull distribution.
#'          }
#'
#' @section Note:
#'      For logS=0, dlogS = 0 unless n=1. Hence, a small shift needs to be introduced
#'      to logS.
#'
#'
dPeleg_model <- function(t, x, parms, temp_profile)  {

    temp <- temp_profile(t)

    with(as.list(c(x, parms)),{

        b <- log( 1 + exp( k_b*(temp - temp_crit) ) )

        dlogS <- -b * n * ( - logS/b ) ^( (n-1)/n)
        res <- c(dlogS)
        return(list(res))
    })

}


#'
#' First Derivate of the Peleg Model with Full Derivation
#'
#' Calculates the first derivative of the Peleg model developed with full derivation.
#' The function is compatible with the ode function of the library deSolve.
#'
#' @section Model Equation:
#'      N = N0 * exp(-b(T)*t^n)
#'      dN/dt = N0 * exp(-b(T)*t^n) * (-b(T)*n*t^(n-1) - t^n * db/dT*dT/dt)
#'            = -N * t^n * (b(T)*n/t + db/dT*dT/dt)
#'
#' @param t numeric vector indicating the time of the experiment.
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
#'          \item temp_crit: Temperature below which there is inactivation.
#'          \item k_b: slope of the b ~ temp line for temperatures above the
#'                     critical one.
#'          \item n: shape factor of the Weibull distribution.
#'          }
#'
#' @section Note:
#'      For t=0, dN = 0 unless n=1. Hence, a small shift needs to be introduced
#'      to t.
#'
#'
dPeleg_model_full<- function(t, x, parms, temp_profile, dtemp_profile)  {

    temp <- temp_profile(t)
    dtemp <- dtemp_profile(t)

    with(as.list(c(x, parms)),{

        A <- exp(k_b*(temp - temp_crit))
        b <- log( 1 + A)
        db <- k_b * A / (1+A)

        dN <- -N * t^n * abs(b*n/t + db*dtemp)
        # dN <- -N * t^n * (b*n/t + db*abs(dtemp))

        res <- c(dN)
        return(list(res))
    })

}

