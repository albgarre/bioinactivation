
#'
#' First Derivate of the Geeraerd Model
#'
#' Calculates the first derivative of the Geeraerd model. The function is compatible with
#' the ode function of the library deSolve.
#'
#' @section Model Equation:
#'      dN/dt = - N * k_max * (1/(1+C_c))
#'      dC_c/dt = -k_max * C_c
#'      k_max = ln(10)/D_ref * exp(ln(10)/z * (T - T_ref))
#'
#' @param t numeric vector indicating the time of the experiment.
#' @param x list with two entries: N and C_c.
#' @param parms parameters for the secondary model. No explicit check of their validity
#'             is performed (see section \bold{Model Parameters}).
#' @param temp_profile a function that provides the temperature at a given time.
#'
#' @return A list with value of the first derivatives of N and C_c at time \code{t}.
#'
#' @section Model Parameters:
#'      \itemize{
#'          \item temp_ref: Reference temperature for the calculation,
#'          \item D_R: D-value at the reference temperature,
#'          \item z: z value,
#'          \item N_min: Minimum value of N (defines the tail).
#'          }
#'
#' @section Notes:
#'      To define the Geeraerd model without tail, assign \code{N_min = 0}.
#'      For the model without shoulder, assign \code{C_0 = 0}
#'
dGeeraerd_model<- function(t, x, parms, temp_profile)  {

    temp <- temp_profile(t)

    with(as.list(c(x, parms)),{

        k_max <- log(10)/D_R * exp(log(10)/z * (temp - temp_ref))
        dC_c <- - k_max * C_c
        dN <- - N * k_max * (1/(1+C_c)) * (1 - N_min/N)

        res <- c(dN, dC_c)
        return(list(res))
    })

}

