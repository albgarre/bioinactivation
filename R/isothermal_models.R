
#' Isothermal Bigelow Model
#'
#' For some conditions given, returns the predicted logarithmic reduction
#' according to the Bigelow's model.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param z numeric defining the z-value.
#' @param D_ref numeric defining the D-value at the reference temperature.
#' @param temp_ref numeric defining the reference temperature.
#'
#' @return A numeric with the predicted logarithmic reduction (log10(N/N0)).
#'
Bigelow_iso <- function(time, temp, z, D_ref, temp_ref){

    D_T <- D_ref * 10^( (temp_ref - temp)/z )
    log_diff <- -time/D_T
    return(log_diff)
}

#' Isothermal Weibull-Mafart Model
#'
#' For some conditions given, returns the predicted logarithmic reduction
#' according to the Weibull-Mafart model.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param delta_ref numeric defining the delta-value at the reference temperature.
#' @param z numeric defining the z-value.
#' @param p numeric defining shape factor of the Weibull distribution.
#' @param temp_ref numeric indicating the reference temperature.
#'
#' @return A numeric with the predicted logarithmic reduction (log10(N/N0)).
#'
WeibullMafart_iso <- function(time, temp, delta_ref, z, p, temp_ref){

    delta <- delta_ref * 10^(- (temp - temp_ref)/z )
    log_diff <- -(time/delta)^p
    return(log_diff)
}

#' Isothermal Weibull-Pelec Model
#'
#' For some conditions given, returns the predicted logarithmic reduction
#' according to the Weibull-Pelec model.
#'
#' @param time numeric indicating the time at which the prediction is taken.
#' @param temp numeric indicating the temperature of the treatment.
#' @param n numeric defining shape factor of the Weibull distribution.
#' @param k_b numeric indicating the slope of the b~temp line.
#' @param temp_crit numeric with the value of the critical temperature.
#'
#' @return A numeric with the predicted logarithmic reduction (log10(N/N0)).
#'
WeibullPelec_iso <- function(time, temp, n, k_b, temp_crit){

    b <- log( 1 + exp(k_b*(temp - temp_crit)))
    log_diff <- -b * time^n
    return(log_diff)

}

