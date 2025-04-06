
#' Time to reach X log reductions
#' 
#' Calculates the treatment time required to reach a given number of log reductions.
#' 
#' The treatement time is calculated by linear interpolation betweent the two points of
#' the simulation whose logS is closer to n_logs
#' 
#' @param n_logs Numeric of length one indicating the number of log recutions
#' @param my_prediction An object of class SimulInactivation
#' 
#' @export
#' 
#' @importFrom utils head tail
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' 
time_to_logreduction <- function(n_logs, my_prediction) {
    
    if (length(n_logs) > 1) stop("n_logs must have length 1")
    
    if (min(my_prediction$simulation$logS > -n_logs)) return(NA)
    
    if (n_logs <= 0) stop("n_logs must be positive")
    
    first_below <- my_prediction$simulation %>%
        filter(.data$logS < -n_logs) %>%
        head(1)
    
    last_above <- my_prediction$simulation %>%
        filter(.data$logS >= -n_logs) %>%
        tail(1)
    
    s2 <- -first_below$logS
    s1 <- -last_above$logS
    t1 <- last_above$time
    t2 <- first_below$time
    
    ratio <- (t2 - t1)/(s2 - s1)
    t1 + (n_logs - s1)*ratio
    
}
















