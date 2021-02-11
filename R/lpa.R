
#' Title
#'
#' @param N0
#' @param b
#' @param c_el
#' @param c_ea
#' @param c_pa
#' @param mu_l
#' @param mu_a
#' @param V
#'
#' @return
#' @export
#'
#' @example
#' # Parameters of control replicate A from Desharnais and Costantino (1980)
#' LPA_deter(b = 11.67, c_el = 0.0093, c_ea = 0.0110, c_pa = 0.0178, mu_l = 0.5129, mu_a = 0.1108, V = 1)
#' iterate(parms = controlA, N0 = c(larvae = 75, pupae = 35, adults = 60),
#' popfun = LPA_deter)
LPA_deter <- function(N0 = c(larvae = 75, pupae = 35, adults = 60),
                b, c_el, c_ea, c_pa, mu_l, mu_a, V){
  N1 <- N0
  N1[1] <- b * N0[3] * exp(-c_ea*N0[3]/V - c_el*N0[1])
  N1[2] <- (1 - mu_l)*N0[1]
  N1[3] <- N0[2] * exp(-c_pa*N0[3]/V)+(1-mu_a)*N0[3]
  return(N1)
}
