

#' Title
#'
#' @param N vector of larvae, pupae, adult to draw sample from
#' @param V total volume of habitat in units of 20 g
#' @param n vector of number of samples to take from each stage
#' @param v volume of sample
#' @param replacement (logical) sample with or without replacement
#'
#' @return data frame with a column for each stage sampled, and a row for each sample.
#' @export
#'
#' @examples
#' sample_beetles(c(larvae = 75, pupae = 35, adults = 150), V = 5, n = 5, v = 0.15)
#' beetles <- iterate(parms = controlA, N0 = c(larvae = 75, pupae = 35, adults = 60), popfun = LPA_deter)
#' ## Not run:
#' samples <- purrr::map_dfr(1:20, function(t)sample_beetles(unlist(beetles[t, 9:11]), V = 1, n = c(0,0,5), v = 0.15), .id = "t")
#'
#' ## End(Not run)
sample_beetles <- function(N, V, n, v, replacement = TRUE){
  if (length(n) == 1){
    # assume only adults sampled
    nn = rep(0, length(N))
    nn[length(N)] = n
    n = nn
  }
  if(!replacement) stop("only sampling with replacement implemented")
  samples <- matrix(data = NA, nrow = max(n), ncol = length(N))
  for(i in 1:length(N)){
    if (n[i] > 0) samples[1:n[i],i] <- rbinom(n[i], N[i], v/V)
  }
  results <- data.frame(rep = 1:max(n),
                        samples)
  names(results)[2:ncol(results)] <- names(N)
  results
}
