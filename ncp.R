ncp <- function(r, pa, N){
  p_plus = (r * pa) / ((r - 1) * pa + 1)
  pa_new = (p_plus + pa) / 2
  answer = (p_plus - pa) / (sqrt(2 / N) * sqrt(pa_new * (1 - pa_new)))
  return(answer)
}

power <- function(r, pa, N) {
  thresh = 0.05
  computed_ncp = ncp(r, pa, N)
  answer = pnorm(qnorm(thresh / 2) + computed_ncp)
  return(answer)
}