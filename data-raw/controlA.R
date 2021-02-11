## code to prepare `controlA` dataset goes here

controlA <- data.frame(
  t = 0:19,
  b = 11.67,
  c_el = 0.0093,
  c_ea = 0.0110,
  c_pa = 0.0178,
  mu_l = 0.5129,
  mu_a = 0.1108,
  V = 1
)
usethis::use_data(controlA, overwrite = TRUE)
