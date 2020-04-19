
# create tibble with features/independent variables
sim_feat <- function(n, x) {
  set.seed(1)
  ds <- tibble(.rows = n)
  for (i in seq_along(x)) {
    ds <- bind_cols(ds, "x{i}" := rbinom(n, 1, x[i]))
  }
  return(ds)
}

# create vector with dependent variable
sim_outc <- function(.data, b0, b) {
  # set.seed(1) # ergibt immer dasselbe Resultat, nicht hier verwenden
  z <- b0 + as.matrix(.data) %*% matrix(b, nrow = length(b))
  pr <- 1/(1+exp(-z)) # pass through an inv-logit function
  y <- rbinom(nrow(.data), 1, pr) # bernoulli response variable
  return(y)
}

# create vector with variable output: independent variable (y), probability of independent variable (pr), link function
sim_test <- function(.data, b0, b) {
  z <- b0 + as.matrix(.data) %*% matrix(b, nrow = length(b))
  pr <- 1/(1+exp(-z)) # pass through an inv-logit function
  y <- rbinom(nrow(.data), 1, pr) # bernoulli response variable
  return(pr)
}

sim_ds <- function(n, x, b0, b) {
  ind <- sim_feat(n, x)
  ds <- 
    ind %>% 
    mutate(y = sim_outc(., b0 = b0, b = b))
}


# calc OR estimates with Wald intervals
calc_or <- function(mod) {
  cbind(tidy(mod, exponentiate = TRUE), exp(confint_tidy(mod, func = stats::confint.default)))
}

# plot odds ratio estimates
plot_or <- function(or_est) {
  or_est %>% 
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, color = "red") +
    coord_flip() + 
    theme_bw() 
}

# plot average margianl effects
plot_ame <- function(ame_est) {
  ame_est %>% 
    summary() %>% 
    ggplot(aes(x = factor, y = AME, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, color = "red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_flip() + 
    theme_bw()
}
