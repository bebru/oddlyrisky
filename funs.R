
# simulation --------------------------------------------------------------

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
sim_outc <- function(dta, b0, b) {
  # set.seed(1) # ergibt immer dasselbe Resultat, nicht hier verwenden
  z <- b0 + as.matrix(dta) %*% matrix(b, nrow = length(b))
  pr <- 1/(1+exp(-z)) # pass through an inv-logit function
  y <- rbinom(nrow(dta), 1, pr) # bernoulli response variable
  return(y)
}

# create vector with variable output: independent variable (y), probability of independent variable (pr), link function
sim_test <- function(dta, b0, b) {
  z <- b0 + as.matrix(dta) %*% matrix(b, nrow = length(b))
  pr <- 1/(1+exp(-z)) # pass through an inv-logit function
  y <- rbinom(nrow(dta), 1, pr) # bernoulli response variable
  return(pr)
}

sim_ds <- function(n, x, b0, b) {
  ind <- sim_feat(n, x)
  ds <- 
    ind %>% 
    mutate(y = sim_outc(., b0 = b0, b = b))
}

# description -------------------------------------------------------------

# raw prevalence of covariates and outcome

# tally all discrete variables, covariates and outcome
# TODO: make outcome variable a parameter
# TODO: make filtering out value 0 (or other) optional
tally_discrete <- function(dta) {
  
  # discrete variables to tally
  ind_int <- dta %>% select(-y) %>% map_lgl(is.integer) %>% which()
  ind_fct <- dta %>% select(-y) %>% map_lgl(is.factor) %>% which()
  ind_chr <- dta %>% select(-y) %>% map_lgl(is.character) %>% which()
  
  nogrp_raw <-
    dta %>% 
    mutate(dummy = 1) %>% 
    select(dummy, y, ind_int, ind_fct, ind_chr) %>% 
    pivot_longer(names_to = "var", values_to = "val", -dummy) %>% 
    count(var, val) %>% 
    group_by(var) %>% 
    mutate(nn = sum(n), pct = n/nn) %>% 
    ungroup() %>% 
    mutate(grp = factor("no_grp", levels = c("no_grp", levels(factor(dta$y))))) %>% 
    select(grp, everything()) %>% 
    filter(val != 0)
  
  grp_raw <- 
    dta %>%
    mutate(grp = factor(y, levels = c("no_grp", levels(factor(dta$y))))) %>% 
    select(-y) %>% 
    select(grp, ind_int, ind_fct, ind_chr) %>% 
    pivot_longer(names_to = "var", values_to = "val", -grp) %>% 
    count(grp, var, val) %>% 
    group_by(grp, var) %>% 
    mutate(nn = sum(n), pct = n/nn) %>% 
    ungroup() %>% 
    filter(val != 0)
  
  comb_raw <-  
    nogrp_raw %>% 
    bind_rows(grp_raw)
  
  return(comb_raw)
}

# plot prevalences of covariates and outcome as lolliplot graph
plot_prev <- function(dta){
  dta %>% 
    ggplot(aes(x = factor(val), y = pct, color = grp)) + 
    geom_linerange(aes(xmin = factor(val), ymin = 0, xmax = factor(val), ymax = pct, group = grp), 
                   color = "grey50", position = position_dodge(width = 0.3)) +
    geom_point(aes(size = n), position = position_dodge(width = 0.3)) +
    scale_y_continuous(labels = scales::percent) +
    scale_size(guide = NULL) +
    facet_grid(rows = vars(var)) + 
    coord_flip() + 
    expand_limits(y = 1) + 
    theme_bw() +
    ggtitle(label = "Crude prevalence", subtitle = "Outcome and covariates, w/ and w/o grouping per ourcome")
}

# estimation --------------------------------------------------------------

# calc OR estimates with Wald intervals
calc_or <- function(mod) {
  cbind(tidy(mod, exponentiate = TRUE), exp(confint_tidy(mod, func = stats::confint.default)))
}

# plot odds ratio estimates
plot_or <- function(dta) {
  dta %>% 
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, color = "red") +
    coord_flip() + 
    ylim(c(0, 5)) +
    theme_bw() +
    ggtitle(label = "Estimated Odds Ratio")
}

# plot average margianl effects
plot_ame <- function(dta) {
  dta %>% 
    summary() %>% 
    ggplot(aes(x = factor, y = AME, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, color = "red") +
    scale_y_continuous(limits = c(-0.5, 0.5), labels = scales::percent_format(accuracy = 1)) +
    coord_flip() + 
    theme_bw() +
    ggtitle(label = "Average marginal effects")
}

