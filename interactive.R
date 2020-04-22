source(here::here("global.R"))

# number of observatiosn
n <- 1e5

# probabilities of features
x <- c(0.8, 0.4)
x <- c(0.8)

# betas
b0 <- log(0.05) # 
b <- c(log(2), log(0.5))
b <- log(1)

z <- b0
pr <- 1/(1+exp(-z)) # pass through an inv-logit function
DescTools::PercTable(rbinom(nrow(ds), 1, pr)) # bernoulli response variable

x <- log(seq(0,2,0.001))
plot(exp(x), 1/(1+exp(-x)), type = "l")
lines(x, x, color = "red")

  sim_test(ds, b0)

ds <- sim_ds(n = n, x = x, b0 = b0, b = b)

DescTools::PercTable(y ~ x1, data = ds, rfrq = "011", margins = c(1,2))

DescTools::Desc(y ~ x1, data = ds)



# Table 1 -----------------------------------------------------------------

# count data

# TOne
ds2 <- 
  ds %>% 
  mutate_all(as.factor) %>% 
  data.frame()

DescTools::TOne(x = ds2[, -3], grp = ds2[, 3])

# replicate TOne
tbl1_comb_raw <- tally_discrete(ds)

plot_prev(tbl1_comb_raw)


# Univariable analysis ----------------------------------------------------

# what is P(y = 1 | x1)
ds %>% 
  count(y, x1) %>% 
  group_by(y) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(x1 == 1)

ind_int <- ds %>% select(-y) %>% map_lgl(is.integer) %>% which()
ind_fct <- ds %>% select(-y) %>% map_lgl(is.factor) %>% which()
ind_chr <- ds %>% select(-y) %>% map_lgl(is.character) %>% which()

table(ds$y)
table(ds$x1)

DescTools::Desc(y ~ x1, data = ds)

# prevalence of predictors conditional on outcome

tbl_cond <- tally_discrete_cond(ds)

tbl_cond %>% 
  ggplot(aes(x = factor(val), y = pct, color = factor(val))) + 
  geom_linerange(aes(xmin = factor(val), ymin = 0, xmax = factor(val), ymax = pct), 
                 color = "grey50") +
  geom_point(aes(size = n)) +
  scale_y_continuous(labels = scales::percent) +
  scale_size(guide = NULL) +
  facet_grid(rows = vars(var)) + 
  coord_flip() + 
  expand_limits(y = 1) + 
  theme_bw() +
  ggtitle(label = "Conditional prevalence", subtitle = "Prevalence of outcome, conditional on covariate")

plot_prev_cond(tbl_cond)
  

# misc --------------------------------------------------------------------

# what is P(y)? defined by b0
ds %>% 
  count(y) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(y == 1)
  
# what is P(x1)? defined by x
ds %>% 
  count(x1) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(x1 == 1)

# what is P(x2)? defined by x
ds %>% 
  count(x2) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(x2 == 1)

# what is P(x1 = 1 | y)
ds %>% 
  count(y, x1) %>% 
  group_by(y) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(x1 == 1)

# what is P(x2 = 1 | y)
ds %>% 
  count(y, x2) %>% 
  group_by(y) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(x2 == 1)


# what is P(y = 1 | x1)
ds %>% 
  count(y, x1) %>% 
  group_by(y) %>% 
  mutate(nn = sum(n), pct = n/nn) %>% 
  filter(x1 == 1)

mod <- glm( y~., data=ds, family="binomial")
mod_or <- calc_or(mod = mod)
mod_ame <- margins(mod)

plot_or(mod_or)
plot_ame(mod_ame)

# prepare project ---------------------------------------------------------

use_description()
use_mit_license("Beat Brüngger")
usethis::use_git_remote(name = "origin", url = "https://github.com/bebru/oddlyrisky.git", overwrite = TRUE) 
