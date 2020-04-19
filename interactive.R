source(here::here("init.R"))

# number of observatiosn
n <- 1e5

# probabilities of features
x <- c(0.8, 0.4)
x <- c(0.8)

# betas
b0 <- log(0.01) # 
b <- c(log(2), log(1.5))
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

mod <- glm( y~., data=ds, family="binomial")
mod_or <- calc_or(mod = mod)
mod_ame <- margins(mod)

plot_or(mod_or)
plot_ame(mod_ame)

# prepare project ---------------------------------------------------------

use_description()
use_mit_license("Beat Brüngger")
