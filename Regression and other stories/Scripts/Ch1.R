# packages
library(tidyverse)
library(readr)
library(brms)

# Edit ggplot theme 
theme_set(theme_linedraw() +
            theme(panel.grid = element_blank()))

# three challenges of statistical inference: 
## generalizing from sample to population 
## generalizing from control to treatment 
## generalizing from measurements to latent constructs 

hibbs <- readRDS("Regression and other stories/data/hibbs.rda")
glimpse(hibbs) # year, growth, inc.party.vote, inc.party.candidate, other.candidate 

hibbs <- hibbs |> 
  rename(
    vote = inc.party.vote,
    incumbent_candidate = inc.party.candidate,
    challenger = other.candidate
  )

hibbs |> 
  ggplot(aes(x = growth, y = vote, label = year)) +
  geom_hline(yintercept = 50, color = "grey85", size = 1/4) +
  geom_text(size = 3) +
  scale_x_continuous(labels = function(x) str_c(x, "%")) +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  labs(subtitle = "Forecasting the election from the economy",
       x = "Average recent growth in personal income",
       y = "Incumbent party's vote share")

# Fitting a model using default priors to test this
m1.1 <- brm(data = hibbs,
            family = gaussian,
            vote ~ 0 + Intercept + growth,
            seed = 1,
            file = "Regression and other stories/fits/m1.1")
print(m1.1)

nd <- tibble(growth = seq(from = -1, to = 5, length.out = 50))

fitted(m1.1,
       newdata = nd) |> 
  data.frame() |> 
  bind_cols(nd) |> 
  mutate(vote = Estimate) |> 
  ggplot(aes(x = growth, y = vote)) +
  geom_hline(yintercept = 50, color = "grey85", size = 1/4) +
  geom_smooth(aes(ymin = Q2.5, ymax = Q97.5),
              stat = "identity", alpha = 1/5, size = 1/4) +
  geom_point(data = hibbs) +
  annotate(geom = "text",
           x = 2.5, y = 53,
           label = str_c("y==", round(fixef(m1.1)[1, 1], digits = 1), "+",
                         round(fixef(m1.1)[2, 1], digits = 1), "*x"),
           hjust = 0, parse = T) +
  scale_x_continuous(labels = function(x) str_c(x, "%")) +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  labs(subtitle = "Data and linear fit",
       x = "Average recent growth in personal income",
       y = "Incumbent party's vote share")

