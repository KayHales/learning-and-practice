# packages and theme settings
library(tidyverse)
# remotes::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

theme_set(theme_linedraw() +
            theme(panel.grid = element_blank()))

# start with an example: 

# compares states and DC with the human development index (HDI). but there are some 
# concerns with how these data were used in the past. HDI is developed with three dimensions:
# life expectancy at birth, knowledge and education, standard of living. 

# import data
hdi <- read.table("Regression and other stories/data/hdi.dat", header = TRUE)
votes <- foreign::read.dta("Regression and other stories/data/state vote and income, 68-00.dta")

glimpse(hdi)
glimpse(votes)

# join hdi and votes, then make the map 
left_join( # joins map polygons to the joined hdi-votes df below
  get_urbn_map(map = "states", sf = TRUE),
  left_join(hdi, # this joins hdi and votes
            votes |> mutate(state = st_state), # give both df the state column
            by = "state") |> # join by state 
    mutate(state_name = ifelse(state == "Washington, D.C.", "District of Columbia", state)),
  by = "state_name"
) |> 
  mutate(hdi = case_when( # make hdi discrete
    hdi >= .95 ~ "0.95+",
    hdi >= .90 ~ ".9-.949",
    hdi >= .85 ~ ".85-.899",
    hdi >= .80 ~ ".8-.849",
    hdi >- .75 ~ ".75-.799"
  )) |> # now make it a factor
  mutate(hdi = factor(hdi, 
                      levels = c("0.95+", ".9-.949", ".85-.899",
                                 ".8-.849", ".75-.799"))) |> 
  ggplot() +
  geom_sf(aes(fill = hdi, geometry = geometry),
          size = .1, color = "white") +
  scale_fill_viridis_d("HDI by State", option = "E",
                       guide = guide_legend(
                         direction = "horizontal", 
                         keywidth = unit(1.5, "cm"),
                         label.hjust = 0, 
                         label.position = "bottom",
                         title.position = "top"
                       )) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("hdi_by_state_map.png", 
       path = "Regression and other stories/output",
       width = 8,
       height = 5)

# if we look at income, we can actually see:
left_join(hdi,
          votes |> mutate(state = st_state),
          by = "state") |> 
  filter(st_year == "2000") |> 
  mutate(rank_income = min_rank(st_income), # give values ranks; e.g., c(90, 92, 99) = c(3, 2, 1)
         hdi_rank = min_rank(hdi)) |> 
  select(st_stateabb, income_rank, hdi_rank) |> 
  arrange(desc(hdi_rank))

# hdi and income seem to correlate decently high or, visually:
left_join(hdi,
          votes |> mutate(state = st_state),
          by = "state") |> 
  filter(st_year == "2000") |> 
  mutate(income_rank = min_rank(st_income), 
         hdi_rank = min_rank(hdi)) |> 
  ggplot(aes(x = income_rank, y = hdi_rank, label = st_stateabb)) +
  geom_text() +
  geom_smooth(stat = "smooth", se = FALSE) +
  labs(x = "Rank of avg state income in 2000",
       y = "Rank of HDI in 2000") 

ggsave("state_income_and_hdi_rank_line.png", 
       path = "Regression and other stories/output",
       width = 8,
       height = 5)


