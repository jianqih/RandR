library(tidyverse)
library(purrr)
library(haven)
library(visdat)
getwd()
cfps2010family <- read_dta("/Users/a182501/rproject/cfps/data/cfps2010famecon_202008.dta")
cfps2010family %>%
  select(urban, starts_with("fk201_a")) %>%
  glimpse()



library(purrr)
get_var_label <- function(dta) {
  labels <- map(dta, function(x) attr(x, "label"))
  data_frame(
    name = names(labels),
    label = as.character(labels)
  )
}

cfps2010family %>%
  select(urban, starts_with("fk201_a")) %>%
  get_var_label()


cfps2010family %>%
  select(urban, starts_with("fk201_a")) %>%
  map(~ count(data.frame(x = .x), x))



library(naniar)
cfps2010family %>%
  select(urban, starts_with("fk201_a")) %>%
  miss_var_summary()



library(visdat)
cfps2010family %>%
  select(urban, starts_with("fk201_a")) %>%
  vis_dat()

library(conflicted)
conflict_prefer("filter", "dplyr")

cfps2010family %>%
  select(urban, starts_with("fk2_s"))%>%
  filter(urban == 0)

a <- cfps2010family %>%
  select(urban, starts_with("fk201_a")) %>%
  filter_at(vars(starts_with("fk201_a")), any_vars(. > 0))

a

a %>% mutate_at(vars(starts_with("fk201_a")), funs(replace(., . < 0, 0)))

