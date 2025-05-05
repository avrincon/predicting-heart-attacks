library(tidyverse)

d <- 
  read_csv("data/Heart_Attack_Risk_Levels_Dataset.csv") |> 
  janitor::clean_names()

d2 <- 
  d |> 
  mutate(
    across(gender, ~factor(., labels = c("Female", "Male")))
  ) |> 
  select(-risk_level, -recommendation)

write_csv(d2, "data/heart_attack_risk_clean.csv")
