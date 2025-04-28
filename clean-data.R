library(tidyverse)

d <- 
  read_csv("data/Heart_Attack_Risk_Levels_Dataset.csv") |> 
  janitor::clean_names()

d2 <- 
  d |> 
  mutate(across(
    gender, 
    ~ifelse(.==1, "Male", "Female") |> factor(levels = c("Female", "Male"))
  ))

write_csv(d2, "data/heart_attack_risk_clean.csv")
