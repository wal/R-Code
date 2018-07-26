# dplyr & case_when

submission_3 <- test %>% 
  mutate(Survived = case_when(
    .$Sex == "female" & .$Pclass == 3 & .$Fare >= 20 ~ 0,
    .$Sex == "female" ~ 1,
    TRUE ~ 0
  )) %>% select(PassengerId, Survived)

## drop columns starting with a string
diseaseInfo %>% select(-starts_with("human"))


## One Hot Encoding - Take a categorical variable and convert it into a seperate column for each level and a boolean to represent it included .. or not



