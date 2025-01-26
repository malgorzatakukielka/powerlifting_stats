library(tidyverse)
# przygotowanie danych
ipf <- read.csv("data/openipf-2025-01-25-f626d9e2.csv")
ipf$Sex <- factor(ipf$Sex, levels = c("F", "M"), labels = c("Female", "Male"))
ipf$Event <- factor(ipf$Event)
ipf$Equipment <- factor(ipf$Equipment)
ipf$AgeClass <- factor(ipf$AgeClass)
ipf$WeightClassKg <- factor(ipf$WeightClassKg)

ipf <- ipf %>% 
  select(-Division, -Squat4Kg, -Bench4Kg, -Deadlift4Kg)

ipf$AgeClass <- fct_recode(ipf$AgeClass, "80+" = "80-999")

ipf <- ipf %>%
  filter(!is.na(BodyweightKg), !is.na(Sex), AgeClass != "", AgeClass != "5-12")

ipf <- ipf %>%
  mutate(WeightClass = case_when(
    # mężczyźni
    Sex == "Male" & BodyweightKg <= 53 & 
      (AgeClass == "13-15" | AgeClass == "16-17" | AgeClass == "18-19" | AgeClass == "20-23") ~ "53 kg",
    Sex == "Male" & BodyweightKg <= 59 ~ "59 kg",
    Sex == "Male" & BodyweightKg <= 66 ~ "66 kg",
    Sex == "Male" & BodyweightKg <= 74 ~ "74 kg",
    Sex == "Male" & BodyweightKg <= 83 ~ "83 kg",
    Sex == "Male" & BodyweightKg <= 93 ~ "93 kg",
    Sex == "Male" & BodyweightKg <= 105 ~ "105 kg",
    Sex == "Male" & BodyweightKg <= 120 ~ "120 kg",
    Sex == "Male" & BodyweightKg > 120 ~ "120+ kg",
    #kobiety
    Sex == "Female" & BodyweightKg <= 43 & 
      (AgeClass == "13-15" | AgeClass == "16-17" | AgeClass == "18-19" | AgeClass == "20-23") ~ "43 kg",
    Sex == "Female" & BodyweightKg <= 47 ~ "47 kg",
    Sex == "Female" & BodyweightKg <= 52 ~ "52 kg",
    Sex == "Female" & BodyweightKg <= 57 ~ "57 kg",
    Sex == "Female" & BodyweightKg <= 63 ~ "63 kg",
    Sex == "Female" & BodyweightKg <= 69 ~ "69 kg",
    Sex == "Female" & BodyweightKg <= 76 ~ "76 kg",
    Sex == "Female" & BodyweightKg <= 84 ~ "84 kg",
    Sex == "Female" & BodyweightKg > 84 ~ "84+ kg",
    TRUE ~ "Unknown")
  )

ipf <- ipf %>% 
  relocate(WeightClass, .before = WeightClassKg)

ipf$WeightClass <- factor(ipf$WeightClass)
