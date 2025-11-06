install.packages("summarytools")
library(summarytools)
install.packages("janitor")
library(janitor)
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)

messy_data <- read_excel("data/messy_mangrove_data.xlsx")

stview(dfSummary(messy_data))

library(tidyr)

messy_long <- pivot_longer(
  messy_data,
  cols = starts_with("week"),
  names_to = "Week",
  values_to = "value"
) %>% rename(Height = value) %>% 
 select(-Notes)

stview(dfSummary(messy_long))

clean_long <- messy_long %>% 
  mutate(Height = parse_number(Height)) %>% 
  mutate(Location = str_to_lower(Location)) %>% 
  mutate(Treatment = case_when(
  Treatment %in% c("Control", "control", "CTRL") ~ "control",
  Treatment %in% c("T1", "Treatment 1") ~ "treatment 1",
  Treatment %in% c("T2", "Treatment 2") ~ "treatment 2"
  ))

stview(dfSummary(clean_long))

#this is jeremiahs way of doing it
clean_mang_data <- function(df) {
  m <- df %>%
    clean_names() %>%
    pivot_longer(
      cols = starts_with("week"),
      names_to = "week_number",
      values_to = "height_mm"
    ) %>%
    mutate(
      week_number = parse_number(week_number),
      height_mm = parse_number(height_mm),
      location = str_to_lower(location),
      treatment = case_when(
        treatment %in% c("Control", "control", "CTRL") ~ "control",
        treatment %in% c("T1", "Treatment 1") ~ "treatment 1",
        treatment %in% c("T2", "Treatment 2") ~ "treatment 2",
        TRUE ~ treatment
      )
    )
  return(m)
}

clean_mang_data(messy_data)
