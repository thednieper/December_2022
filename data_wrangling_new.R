install.packages("tidytuesdayR")
install.packages("dplyr")

library(dplyr)
library(janitor)

tuesdata <- tidytuesdayR::tt_load("2022-02-01")

saveRDS(tuesdata$breed_traits, "breed_traits.rds")

breed_traits <- clean_names("breed_traits")

select(breed_traits, breed)

select(breed_traits, coat_length)

arrange(breed_traits, drooling_level)

# filter and then arrange via the pipe

breed_traits |> 
  filter(drooling_level==5) |> 
  arrange(breed)

drooly_dogs |>
  filter(drooling_level==5) |> 
  arrange(breed)

noisy_dogs |> 
  filter(barking_level==5) |> 
  mutate(bark_energy_level = energy_level*barking_level) |> 
  select(breed, energy_level, barking_level, bark_energy_level) |> 
  arrange(bark_energy_level)

install.packages("tidyverse")
library(tidyverse)

install.packages("backports")

install.packages("tidyverse")
library(tidyverse)

install.packages("colorspace")

breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  select(breed, trainability_level, trainability_category) |> 
  filter(trainability_category == "Very trainable")

smooth_dogs <- breed_traits |> 
  mutate(smooth_coat = if_else(coat_type == "Smooth", TRUE, FALSE)) |> 
  select(breed, coat_type, smooth_coat)

breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  group_by(trainability_category) |> 
  summarise(
    avg_energy_lvl = mean(energy_level),
    count = n()
    )

breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  group_by(trainability_category) |> 
  summarise(
    avg_energy_lvl = median(energy_level),
    count = n()
  )

breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  count(trainability_category)

#group by, summarise, count TODAY is 30 November 2022



