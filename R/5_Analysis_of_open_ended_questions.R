# Code to reproduce the results presented in "Birdwatcher's attitudes and preferences that influence
# their decisions to engage in local, national, and international birdwatching trips." Published in 
# the Journal of Ecotourism.

# Analysis of open-ended questions

library(tidyverse)
library(lubridate)

# read in data
survey <- read_csv("Data/Final_Survey_Data.csv")

# the survey officially launched on Oct 26, 2023. All survey entries before this were part of pilot testing.
# remove pilot testing data.
survey <- survey %>%
  mutate(StartDate = mdy_hm(survey$StartDate)) %>%
  filter(StartDate >= mdy_hm("10/26/2023 00:00"),
         Finished==TRUE)

# filter the data to US-only respondents - the results of the main analysis use data from just US birdwatchers. However, you can 
# remove this code to get results from all birdwatchers who took the survey
survey <- survey %>%
  filter(country=="United States of America")

# alternatively, you could run the below code to get results from only birdwatchers from the US that are not casual birdwatchers,
# as reported in supplementary material
# survey <- survey %>%
#  filter(country=="United States of America",
#         Q9!="Casual birder -- enjoys bird watching while taking trips for other primary reasons (nature, culture, hobbies). Doesn’t necessarily keep a list and is mostly driven by the enjoyment of birds for their beauty or interesting features.")

# we categorized notes into six themes, so let's summarize that here

# count of notes 
complete.notes <- survey %>%
  dplyr::filter(complete.cases(Q40)) %>%
  dplyr::summarise(count=n())

# separate out the notes theme column
notes_sep <- survey %>%
  dplyr::filter(complete.cases(Q40), complete.cases(Q40_Themes)) %>%
  separate_rows(Q40_Themes, sep="; ")

# get percentage of each theme
(per_theme <- notes_sep %>%
    group_by(Q40_Themes) %>%
    dplyr::summarise(per=(n()/as.numeric(complete.notes))*100))
