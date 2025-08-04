# Code to reproduce the results presented in "Birdwatcher's attitudes and preferences that influence
# their decisions to engage in local, national, and international birdwatching trips." Published in 
# the Journal of Ecotourism.

# Barriers and motivations for international birdwatcher travel

library(tidyverse)
library(lubridate)
library(tidyr)
library(basemaps)
library(ggspatial)
library(sf)
library(scales)

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

# Do you engage in birdwatching activities internationally?
(international_birdwatching <- survey %>% group_by(Q20) %>%
    filter(complete.cases(Q20)) %>% 
    summarize(count=n(), percentage=n()/nrow(survey %>%
                                               filter(complete.cases(Q20)))*100))

ggplot(data=international_birdwatching, aes(x=Q20, y=percentage)) + geom_bar(stat="identity") +
  theme_classic() + xlab("Do you engage in birdwatching activities internationally?") + ylab("Percentage")


# Non-internationally traveling respondents ------------------------------------------------------------

## Reasons that respondents do not travel internationally ------------------

# why have you not traveled internationally for birdwatching?
(no_int_bw <-survey %>% separate_rows(Q22, sep=",") %>%
   group_by(Q22) %>%
   filter(complete.cases(Q22)) %>%
   summarize(percentage=n()/nrow(survey %>%
                                   filter(complete.cases(Q22)))*100) %>%
   mutate(Q22=factor(str_wrap(Q22, width=25), levels=c(unique(str_wrap(Q22, width=25))))) %>%
   arrange(desc(percentage)))

ggplot(data=no_int_bw, aes(x=reorder(Q22, percentage), y=percentage)) + 
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + xlab("Reason for not traveling internationally") + ylab("Percentage") +
  coord_flip() +
  theme(text = element_text(size = 27))

# list of other reasons
unique(survey$Q22_9_TEXT)

# In the future would you like to travel internationally to engage in birdwatching activities?
(future_int_bw <- survey %>% group_by(Q23) %>%
    filter(complete.cases(Q23)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q23)))*100))

ggplot(data=future_int_bw, aes(x=Q23, y=percentage)) + geom_bar(stat="identity") +
  theme_classic() + xlab("In the future, would you like to travel internationally to engage in birdwatching?") + ylab("Percentage")


## Countries respondents desire to visit -----------------------------------

# If you were to go birdwatching internationally, which three countries would you hope to visit first?
(top_country <- survey %>% group_by(Q24_1) %>%
   filter(complete.cases(Q24_1)) %>% 
   dplyr::summarize(count=n(), percentage=n()/nrow(survey)*100) %>%
   arrange(desc(percentage)) %>%
   dplyr::rename(country=Q24_1))

(second_country <- survey %>% group_by(Q24_2) %>%
    filter(complete.cases(Q24_2)) %>% 
    dplyr::summarize(count=n(), percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage)) %>%
    dplyr::rename(country=Q24_2))

(third_country <- survey %>% group_by(Q24_3) %>%
    filter(complete.cases(Q24_3)) %>% 
    dplyr::summarize(count=n(), percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage))%>%
    dplyr::rename(country=Q24_3))

(top_country_gg <- ggplot(data=top_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Top Country") + coord_flip() +
    ylab("Percentage"))

(second_country_gg <- ggplot(data=second_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Second Top Country") + coord_flip() +
    ylab("Percentage"))

(third_country_gg <- ggplot(data=third_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Third Top Country") + coord_flip() +
    ylab("Count"))

# map international travel destinations
all_countries <- rbind(third_country, top_country, second_country)

# we need to split trinidad and trabago into two separate columns
all_countries <- all_countries %>% 
  group_by(country) %>%
  dplyr::summarise(count=sum(count)) %>%
  mutate(country=case_when(country=="United States" ~ "USA",
                           country=="United Kingdom" ~ "UK",
                           country=="Galapagos" ~ "Ecuador",
                           country=="Equador" ~ "Ecuador",
                           country=="Great Britain" ~ "UK",
                           country=="Hawaii" ~ "USA",
                           country=="New Guinea" ~ "Papua New Guinea",
                           country=="Scotland" ~ "UK",
                           country=="England" ~ "UK",
                           TRUE ~ country),
         country=as.character(country)) %>%
  filter(!country %in% c("Africa", "Caribbean")) %>%
  unnest(country)

# get map
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# prepare for figure
world_with_counts <- world %>%
  left_join(all_countries, by = c("ID" = "country"))

# plot the data
ggplot(data = world_with_counts) +
  geom_sf(aes(fill = count), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Birdwatchers", 
    trans="reverse", 
    direction=-1) +
  theme_bw() +
  labs(x=NULL, y=NULL) + 
  theme(panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Countries Non-Internationally Traveling Birdwatchers Desire to Visit")

ggsave("Figures/Non_international_birdwatchers_map.jpeg", height=8, width=11)

# Internationally traveling respondents ------------------------------------------------------------

# What tours/festivals have you attended (select all that apply)
(tours_fest <-survey %>% separate_rows(Q31, sep=",") %>%
   group_by(Q31) %>%
   filter(complete.cases(Q31)) %>%
   summarize(percentage=n()/nrow(survey %>%
                                   filter(complete.cases(Q31)))*100) %>%
   arrange(desc(percentage)))

tours_fest$Q31 <- sub("\\(.*", "", tours_fest$Q31)

(tours_fest_plot <- ggplot(data=tours_fest, aes(x=reorder(Q31, percentage), y=percentage)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_classic() + theme(axis.title.y=element_blank()) + ylab("Percentage") +
    coord_flip())

# How often do you travel alone versus in a group of birdwatchers on international birdwatching trips?
(travel_group <-survey %>%
    group_by(Q32) %>%
    filter(complete.cases(Q32)) %>%
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q32)))*100) %>%
    arrange(desc(percentage)))

(group_travel_plot <- ggplot(data=travel_group, aes(x=reorder(Q32, percentage), y=percentage)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_classic() + theme(axis.title.y=element_blank()) + ylab("Percentage") +
    coord_flip())

# How do you typically gather information about birdwatching trips?
bw_info <- survey
bw_info$Q37...73 <- sub("\\(.*", "", bw_info$Q37...73)
(bw_info <- bw_info %>% separate_rows(Q37...73, sep=",") %>%
    group_by(Q37...73) %>%
    filter(complete.cases(Q37...73)) %>%
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q37...73)))*100) %>%
    arrange(desc(percentage)) %>%
    mutate(Q37...73=factor(str_wrap(Q37...73, width=25), levels=c(unique(str_wrap(Q37...73, width=25))))))

(information_gathering_plot <- ggplot(data=bw_info, aes(x=reorder(Q37...73, percentage), y=percentage)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_classic() + theme(axis.title.y=element_blank()) + ylab("Percentage") +
    ggtitle(str_wrap("Method of Gathering Information about Birdwatching Trips", width=35)) +
    coord_flip())

# Other write in
unique(survey$Q37_8_TEXT)

# Average cost per day of international birdwatching trips
(cost_int <- survey %>% 
    group_by(Q38) %>%
    filter(complete.cases(Q38)) %>%
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q38)))*100) %>%
    mutate(Q38=factor(Q38, levels=c("More than $600",
                                    "$400-$600",
                                    "$200-$400",
                                    "$100-$200",
                                    "$50-$100"))) %>%
    arrange(desc(percentage)))

(cost_trips_plot <- ggplot(data=cost_int, aes(x=Q38, y=percentage)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_classic() + theme(axis.title.y=element_blank()) + ylab("Percentage") +
    ggtitle(str_wrap("Average Cost Per Day of Birdwatching Trip", width=30)) +
    coord_flip())

# When selecting an international birding trip, how important is it that some of the profit is 
# invested in local communities?

# money for local communities to improve their livelihoods
(invest_livelihoods <-survey %>%
    group_by(Q39_1) %>%
    filter(complete.cases(Q39_1)) %>%
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q39_1)))*100) %>%
    mutate(Q39_1=factor(Q39_1, levels=c("Extremely important",
                                        "Very important",
                                        "Moderately important",
                                        "Slightly important",
                                        "Not at all important"))) %>%
    arrange(desc(percentage)))

(invest_livelihoods_plot <- ggplot(data=invest_livelihoods, aes(x=Q39_1, y=percentage)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_classic() + theme(axis.title.y=element_blank()) + ylab("Percentage") + ggtitle("Invest in Livelihoods") +
    coord_flip())

# money for conservation and restoration activities that benefit birds and their habitats
(invest_conserv <-survey %>%
    group_by(Q39_2) %>%
    filter(complete.cases(Q39_2)) %>%
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q39_2)))*100) %>%
    mutate(Q39_2=factor(Q39_2, levels=c("Extremely important",
                                        "Very important",
                                        "Moderately important",
                                        "Slightly important",
                                        "Not at all important"))) %>%
    arrange(desc(percentage)))

(invest_conserv_plot <- ggplot(data=invest_conserv, aes(x=Q39_2, y=percentage)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_classic() + 
    theme(axis.title.y=element_blank()) + 
    ylab("Percentage") + ggtitle("Invest in Conservation") +
    coord_flip())

## Countries respondents have visited ------------------------------------------------------------

# How many countries have you visited for birdwatching?
(countries_bw <- survey %>% group_by(Q25) %>%
   filter(complete.cases(Q25)) %>% 
   summarize(count=n()) %>%
   arrange(desc(count)))

ggplot(data=countries_bw, aes(x=as.numeric(Q25), y=count)) + geom_bar(stat="identity") +
  theme_classic() + theme(axis.title.y=element_blank()) + xlab("Number of Countires") + 
  ylab("Count")

# last three countires you visited for birdwatching
(recent_country <- survey %>% group_by(Q26_1) %>%
    filter(complete.cases(Q26_1)) %>% 
    dplyr::summarize(count=n(), percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage)) %>%
    dplyr::rename(country=Q26_1))

(second_recent_country <- survey %>% group_by(Q26_2) %>%
    filter(complete.cases(Q26_2)) %>% 
    dplyr::summarize(count=n(), percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage)) %>%
    dplyr::rename(country=Q26_2))

(third_recent_country <- survey %>% group_by(Q26_3) %>%
    filter(complete.cases(Q26_3)) %>% 
    dplyr::summarize(count=n(), percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage)) %>%
    dplyr::rename(country=Q26_3))

(recent_country_gg <- ggplot(data=recent_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Recent Country Visited") + coord_flip() +
    ylab("Percentage"))

(second_recent_country_gg <- ggplot(data=second_recent_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Second Recent Country Visited") + coord_flip() +
    ylab("Percentage"))

(third_recent_country_gg <- ggplot(data=third_recent_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Third Recent Country Visited") + coord_flip() +
    ylab("Percentage"))

# map international travel locations
all_countries <- rbind(recent_country, second_recent_country, third_recent_country)

# clean country names
all_countries <- all_countries %>%
  mutate(country=case_when(country=="United States" ~ "USA",
                           country=="United Kingdom" ~ "UK",
                           country=="Galapagos" ~ "Ecuador",
                           country=="Equador" ~ "Ecuador",
                           country=="Great Britain" ~ "UK",
                           country=="Hawaii" ~ "USA",
                           country=="New Guinea" ~ "Papua New Guinea",
                           country=="Scotland" ~ "UK",
                           country=="America" ~ "USA",
                           country=="Borneo" ~ "Indonesia",
                           country=="British Virgin Islands" ~ "Virgin Islands, British",
                           country=="Canda" ~ "Canada",
                           country=="Costa Rico" ~ "Costa Rica",
                           country=="Domician Republic" ~ "Dominican Republic",
                           country=="Dubai" ~ "United Arab Emirates",
                           country=="Inglaterra" ~ "UK",
                           country=="Kazhakstan" ~ "Kazakhstan",
                           country=="Kyrgzstan" ~ "Kyrgyzstan",
                           country=="Malyasia" ~ "Malaysia",
                           country=="Phillipines" ~ "Philippines",
                           country=="Republic of Georgia" ~ "Georgia",
                           country=="San Blas" ~ "Panama",
                           country=="Surinam" ~ "Suriname",
                           country=="Tahiti" ~ "French Polynesia",
                           TRUE ~ country),
         country=as.character(country)) %>%
  filter(!country %in% c("Africa", "Caribbean", "England", "Albany", 
                         "Lesser Antilles", "Lesser Antilles (multiple countries)")) %>%
  unnest(country) %>% 
  group_by(country) %>%
  dplyr::summarise(count=sum(count))

# get map
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# prepare for figure
world_with_counts <- world %>%
  left_join(all_countries, by = c("ID" = "country"))

# plot the data
ggplot(data = world_with_counts) +
  geom_sf(aes(fill = count), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Birdwatchers", , 
    trans="reverse", 
    direction=-1) +
  theme_bw() +
  labs(x=NULL, y=NULL) + 
  theme(panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Countries Internationally Traveling Birdwatchers Have Visited")

ggsave("Figures/international_birdwatchers_map.jpeg", height=8, width=11)

## Countries respondents desire to visit ------------------------------------------------------------

# List the next 3 countries you plan to, or hope to, visit next for birdwatching
(future_country <- survey %>% group_by(Q27_1) %>%
   filter(complete.cases(Q27_1)) %>% 
   dplyr::summarize(count=n(),
                    percentage=n()/nrow(survey)*100) %>%
   arrange(desc(percentage)) %>%
   dplyr::rename(country=Q27_1))

(second_future_country <- survey %>% group_by(Q27_2) %>%
    filter(complete.cases(Q27_2)) %>%
    dplyr::summarize(count=n(),
                     percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage)) %>%
    dplyr::rename(country=Q27_2))

(third_future_country <- survey %>% group_by(Q27_3) %>%
    filter(complete.cases(Q27_3)) %>% 
    dplyr::summarize(count=n(),
                     percentage=n()/nrow(survey)*100) %>%
    arrange(desc(percentage))%>%
    dplyr::rename(country=Q27_3))

(future_country_gg <- ggplot(data=future_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("First Country Planned to Visited") + coord_flip() +
    ylab("Percentage"))

(second_future_country_gg <- ggplot(data=second_future_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Second Country Planned to Visited") + coord_flip() +
    ylab("Percentage"))

(third_future_country_gg <- ggplot(data=third_future_country, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
    theme_classic() + theme(axis.title.y=element_blank()) + ggtitle("Third Country Planned to Visited") + coord_flip() +
    ylab("Percentage"))


# bind the three data frames
all_countries <- rbind(future_country, second_future_country, third_future_country)

# clean country names
all_countries <- all_countries %>%
  mutate(country=case_when(country=="United States" ~ "USA",
                           country=="United Kingdom" ~ "UK",
                           country=="Galapagos" ~ "Ecuador",
                           country=="Equador" ~ "Ecuador",
                           country=="Great Britain" ~ "UK",
                           country=="Hawaii" ~ "USA",
                           country=="New Guinea" ~ "Papua New Guinea",
                           country=="Scotland" ~ "UK",
                           country=="America" ~ "USA",
                           country=="Borneo" ~ "Indonesia",
                           country=="British Virgin Islands" ~ "Virgin Islands, British",
                           country=="Canda" ~ "Canada",
                           country=="Costa Rico" ~ "Costa Rica",
                           country=="Domician Republic" ~ "Dominican Republic",
                           country=="Dubai" ~ "United Arab Emirates",
                           country=="Inglaterra" ~ "UK",
                           country=="Kazhakstan" ~ "Kazakhstan",
                           country=="Kyrgzstan" ~ "Kyrgyzstan",
                           country=="Malyasia" ~ "Malaysia",
                           country=="Phillipines" ~ "Philippines",
                           country=="Republic of Georgia" ~ "Georgia",
                           country=="San Blas" ~ "Panama",
                           country=="Surinam" ~ "Suriname",
                           country=="Tahiti" ~ "French Polynesia",
                           country=="Argentina (planned Dec 2023)" ~ "Argentina",
                           country=="Azerbijan" ~ "Azerbaijan",
                           country=="Barbaros" ~ "Barbados",
                           country=="Bulgaria and Romania" ~ "Bulgaria",
                           country=="Burmuda" ~ "Bermuda",
                           country=="Chili" ~ "Chile",
                           country=="Chilie" ~ "Chile",
                           country=="Falklands" ~ "UK",
                           country=="New Zealand-2025" ~ "New Zealand",
                           country=="Newfoundland" ~ "Canada",
                           country=="Oaxaca" ~ "Mexico",
                           country=="Philipines" ~ "Philippines",
                           country=="Republic of Ireland" ~ "UK",
                           country=="Socotra" ~ "Yemen",
                           country=="Solomon Isalnds" ~ "Solomon Islands",
                           country=="Sri lanka" ~ "Sri Lanka",
                           country=="St. Kitts and Nevis" ~ "Saint Kitts",
                           country=="The Gambia" ~ "Gambia",
                           country=="Trinidad and Tabago" ~ "Trinidad",
                           country=="Trinidad and Tobego" ~ "Trinidad",
                           country=="West Indies" ~ "Australia",
                           country=="Tasmania" ~ "Australia",
                           TRUE ~ country),
         country=as.character(country)) %>%
  filter(!country %in% c("Africa", "Caribbean", "England", "Albany", 
                         "Lesser Antilles", "Lesser Antilles (multiple countries)",
                         "Greater and Lesser Antilles", "Lesser Antilles islands",
                         "West Indies")) %>%
  unnest(country) %>% 
  group_by(country) %>%
  dplyr::summarise(count=sum(count))

# get map
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# prepare for figure
world_with_counts <- world %>%
  left_join(all_countries, by = c("ID" = "country"))

ggplot(data = world_with_counts) +
  geom_sf(aes(fill = count), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Birdwatchers", 
    trans="reverse", 
    direction=-1) +
  theme_bw() +
  labs(x=NULL, y=NULL) + 
  theme(panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Countries Internationally Traveling Birdwatchers Desire to Visit")

ggsave("Figures/international_birdwatchers_desire_map.jpeg", height=8, width=11)
