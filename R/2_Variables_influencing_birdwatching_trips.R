# Code to reproduce the results presented in "Birdwatcher's attitudes and preferences that influence
# their decisions to engage in local, national, and international birdwatching trips." Published in 
# the Journal of Ecotourism.

# Variables influencing birdwatching trips

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

# clean type of birdwatcher field
survey$Q9 <- sub("\\--.*", "", survey$Q9)
(birdwatcher_type <- survey %>% group_by(Q9) %>%
    mutate(Q9=factor(Q9, levels=c("Hardcore birder ",
                                  "Enthusiastic birder ",
                                  "Casual birder "))) %>%
    filter(complete.cases(Q9)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q9)))*100))

# Locally -----------------------------------------------------------------

# Do you engage in birdwatching activities locally?
(local_birdwatching <- survey %>% group_by(Q10) %>%
   filter(complete.cases(Q10)) %>% 
   summarize(percentage=n()/nrow(survey %>%
                                   filter(complete.cases(Q10)))*100))

ggplot(data=local_birdwatching, aes(x=Q10, y=percentage)) + geom_bar(stat="identity") +
  theme_classic() + xlab("Do you engage in birdwatching activities locally?") + ylab("Percentage")

# how often do you engage in local birdwatching activities per month?
(local_birdwatching_freq <- survey %>% group_by(Q11) %>%
    mutate(Q11=factor(Q11, levels=c("More than 10 times a month",
                                    "6-10 times a month",
                                    "1-5 times a month",
                                    "Less than once a month"))) %>%
    filter(complete.cases(Q11)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q11)))*100))

ggplot(data=local_birdwatching_freq, aes(x=Q11, y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Frequency of birdwatching activities per month") + ylab("Percentage")


# what are the most important factors when visiting local birdwatching sites?
duplicates <- function(i) {
  Q12survey <- survey %>% dplyr::select(starts_with("Q12"))
  return(!any(duplicated(as.numeric(Q12survey[i,]))))
}

survey$correct_rankQ12 <- lapply(1:nrow(survey), duplicates)

Q12survey <- survey %>%
  filter(correct_rankQ12==TRUE)

# filter out people who did not answer this question all together
answersQ12 <- survey %>% dplyr::select(starts_with("Q12")) %>%
  filter_all(any_vars(!is.na(.)))

# proportion of retained answers
nrow(Q12survey)/(nrow(survey)-(nrow(survey)-nrow(answersQ12)))

(rare_species <- Q12survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q12_1),
           Q12_1 > 0,
           Q12_1 <= 5,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q12_1)), sd=sd(as.numeric(Q12_1))) %>%
    mutate(question="See rare species") %>%
    rename(birdwatcher_type=Q9)) 

(life_list <- Q12survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q12_2),
           Q12_2 > 0,
           Q12_2 <= 5,
           complete.cases(Q9)) %>%
    summarize(mean=mean(as.numeric(Q12_2)), sd=sd(as.numeric(Q12_2))) %>%
    mutate(question="Add to life list") %>%
    rename(birdwatcher_type=Q9))

(monitoring <- Q12survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q12_3),
           Q12_3 > 0,
           Q12_3 <= 5, 
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q12_3)), sd=sd(as.numeric(Q12_3))) %>%
    mutate(question="Species monitoring")  %>%
    rename(birdwatcher_type=Q9))

(new_sites <- Q12survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q12_4),
           Q12_4 > 0,
           Q12_4 <= 5,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q12_4)), sd=sd(as.numeric(Q12_4))) %>%
    mutate(question="Explore new sites") %>%
    rename(birdwatcher_type=Q9))

(new_people <- Q12survey %>%
    group_by(Q9) %>%
    filter(complete.cases(Q12_5),
           Q12_5 > 0,
           Q12_5 <= 5,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q12_2)), sd=sd(as.numeric(Q12_2))) %>%
    mutate(question="Meet people")  %>%
    rename(birdwatcher_type=Q9))

local_birdwatching_rank <- rbind(rare_species, life_list, monitoring, new_sites, new_people)

(local_rank <- ggplot(local_birdwatching_rank, aes(x = reorder(question, -mean), y = -mean, color = birdwatcher_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +  # Plot means
    geom_errorbar(aes(ymin = -mean - sd, ymax = -mean + sd),  # Plot error bars
                  position = position_dodge(width = 0.5), width = 0.5, size=1.5) +
    labs(title = "Local",
         x = "", y = "Rank", color = "Birdwatcher Type") +
    theme_classic() +
    coord_flip() +  # Rotate x-axis labels for better readability 
    theme(text = element_text(size = 20)) +
    guides(color = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = c("indianred2", "gold3", "steelblue")) +
    scale_y_continuous(labels = c("Low Importance", "High Importance"),
                       breaks=c(-5,-1)) +
    geom_hline(yintercept=-3, linetype="dashed")) 

ggsave("Figures/Factor_Rank_Local_Birdwatching.jpeg", height=7, width=10, units="in")

## Difference between groups -----------------------------------------------

# Run Wilcox test to see if birder category rankings are significantly different
# to start, create a data frame from all the data
Q12_columns <- grep("^Q12", colnames(Q12survey), value = TRUE)

(stats_local <- Q12survey %>% 
    dplyr::select(Q9, starts_with("Q12")) %>%
    dplyr::rename("rare_species"=Q12_1,
                  "life_list"=Q12_2,
                  "monitoring"=Q12_3,
                  "new_sites"=Q12_4,
                  "new_people"=Q12_5) %>%
    pivot_longer(cols=c("rare_species", "life_list", "monitoring",
                        "new_sites", "new_people"),
                 names_to="characteristics",
                 values_to="rank") %>%
    filter(complete.cases(rank),
           rank > 0 &
             rank <= 5) %>%
    mutate(birdwatcher_type=str_remove(Q9, "--.*"),
           rank=as.numeric(rank)) %>%
    dplyr::select(-Q9))

# make characteristics a factor
stats_local$characteristics <- as.factor(stats_local$characteristics)

# make birdwatcher_type a factor
stats_local$birdwatcher_type <- as.factor(stats_local$birdwatcher_type)

# do casual significantly value exploring new sites compared to the other birdwatchers types?
mean((stats_local %>%
        filter(characteristics=="new_sites",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="new_sites",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="new_sites",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="new_sites",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="new_sites",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="new_sites",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="new_sites",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="new_sites",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="new_sites",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))
sd((stats_local %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Casual birder "))$rank)
mean((stats_local %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Casual birder "))$rank)

sd((stats_local %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Enthusiastic birder "))$rank)
mean((stats_local %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_local %>%
        filter(characteristics=="new_people",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="new_people",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="new_people",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="new_people",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="new_people",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="new_people",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="new_people",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="new_people",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="new_people",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_local %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_local %>%
        filter(characteristics=="monitoring",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="monitoring",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="monitoring",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="monitoring",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_local %>%
        filter(characteristics=="monitoring",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_local %>%
      filter(characteristics=="monitoring",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="monitoring",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="monitoring",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_local %>%
                                             filter(characteristics=="monitoring",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

# In-Country -----------------------------------------------------------------

# Do you engage in birdwatching activities in-country?
(incountry_birdwatching <- survey %>% group_by(Q13) %>%
   filter(complete.cases(Q13)) %>% 
   summarize(percentage=n()/nrow(survey %>% 
                                   filter(complete.cases(Q13)))*100))

ggplot(data=incountry_birdwatching, aes(x=Q13, y=percentage)) + geom_bar(stat="identity") +
  theme_classic() + xlab("Do you engage in birdwatching activities in-country?") + ylab("Percentage")

# how often do you engage in local birdwatching activities per month?
(incountry_birdwatching_freq <- survey %>% group_by(Q15) %>%
    mutate(Q15=factor(Q15, levels=c("More than 10 times a year",
                                    "6-10 times a year",
                                    "1-5 times a year",
                                    "Less than once a year"))) %>%
    filter(complete.cases(Q15)) %>% 
    summarize(percentage=n()/nrow(survey %>% filter(complete.cases(Q15)))*100))

ggplot(data=incountry_birdwatching_freq, aes(x=Q15, y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Frequency of birdwatching activities per month") + ylab("Percentage")

# what are the most important factors when visiting local birdwatching sites?
duplicates <- function(i) {
  Q16survey <- survey %>% select(starts_with("Q16")) %>% filter_all(any_vars(!is.na(.)))
  return(!any(duplicated(as.numeric(Q16survey[i,]))))
}

survey$correct_rankQ16 <- lapply(1:nrow(survey), duplicates)

Q16survey <- survey %>%
  filter(correct_rankQ16==TRUE)

# filter out people who did not answer this question all together
answersQ16 <- survey %>% select(starts_with("Q16")) %>%
  filter_all(any_vars(!is.na(.)))

# proportion of retained answers
nrow(Q16survey)/(nrow(survey)-(nrow(survey)-nrow(answersQ16)))

(rare_species <- Q16survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q16_1),
           Q16_1 > 0,
           Q16_1 <= 5,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q16_1)), sd=sd(as.numeric(Q16_1))) %>%
    mutate(question="See rare species") %>%
    rename(birdwatcher_type=Q9)) 

(life_list <- Q16survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q16_2),
           Q16_2 > 0,
           Q16_2 <= 5,
           complete.cases(Q9)) %>%
    summarize(mean=mean(as.numeric(Q16_2)), sd=sd(as.numeric(Q16_2))) %>%
    mutate(question="Add to life list") %>%
    rename(birdwatcher_type=Q9))

(monitoring <- Q16survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q16_3),
           Q16_3 > 0,
           Q16_3 <= 5, 
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q16_3)), sd=sd(as.numeric(Q16_3))) %>%
    mutate(question="Species monitoring")  %>%
    rename(birdwatcher_type=Q9))

(new_sites <- Q16survey %>% 
    group_by(Q9) %>%
    filter(complete.cases(Q16_4),
           Q16_4 > 0,
           Q16_4 <= 5,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q16_4)), sd=sd(as.numeric(Q16_4))) %>%
    mutate(question="Explore new sites") %>%
    rename(birdwatcher_type=Q9))

(new_people <- Q16survey %>%
    group_by(Q9) %>%
    filter(complete.cases(Q16_5),
           Q16_5 > 0,
           Q16_5 <= 5,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q16_2), na.rm = TRUE), sd=sd(as.numeric(Q16_2), na.rm=TRUE)) %>%
    mutate(question="Meet people")  %>%
    rename(birdwatcher_type=Q9))

incountry_birdwatching_rank <- rbind(rare_species, life_list, monitoring, new_sites, new_people)

(incountry_rank <- ggplot(incountry_birdwatching_rank, aes(x = reorder(question, -mean), y = -mean, color = birdwatcher_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +  # Plot means
    geom_errorbar(aes(ymin = -mean - sd, ymax = -mean + sd),  # Plot error bars
                  position = position_dodge(width = 0.5), width = 0.5, size=1.5) +
    labs(title = "In-Country",
         x = "", y = "Rank", color = "Birdwatcher Type") +
    theme_classic() +
    coord_flip() +  # Rotate x-axis labels for better readability 
    theme(text = element_text(size = 20)) +
    guides(color = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = c("indianred2", "gold3", "steelblue")) +
    scale_y_continuous(labels = c("Low Importance", "High Importance"),
                       breaks=c(-5,-1)) +
    geom_hline(yintercept=-3, linetype="dashed")) 


(incountry_rank <- ggplot(incountry_birdwatching_rank, aes(x = reorder(question, -mean), y = -mean, color = birdwatcher_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +  # Plot means
    geom_errorbar(aes(ymin = -mean - sd, ymax = -mean + sd),  # Plot error bars
                  position = position_dodge(width = 0.5), width = 0.5, size=1.5) +
    labs(title = "Local",
         x = "", y = "Rank", color = "Birdwatcher Type") +
    theme_classic() +
    coord_flip() +  # Rotate x-axis labels for better readability 
    theme(text = element_text(size = 20)) +
    guides(color = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = c("indianred2", "gold3", "steelblue")) +
    geom_hline(yintercept=-3, linetype="dashed") +
    scale_y_continuous(labels = function(x) abs(x)))

ggsave("Figures/Factor_Rank_inCountry_Birdwatching.jpeg", height=7, width=10, units="in")

## Differences between groups ----------------------------------------------

# Run Wilcox test to see if birder category rankings are significantly different
# to start, create a data frame from all the data
(stats_incountry <- Q16survey %>% 
   dplyr::select(Q9, starts_with("Q16")) %>%
   dplyr::rename("rare_species"=Q16_1,
                 "life_list"=Q16_2,
                 "monitoring"=Q16_3,
                 "new_sites"=Q16_4,
                 "new_people"=Q16_5) %>%
   pivot_longer(cols=c("rare_species", "life_list", "monitoring",
                       "new_sites", "new_people"),
                names_to="characteristics",
                values_to="rank") %>%
   filter(complete.cases(rank),
          rank > 0 &
            rank <= 5) %>%
   mutate(birdwatcher_type=str_remove(Q9, "--.*")) %>%
   dplyr::select(-Q9))

# make characteristics a factor
stats_incountry$characteristics <- as.factor(stats_incountry$characteristics)

# make birdwatcher_type a factor
stats_incountry$birdwatcher_type <- as.factor(stats_incountry$birdwatcher_type)

# make rank numeric
stats_incountry$rank <- as.numeric(stats_incountry$rank)

# do casual significantly value exploring new sites compared to the other birdwatchers types?
mean((stats_incountry %>%
        filter(characteristics=="new_sites",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="new_sites",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_incountry %>%
        filter(characteristics=="new_sites",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="new_sites",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_incountry %>%
        filter(characteristics=="new_sites",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="new_sites",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="new_sites",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="new_sites",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="new_sites",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_incountry %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_incountry %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_incountry %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Hardcore birder "))$rank)


wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_incountry %>%
        filter(characteristics=="new_people",
               birdwatcher_type=="Enthusiastic birder "))$rank)
mean((stats_incountry %>%
        filter(characteristics=="new_people",
               birdwatcher_type=="Hardcore birder "))$rank)

sd((stats_incountry %>%
      filter(characteristics=="new_people",
             birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="new_people",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="new_people",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="new_people",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="new_people",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_incountry %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_incountry %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_incountry %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Hardcore birder "))$rank)


wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_incountry %>%
        filter(characteristics=="monitoring",
               birdwatcher_type=="Enthusiastic birder "))$rank)
mean((stats_incountry %>%
        filter(characteristics=="monitoring",
               birdwatcher_type=="Hardcore birder "))$rank)

sd((stats_incountry %>%
      filter(characteristics=="monitoring",
             birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_incountry %>%
      filter(characteristics=="monitoring",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="monitoring",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="monitoring",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_incountry %>%
                                             filter(characteristics=="monitoring",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

# Internationally -----------------------------------------------------------------

# Do you engage in birdwatching activities internationally?
(international_birdwatching <- survey %>% group_by(Q20) %>%
   filter(complete.cases(Q20)) %>% 
   summarize(percentage=n()/nrow(survey %>%
                                   filter(complete.cases(Q20)))*100))

# Rank reasons for selecting an international country to visit
duplicates <- function(i) {
  Q28survey <- survey %>% select(starts_with("Q28"))
  return(!any(duplicated(as.numeric(Q28survey[i,]))))
}

survey$correct_rankQ28 <- lapply(1:nrow(survey), duplicates)

answersQ28 <- survey %>% select(starts_with("Q28")) %>%
  filter_all(any_vars(!is.na(.)))

Q28survey <- survey %>%
  filter(correct_rankQ28==TRUE)

nrow(Q28survey)/(nrow(survey)-(nrow(survey)-nrow(answersQ28)))

(bird_div <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_1),
           Q28_1 > 0,
           Q28_1 <= 9, 
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_1)), sd=sd(as.numeric(Q28_1))) %>%
    mutate(question="Bird diversity") %>%
    rename(birdwatcher_type=Q9))

(rare_sp <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_2),
           Q28_2 > 0,
           Q28_2 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_2)), sd=sd(as.numeric(Q28_2))) %>%
    mutate(question="Rare bird species") %>%
    rename(birdwatcher_type=Q9))

(life_list_int <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_3),
           Q28_3 > 0,
           Q28_3 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_3)), sd=sd(as.numeric(Q28_3))) %>%
    mutate(question="Life list species") %>%
    rename(birdwatcher_type=Q9))

(tour_companies <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_4),
           Q28_4 > 0,
           Q28_4 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_4)), sd=sd(as.numeric(Q28_4))) %>%
    mutate(question="Good tour companies") %>%
    rename(birdwatcher_type=Q9))

(local_guides <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_5),
           Q28_5 > 0,
           Q28_5 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_5)), sd=sd(as.numeric(Q28_5))) %>%
    mutate(question="Good local guides") %>%
    rename(birdwatcher_type=Q9))

(safety <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_6),
           Q28_6 > 0,
           Q28_6 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_6)), sd=sd(as.numeric(Q28_6))) %>%
    mutate(question="Safety") %>%
    rename(birdwatcher_type=Q9))

(infrastructure <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_7),
           Q28_7 > 0,
           Q28_7 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_7)), sd=sd(as.numeric(Q28_7))) %>%
    mutate(question="Access to infrastructure") %>%
    rename(birdwatcher_type=Q9))

(lodging <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_8),
           Q28_8 > 0,
           Q28_8 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_8)), sd=sd(as.numeric(Q28_8))) %>%
    mutate(question="Lodging infrastructure") %>%
    rename(birdwatcher_type=Q9))

(other_activities <- Q28survey %>% group_by(Q9) %>%
    filter(complete.cases(Q28_10),
           Q28_10 > 0,
           Q28_10 <= 9,
           complete.cases(Q9)) %>% 
    summarize(mean=mean(as.numeric(Q28_10)), sd=sd(as.numeric(Q28_10))) %>%
    mutate(question="Non-birdwatching activities") %>%
    rename(birdwatcher_type=Q9))


international_birdwatching_rank <- rbind(bird_div, rare_sp, life_list_int, tour_companies, local_guides,
                                         safety, infrastructure, lodging, other_activities)

(international_rank <- ggplot(international_birdwatching_rank, aes(x = reorder(question, -mean), y = -mean, color = birdwatcher_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +  # Plot means
    geom_errorbar(aes(ymin = -mean - sd, ymax = -mean + sd),  # Plot error bars
                  position = position_dodge(width = 0.5), width = 0.5, size=1.5) +
    labs(title = "International",
         x = "", y = "Rank", color = "Birdwatcher Type") +
    theme_classic() +
    coord_flip() +  # Rotate x-axis labels for better readability 
    theme(text = element_text(size = 20)) +
    guides(color = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = c("indianred2", "gold3", "steelblue")) +
    scale_y_continuous(labels = c("Low Importance", "High Importance"),
                       breaks=c(-9,-1))+
    geom_hline(yintercept=-5, linetype="dashed")) 


(international_rank <- ggplot(international_birdwatching_rank, aes(x = reorder(question, -mean), y = -mean, color = birdwatcher_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +  # Plot means
    geom_errorbar(aes(ymin = -mean - sd, ymax = -mean + sd),  # Plot error bars
                  position = position_dodge(width = 0.5), width = 0.5, size=1.5) +
    labs(title = "Local",
         x = "", y = "Rank", color = "Birdwatcher Type") +
    theme_classic() +
    coord_flip() +  # Rotate x-axis labels for better readability 
    theme(text = element_text(size = 20)) +
    guides(color = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = c("indianred2", "gold3", "steelblue")) +
    geom_hline(yintercept=-3, linetype="dashed") +
    scale_y_continuous(limits=c(-10, 0), breaks=seq(-9,-1,1),
                       labels = function(x) abs(x)))

ggsave("Figures/Factor_Rank_international_Birdwatching.jpeg", height=8, width=10, units="in")

## Differences between groups ----------------------------------------------

# Run Wilcox test to see if birder category rankings are significantly different
# to start, create a data frame from all the data
(stats_internationally <- Q28survey %>% 
   dplyr::select(Q9, starts_with("Q28")) %>%
   dplyr::rename("bird_div"=Q28_1,
                 "rare_species"=Q28_2,
                 "life_list"=Q28_3,
                 "tour_companies"=Q28_4,
                 "local_guides"=Q28_5,
                 "safety"=Q28_6,
                 "infrastructure"=Q28_7,
                 "lodging"=Q28_8,
                 "other_activities"=Q28_10) %>%
   pivot_longer(cols=c("bird_div", "rare_species", "life_list", "tour_companies",
                       "local_guides", "safety", "infrastructure", "lodging",
                       "other_activities"),
                names_to="characteristics",
                values_to="rank") %>%
   filter(complete.cases(rank),
          rank > 0 &
            rank <= 9) %>%
   mutate(birdwatcher_type=str_remove(Q9, "--.*")) %>%
   dplyr::select(-Q9))

# make characteristics a factor
stats_internationally$characteristics <- as.factor(stats_internationally$characteristics)

# make birdwatcher_type a factor
stats_internationally$birdwatcher_type <- as.factor(stats_internationally$birdwatcher_type)

# make rank numeric
stats_internationally$rank <- as.numeric(stats_internationally$rank)

# do casual significantly value exploring new sites compared to the other birdwatchers types?
mean((stats_internationally %>%
        filter(characteristics=="bird_div",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="bird_div",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="bird_div",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="bird_div",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="bird_div",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="bird_div",
             birdwatcher_type=="Hardcore birder "))$rank)


wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="bird_div",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="bird_div",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="bird_div",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="rare_species",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="rare_species",
             birdwatcher_type=="Hardcore birder "))$rank)


wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="rare_species",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="life_list",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="life_list",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="life_list",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="tour_companies",
               birdwatcher_type=="Enthusiastic birder "))$rank)
mean((stats_internationally %>%
        filter(characteristics=="tour_companies",
               birdwatcher_type=="Hardcore birder "))$rank)

sd((stats_internationally %>%
      filter(characteristics=="tour_companies",
             birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="tour_companies",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="tour_companies",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="tour_companies",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="tour_companies",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))
mean((stats_internationally %>%
        filter(characteristics=="local_guides",
               birdwatcher_type=="Enthusiastic birder "))$rank)
mean((stats_internationally %>%
        filter(characteristics=="local_guides",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="local_guides",
             birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="local_guides",
             birdwatcher_type=="Hardcore birder "))$rank)


wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="local_guides",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="local_guides",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="local_guides",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="safety",
               birdwatcher_type=="Enthusiastic birder "))$rank)
mean((stats_internationally %>%
        filter(characteristics=="safety",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="safety",
             birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="safety",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="safety",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="safety",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="safety",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="infrastructure",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="infrastructure",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="infrastructure",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="infrastructure",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="infrastructure",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="infrastructure",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="infrastructure",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="infrastructure",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="infrastructure",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="lodging",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="lodging",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="lodging",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="lodging",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="lodging",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="lodging",
             birdwatcher_type=="Hardcore birder "))$rank)


wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="lodging",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="lodging",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="lodging",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

mean((stats_internationally %>%
        filter(characteristics=="other_activities",
               birdwatcher_type=="Casual birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="other_activities",
             birdwatcher_type=="Casual birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="other_activities",
               birdwatcher_type=="Enthusiastic birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="other_activities",
             birdwatcher_type=="Enthusiastic birder "))$rank)

mean((stats_internationally %>%
        filter(characteristics=="other_activities",
               birdwatcher_type=="Hardcore birder "))$rank)
sd((stats_internationally %>%
      filter(characteristics=="other_activities",
             birdwatcher_type=="Hardcore birder "))$rank)

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="other_activities",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="other_activities",
                                                    birdwatcher_type=="Enthusiastic birder " |
                                                      birdwatcher_type=="Casual birder ")))

wilcox.test(rank ~ birdwatcher_type, data=(stats_internationally %>%
                                             filter(characteristics=="other_activities",
                                                    birdwatcher_type=="Casual birder " |
                                                      birdwatcher_type=="Hardcore birder ")))

