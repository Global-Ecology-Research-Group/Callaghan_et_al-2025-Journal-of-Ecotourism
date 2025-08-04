# Code to reproduce the results presented in "Birdwatcher's attitudes and preferences that influence
# their decisions to engage in local, national, and international birdwatching trips." Published in 
# the Journal of Ecotourism.

# Demographics and birdwatching participation frequency

library(tidyverse)
library(lubridate)
library(FSA)

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

# Age ------------------------------------------------------------

(age <- survey %>% group_by(Q2) %>%
   mutate(Q2 = if_else(Q2 == '65 or above', '65+', Q2)) %>%
   filter(complete.cases(Q2)) %>% 
   summarize(percentage=n()/nrow(survey %>%
                                   filter(complete.cases(Q2)))*100))

ggplot(data=age, aes(x=Q2, y=percentage)) + 
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + xlab("Age") + ylab("Percentage")

# does age influence birdwatcher type?
vci <- data.frame(age=as.factor(survey$Q2),
                  birdwatcher_type=factor(survey$Q9)) %>%
  filter(complete.cases(age), complete.cases(birdwatcher_type)) %>%
  as.data.frame()

vci_table <- table(vci$age, vci$birdwatcher_type)

# we will fun a Chi-Square Test since we are comparing one categorical group to another
chi_square_test <- chisq.test(vci_table)
print(chi_square_test)

# Gender ------------------------------------------------------------

# combine non-binary and perfer not to say
(gender <- survey %>% group_by(Q3) %>%
    filter(complete.cases(Q3)) %>% 
    summarize(percentage=n()/nrow(survey %>% filter(complete.cases(Q3)))*100))

ggplot(data=gender, aes(x=Q3, y=percentage)) + 
  geom_bar(stat="identity", fill="steelblue") +
  theme_classic() + xlab("Gender") + ylab("Percentage")

# does gender influence birdwatcher type?
vci <- data.frame(gender=as.factor(survey$Q3),
                  birdwatcher_type=factor(survey$Q9)) %>%
  filter(complete.cases(gender), complete.cases(birdwatcher_type)) %>%
  as.data.frame()

vci_table <- table(vci$gender, vci$birdwatcher_type)

# Perform Fisher's Exact Test since we have small categories
fisher_test <- fisher.test(vci_table, workspace = 2e8, hybrid = TRUE)
print(fisher_test)

# Nationality ------------------------------------------------------------

(country_nat <- survey %>% group_by(Q1) %>%
    filter(complete.cases(Q1)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q1)))*100))

ggplot(data=country_nat, aes(x=reorder(Q1, percentage), y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Country of Nationality") + ylab("Percentage")


# Education ------------------------------------------------------------

(education <- survey %>% group_by(Q5) %>%
    mutate(Q5=factor(Q5, levels=c("Doctorate (PhD) or higher",
                                  "Master's degree",
                                  "Bachelor's degree/ Undergraduate degree",
                                  "High school or equivalent",
                                  "Other (please specify)"))) %>%
    filter(complete.cases(Q5)) %>% 
    summarize(percentage=n()/nrow(survey %>% filter(complete.cases(Q5)))*100))

# write in answers for "Other"
unique(survey$Q5_5_TEXT)

ggplot(data=education, aes(x=Q5, y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Highest Level of Education") + ylab("Percentage")

# does education influence birdwatcher type?
vci <- data.frame(education=as.factor(survey$Q5),
                  birdwatcher_type=factor(survey$Q9)) %>%
  filter(complete.cases(education), complete.cases(birdwatcher_type)) %>%
  as.data.frame()

vci_table <- table(vci$education, vci$birdwatcher_type)

# we will fun a Chi-Square Test since we are comparing one categorical group to another
chi_square_test <- chisq.test(vci_table)
print(chi_square_test)

# Country of residence ------------------------------------------------------------

(country_current <- survey %>% group_by(country) %>%
    filter(complete.cases(country)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(country)))*100))

ggplot(data=country_current, aes(x=reorder(country, percentage), y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Current Residence") + ylab("Percentage")

# Ethnicity ------------------------------------------------------------

(ethnicity <- survey %>% group_by(Q37...85) %>%
    filter(complete.cases(Q37...85)) %>% 
    summarize(percentage=n()/nrow(survey%>%
                                    filter(complete.cases(Q37...85)))*100) %>%
    arrange(desc(percentage)))

ggplot(data=ethnicity, aes(x=reorder(Q37...85, percentage), y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Ethnicity") + ylab("Percentage")

# Household income ------------------------------------------------------------

(income <- survey %>% group_by(Q7) %>%
    mutate(Q7=factor(Q7, levels=c("Over $600,000",
                                  "$400,000-$600,000",
                                  "$200,000-$400,000",
                                  "$100,000 - $200,000",
                                  "$50,000-$100,000",
                                  "Less than $50,000"))) %>%
    filter(complete.cases(Q7)) %>%
    summarize(count=n()) %>%
    mutate(percentage = round(count / sum(count) * 100, 1)) %>%
    ungroup())

ggplot(data=income, aes(x=Q7, y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Income") + ylab("Percentage")

# does household income influence birdwatching type?
vci <- data.frame(income=as.factor(survey$Q7),
                  birdwatcher_type=factor(survey$Q9)) %>%
  filter(complete.cases(income), complete.cases(birdwatcher_type)) %>%
  as.data.frame()

vci_table <- table(vci$income, vci$birdwatcher_type)

# we will fun a Chi-Square Test since we are comparing one categorical group to another
chi_square_test <- chisq.test(vci_table)
print(chi_square_test)

# Years of birdwatching experience ------------------------------------------------------------

(years_experience <- survey %>% group_by(Q8) %>%
    mutate(Q8=factor(Q8, levels=c("More than 10 years",
                                  "6-10 years",
                                  "1-5 years",
                                  "Less than 1 year"))) %>%
    filter(complete.cases(Q8)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q8)))*100))

ggplot(data=years_experience, aes(x=Q8, y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Years of Birdwatching Experience") + ylab("Percentage")

# does years of experience influence birdwatching type?
vci <- data.frame(experience=as.factor(survey$Q8),
                  birdwatcher_type=factor(survey$Q9)) %>%
  filter(complete.cases(experience), complete.cases(birdwatcher_type)) %>%
  as.data.frame()

vci_table <- table(vci$experience, vci$birdwatcher_type)

# Perform Fisher's Exact Test since we have small categories
fisher_test <- fisher.test(vci_table, workspace = 2e8, hybrid = TRUE)
print(fisher_test)

# Type of birdwatcher ------------------------------------------------------------

survey$Q9 <- sub("\\--.*", "", survey$Q9)
(birdwatcher_type <- survey %>% group_by(Q9) %>%
    mutate(Q9=factor(Q9, levels=c("Hardcore birder ",
                                  "Enthusiastic birder ",
                                  "Casual birder "))) %>%
    filter(complete.cases(Q9)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q9)))*100))

ggplot(data=birdwatcher_type, aes(x=Q9, y=percentage)) + geom_bar(stat="identity") +
  coord_flip() + theme_classic() + xlab("Type of Birdwatcher") + ylab("Percentage")

# Distribution of engagement locally, in-country, and internationally (Figure 1) ----------------------

# Do you engage in birdwatching activities locally?
(local_birdwatching_sum <- survey %>% group_by(Q10) %>%
   filter(complete.cases(Q10)) %>% 
   summarize(percentage=n()/nrow(survey %>% 
                                   filter(complete.cases(Q10)))*100) %>%
   rename(engage=Q10))

(local_birdwatching <- survey %>% group_by(Q10, Q9) %>%
    filter(complete.cases(Q10), complete.cases(Q9)) %>% 
    summarize(percentage=n()/nrow(survey %>% 
                                    filter(complete.cases(Q10), complete.cases(Q9)))*100,
              count=n()) %>%
    mutate(spatial="Locally") %>%
    rename(engage=Q10, birdwatcher_type=Q9))

# Do you engage in birdwatching activities in-country?
(incountry_birdwatching_sum <- survey %>% group_by(Q13) %>%
    filter(complete.cases(Q13)) %>% 
    summarize(percentage=n()/nrow(survey %>% 
                                    filter(complete.cases(Q13)))*100) %>%
    rename(engage=Q13))

(incountry_birdwatching <- survey %>% group_by(Q13, Q9) %>%
    filter(complete.cases(Q13), complete.cases(Q9)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q13), complete.cases(Q9)))*100,
              count=n()) %>%
    mutate(spatial="In-County") %>%
    rename(engage=Q13, birdwatcher_type=Q9))

# Do you engage in birdwatching activities internationally?
(international_birdwatching_sum <- survey %>% group_by(Q20) %>%
    filter(complete.cases(Q20)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q20)))*100) %>%
    rename(engage=Q20))

(international_birdwatching <- survey %>% group_by(Q20, Q9) %>%
    filter(complete.cases(Q20), complete.cases(Q9)) %>% 
    summarize(percentage=n()/nrow(survey %>%
                                    filter(complete.cases(Q20), complete.cases(Q9)))*100, 
              count=n()) %>%
    mutate(spatial="Internationally") %>%
    rename(engage=Q20, birdwatcher_type=Q9))

# combine data 
birdwatching_engagement <- rbind(local_birdwatching, incountry_birdwatching, international_birdwatching)

# get total per spatial group
(total_spatial <- birdwatching_engagement %>%
    group_by(spatial, engage) %>%
    summarize(count=sum(count))  %>%
    group_by(spatial) %>%
    mutate(percentage = count / sum(count) * 100,
           birdwatcher_type="Combined "))

birdwatching_engagement <- rbind(birdwatching_engagement, total_spatial)

# customize order of birdwatcher type and spatial by making that column a factor
birdwatching_engagement$birdwatcher_type <- factor(birdwatching_engagement$birdwatcher_type, 
                                                   levels=c("Hardcore birder ",
                                                            "Enthusiastic birder ",
                                                            "Casual birder ",
                                                            "Combined "))

birdwatching_engagement$spatial <- factor(birdwatching_engagement$spatial, 
                                          levels=c("Locally",
                                                   "In-County",
                                                   "Internationally"
                                          ))

ggplot(birdwatching_engagement, aes(x = birdwatcher_type, y = percentage, fill = engage)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Do you engage in birdwatching activities?", x = "", y = "Percentage",
       fill="") +
  scale_fill_manual(values = c("indianred2", "steelblue2")) +
  facet_grid(spatial ~ ., scales = "free_y", switch = "y") +
  theme_classic() +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 20))


ggsave("Figures/Figure_1.jpeg", height=7, width=9, units="in")

# Number of countries visited ---------------------------------------------

# How many countries have you visited for birdwatching?
(countries_bw <- survey %>% 
   filter(complete.cases(Q25)) %>%
   group_by(Q25) %>%
   summarize(count=n()) %>%
   arrange(desc(count)))

# summarize countries visited by birdwatcher type
(countries_bw2 <- survey %>% group_by(Q9) %>%
    dplyr::mutate(Q25 = as.numeric(Q25)) %>%
    filter(complete.cases(Q25),
           complete.cases(Q9), 
           Q25 > 0) %>% 
    summarize(average=mean(Q25, na.rm = TRUE), sd=sd(Q25),
              median=median(Q25, na.rm=TRUE), min=min(Q25, na.rm=TRUE),
              max=max(Q25, na.rm=TRUE)))

# calculate for all the data
survey %>% 
  dplyr::mutate(Q25 = as.numeric(Q25)) %>%
  filter(complete.cases(Q25),
         complete.cases(Q9), 
         Q25 > 0) %>% 
  summarize(average=mean(Q25, na.rm = TRUE), sd=sd(Q25),
            median=median(Q25, na.rm=TRUE), min=min(Q25, na.rm=TRUE),
            max=max(Q25, na.rm=TRUE))

# lets calculate this for all data 
mean(as.numeric(survey$Q25), na.rm=TRUE)
sd(as.numeric(survey$Q25), na.rm=TRUE)
median(as.numeric(survey$Q25), na.rm=TRUE)

(countries_bw2 <- survey %>% 
    dplyr::mutate(Q25 = as.numeric(Q25)) %>%
    filter(complete.cases(Q25),
           complete.cases(Q9)) %>%
    filter(Q25 > 0) %>%
    summarize(average=mean(Q25, na.rm = TRUE), sd=sd(Q25),
              median=median(Q25, na.rm=TRUE), min=min(Q25, na.rm=TRUE),
              max=max(Q25, na.rm=TRUE)))

ggplot(data=countries_bw, aes(x=as.numeric(Q25), y=count)) + geom_bar(stat="identity") +
  theme_classic() + theme(axis.title.y=element_blank()) + xlab("Number of Countires") + 
  ylab("Count")

int_par <- countries_bw %>%
  filter(as.numeric(Q25) > 0)

summary(as.numeric(int_par$Q25))

# does annual income influence the number of countries birdwatchers travel to?
hist(as.numeric(survey$Q25), na.rm=TRUE) # continuous data is left skewed
vci <- data.frame(log_countries=log(as.numeric(survey$Q25)+1),
                  countries=as.numeric(survey$Q25),
                  income=factor(survey$Q7, levels=c("Over $600,000",
                                                    "$400,000-$600,000",
                                                    "$200,000-$400,000",
                                                    "$100,000 - $200,000",
                                                    "$50,000-$100,000",
                                                    "Less than $50,000")),
                  birdwatcher_type=as.factor(survey$Q9)) %>%
  filter(complete.cases(countries), complete.cases(income), complete.cases(birdwatcher_type))

# check if the log transformation made the data normally distributed
hist(vci$log_countries)
qqnorm(vci$log_countries)
qqline(vci$log_countries)
shapiro_test <- shapiro.test(vci$log_countries)
print(shapiro_test)

# the data is not normally distributed, so let's try using a non-parametric test - Kruskal-Wallis Test
kruskal_test <- kruskal.test(countries ~ income, data=vci)
print(kruskal_test)

# If the Kruskal-Wallis test is significant, perform post hoc tests
if(kruskal_test$p.value < 0.05) {
  dunn_test <- dunnTest(countries ~ income, data = vci, method = "bh")
  print(dunn_test)
}

# Print mean ranks for interpretation
mean_ranks <- aggregate(countries ~ income, data = vci, FUN = mean)
print(mean_ranks)

# how does it differ by birdwatcher type?
# the data is not normally distributed, so let's try using a non-parametric test - Kruskal-Wallis Test
kruskal_test <- kruskal.test(countries ~ birdwatcher_type, data=vci)
print(kruskal_test)

# If the Kruskal-Wallis test is significant, perform post hoc tests
dunn_test <- dunnTest(countries ~ birdwatcher_type, data = vci, method = "bh")
print(dunn_test)

# Print mean ranks for interpretation
mean_ranks <- aggregate(countries ~ birdwatcher_type, data = vci, FUN = mean)
print(mean_ranks)

# median countries visited
median(vci[vci$countries>1,]$countries)
summary(vci[vci$countries>1,]$countries)
