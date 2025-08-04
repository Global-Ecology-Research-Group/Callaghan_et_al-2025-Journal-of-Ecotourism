# Code to reproduce the results presented in "Birdwatcher's attitudes and preferences that influence
# their decisions to engage in local, national, and international birdwatching trips." Published in 
# the Journal of Ecotourism.

# Factor analysis of birdwatching trip/experience preferences

library(tidyverse)
library(lubridate)
library(likert)
library(psych)

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

# Likert Ananlysis and Figure ------------------------------------------------------------

# format data for the likert function
likert_data <- survey %>%
  filter(
    complete.cases(Q36_1),
    complete.cases(Q36_2),
    complete.cases(Q36_3),
    complete.cases(Q36_4),
    complete.cases(Q36_5),
    complete.cases(Q36_6),
    complete.cases(Q36_7),
    complete.cases(Q36_8),
    complete.cases(Q36_9),
    complete.cases(Q36_10),
    complete.cases(Q36_11)
  ) %>%
  rename(
    'High bird diversity' = Q36_1, 
    'Opportunities to see rare birds' = Q36_2,
    'Personal safety' = Q36_3,
    'Cost' = Q36_4,
    'Knowledgeable non-local tour guides' = Q36_5,
    "Knowledgeable local tour guides" = Q36_6,
    'Well planned itineraries' = Q36_7,
    'Bird photography' = Q36_8,
    'Bird blinds and feeders' = Q36_9,
    'Focus on conservation, sustainability, and support for local communities' = Q36_10,
    "Lodging infrastructure" = Q36_11
  ) %>%
  dplyr::select(
    'High bird diversity', 
    'Opportunities to see rare birds',
    'Personal safety',
    'Cost',
    'Knowledgeable non-local tour guides',
    "Knowledgeable local tour guides",
    'Well planned itineraries',
    'Bird photography',
    'Bird blinds and feeders',
    'Focus on conservation, sustainability, and support for local communities',
    "Lodging infrastructure"
  ) %>%
  mutate_all(~factor(., levels = c(
    "Not at all important",
    "Slightly important",
    "Moderately important",
    "Very important",
    "Extremely important"
  ))) %>%
  as.data.frame()

# run likert function
likert(likert_data)

# plot the data 
plot(likert(likert_data), xaxt = "n") +
  scale_fill_manual(values = c("indianred2", "lightsalmon", "lightgrey", "lightsteelblue", "steelblue"), 
                    breaks = c("Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left", 
        legend.margin = margin(l = -6, unit = "cm"), 
        text=element_text(size=13)) 

ggsave("Figures/US_only/Figure_4.jpeg", height=5, width=9, units="in")

# Factor Analysis ------------------------------------------------------------

# Use a factor analysis to determine enabling conditions (pull factors) versus
# deterring conditions (push factors)

# start by transforming categorical data into numeric format
likert_scale <- c("Not at all important" = 1, 
                  "Slightly important" = 2, 
                  "Moderately important" = 3, 
                  "Very important" = 4, 
                  "Extremely important" = 5)

likert_data_numeric <- data.frame(lapply(likert_data, function(x) likert_scale[x]))

# now perform factor analysis. Start by determining the number of factors
# using a scree plot
parallel_results <- fa.parallel(likert_data_numeric, fa="fa")
parallel_results

# get eigen values
parallel_results$fa.values

# we need to retain 4 factors based on this result. Next, let's see how we should group the data

# perform factor analysis on correlation matrix
cor_matrix <- cor(likert_data_numeric)

# perform factor analysis
factor_analysis_result <- factanal(covmat = cor_matrix, factors = 4, rotation = "varimax")

print(factor_analysis_result$loadings)
print(factor_analysis_result$correlation)
cor(factor_analysis_result$loadings)

# extract factor loadings
factor_loadings <- factor_analysis_result$loadings

# scale the original data
scaled_data <- scale(likert_data_numeric)

# calculate factor scores
factor_scores <- scaled_data %*% factor_loadings

# print factor scores
print(factor_scores)

# calculate squared multiple correlations (SMC)
SMC <- 1 - apply(cor_matrix, 2, function(x) sum(x^2))

# estimate commonalities
commonalities <- pmax(0, 1 - (1 - SMC), na.rm=TRUE)

# print commonalities
print(commonalities)

# extract factor loadings from the result
factor_loadings <- factor_analysis_result$loadings

# identify items associated with each factor based on non-zero loadings
factor_items <- lapply(1:ncol(factor_loadings), function(i) {
  items <- which(abs(factor_loadings[, i]) > 0.3)  
  colnames(likert_data_numeric)[items]
})

# calculate Cronbach's alpha for each factor
alpha.pa1 = alpha(likert_data_numeric[,factor_items[[1]]])
print(alpha.pa1, digits = 3)
# removing any of the values does not increase alpha

alpha.pa2 = alpha(likert_data_numeric[,factor_items[[2]]])
print(alpha.pa2, digits = 3)
# removing any of the values does not increase alpha

alpha.pa3 = alpha(likert_data_numeric[,factor_items[[3]]])
print(alpha.pa3, digits = 3)
# removing any of the values does not increase alpha

alpha.pa4 = alpha(likert_data_numeric[,factor_items[[4]]])
print(alpha.pa4, digits = 3)
# removing any of the values does not increase alpha

