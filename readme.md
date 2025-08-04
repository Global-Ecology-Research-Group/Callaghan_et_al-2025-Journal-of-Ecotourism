This repository contains all the code and files used in the analyses reported in "Birdwatchers' attitudes and preferences that influence their decisions to engage in local, national, and international birdwatching trips" article, which is published in the Journal of Extension.

# R Folder

This folder contains five scripts, each corresponding to a specific results section in the main paper.

-   **1_Demographics_and_birdwatching_participation_frequency.R** generates the results reported in Sections 3.1 and 3.2.

-   **2_Variables_influencing_birdwatching_trips.R** produces the results in Section 3.3.

-   **3_Barriers_and_motivations_for_international_birdwatching_travel.R** corresponds to the results in Section 3.4.

-   **4_Factor_analysis_of_birdwatching_trip_or_experience_preference.R** produces the results in Section 3.5.

-   **5_Analysis_of_open_ended_questions.R** generates the results reported in Section 3.6.

In all scripts, we filter the data to only include respondents from the United States, as we have done in the main analysis of our paper. However, the code can be modified to look at all survey respondents or respondents from the United States which are enthusiastic or hard-core birders only, to recreate the supplemental analyses presented in the paper.

# Data folder

The data folder contains the survey data used in the analysis. It includes two files:

-   **Survey_Data_Raw.csv**: which contains the original, unaltered responses from participants.

-   **Final_Survey_Data.csv**: a cleaned version of the data set.

In the final file, we harmonized ethnicity and nationality categories, corrected country names, and added a new column, Q40_Themes, in which we manually categorized responses from the notes field into six themes, as described in the paper. This cleaned dataset (**Final_Survey_Data.csv**) was used for all analyses presented in the paper.

# Figures folder

This folder contains all figures presented in the paper.
