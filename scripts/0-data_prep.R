# Data manipulation for CAnD3 presentation

library(dplyr)
library(magrittr)
library(forcats)
library(tidyr)
library(kableExtra)
library(emoji)


# Read 2021 data 

multiple_2021 = read.csv("data/kaggle_survey_2021_responses.csv")


df = multiple_2021 %>% select(Q1, Q2, Q3, Q4, Q5, Q6, starts_with("Q7"), Q15, Q20, 
                         starts_with("Q24"), Q25, Q41) %>% 
  rename(age = Q1, 
         gender = Q2, 
         country = Q3, 
         edu = Q4, 
         position = Q5, 
         code_yrs = Q6,
         Python = Q7_Part_1,
         R = Q7_Part_2, 
         SQL = Q7_Part_3,
         C = Q7_Part_4, 
         `C++` = Q7_Part_5,
         Java = Q7_Part_6, 
         JavaScript = Q7_Part_7,
         Julia = Q7_Part_8,
         TypeScript = Q7_Part_8, 
         Swift = Q7_Part_9, 
         Bash = Q7_Part_10,
         MATLAB = Q7_Part_11, 
         None = Q7_Part_12,
         yrs_ML = Q15, 
         industry = Q20, 
         Analysis = Q24_Part_1, 
         Infrastructure = Q24_Part_2,
         `Built ML prototype` = Q24_Part_3, 
         `Build ML models` = Q24_Part_4, 
         `Improve ML models` = Q24_Part_5, 
         `Research ML models` = Q24_Part_6, 
         `N/A ML` = Q24_Part_7,
         compensation_USD = Q25,
         primary_tool = Q41) 

df = df[-1,]


