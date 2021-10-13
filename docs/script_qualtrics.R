# Date: YYYY-MM-DD
# Name: your name here
# Example: Single occasion survey

# Load data
library(tidyverse)
library(janitor)
library(skimr)

# read the file in the normal way and get just the names of the columns.
# put the names into the col_names
survey_file <- "data_qualtrics.csv"
col_names <- names(read_csv(survey_file,
                            n_max = 0,
                            show_col_types = FALSE))

# skip the first 3 rows than read in the data and use names from above
raw_data <- read_csv(survey_file,
                     col_names = col_names, 
                     skip = 3,
                     show_col_types = FALSE)

# list of Qualtrics columns you probably don't want
cols_to_maybe_remove <- c("StartDate",
                          "EndDate",
                          "Status",
                          "IPAddress",
                          "Progress",
                          "Duration (in seconds)",
                          "Finished",
                          "RecordedDate",
                          "ResponseId",
                          "RecipientLastName",
                          "RecipientFirstName",
                          "RecipientEmail",
                          "ExternalReference",
                          "LocationLatitude",
                          "LocationLongitude",
                          "DistributionChannel",
                          "UserLanguage")

# check to see overlap with actual column names in data file
# put names to remove (that are present) into cols_to_remove
cols_to_remove_id <- col_names %in% cols_to_maybe_remove
cols_to_remove <- col_names[cols_to_remove_id]

# remove the unwanted Qualtrics columns
raw_data <- raw_data %>% 
  select(!all_of(cols_to_remove))


analytic_data_survey <- raw_data

# Initial cleaning
## Convert column names to tidyverse style guide
## Remove empty rows and columns
analytic_data_survey <- analytic_data_survey %>%
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  clean_names()

glimpse(analytic_data_survey)

# Convert variables to factors as needed
## Convert sex to factor
analytic_data_survey <- analytic_data_survey %>%
  mutate(sex = as_factor(sex))


## Participant identification to factor
## Participant identification column must be created first
## get the number of rows in the data set
N <- dim(analytic_data_survey)[1] 

## create set of factor levels
participant_id_levels <- factor(seq(1, N))

## put the factor levels into a column called participant_id
analytic_data_survey <- analytic_data_survey %>%
  mutate(participant_id = participant_id_levels)

# Screen factors
## screen
analytic_data_survey %>%
  select(sex) %>%
  summary()

## change to desired order
analytic_data_survey <- analytic_data_survey %>%
  mutate(sex = fct_relevel(sex,
                           "female",
                           "intersex",
                           "male"))


# Screen numeric variables
analytic_data_survey %>%
  select(year_of_birth) %>%
  skim()




# Convert Commitment items to numeric  values
## Check levels for a likert7 item
analytic_data_survey %>%
  pull(aff_com1_likert7) %>%
  unique()

## write code to create ordered factor labels/levels
likert7_factor <- c("Strongly Disagree",
                    "Moderately Disagree",
                    "Slightly Disagree",
                    "Neither Agree nor Disagree",
                    "Slightly Agree",
                    "Moderately Agree",
                    "Strongly Agree")


## assign factor levels and then covert to numeric
analytic_data_survey <- analytic_data_survey %>%
  mutate(across(.cols = contains("likert7"), 
                .fns = ~ factor(.x, levels = likert7_factor))) %>%
  mutate(across(.cols = contains("likert7"), 
                .fns = as.numeric))


# Convert Job Satisfaction items to numeric  values
## Check levels for a likert5 item
analytic_data_survey %>%
  pull(job_aff1_likert5) %>%
  unique()

## write code to create ordered factor labels/levels
likert5_factor <- c("Strongly Disagree",
                    "Disagree",
                    "Neutral",
                    "Agree",
                    "Strongly Agree")

## assign factor levels and then covert to numeric
analytic_data_survey <- analytic_data_survey %>%
  mutate(across(.cols = contains("likert5"), 
                .fns = ~ factor(.x, levels = likert5_factor))) %>%
  mutate(across(.cols = contains("likert5"), 
                .fns = as.numeric))

# Reverse key items
## Reverse key likert7 items
analytic_data_survey <- analytic_data_survey %>% 
  mutate(8 - across(.cols = ends_with("_likert7rev")) ) %>% 
  rename_with(.fn = str_replace,
              .cols = ends_with("_likert7rev"),
              pattern = "_likert7rev",
              replacement = "_likert7")

## No likert5 items are reverse-keyed but if they were
## You would adapt the code above replacing 8 (one higher than 7) to 6 (one higher than 5)


# Create scale scores
## mutate commands create scale scores
## select commands with "-" remove items after scale creation
analytic_data_survey <- analytic_data_survey %>% 
  rowwise() %>% 
  mutate(affective_commitment = mean(c_across(starts_with("aff_com")),
                                     na.rm = TRUE)) %>%
  mutate(continuance_commitment = mean(c_across(starts_with("contin_com")),
                                       na.rm = TRUE)) %>%
  mutate(normative_commitment = mean(c_across(starts_with("norm_com")),
                                     na.rm = TRUE)) %>%
  mutate(job_satisfaction = mean(c_across(starts_with("job_aff")),
                                 na.rm = TRUE)) %>%
  ungroup() %>%
  select(-starts_with("aff_com")) %>%
  select(-starts_with("contin_com")) %>%
  select(-starts_with("norm_com")) %>%
  select(-starts_with("job_aff")) 


glimpse(analytic_data_survey)
