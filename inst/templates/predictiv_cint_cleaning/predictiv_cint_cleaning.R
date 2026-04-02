# This script cleans data collected on Predictiv through Cint.
# This is written for new Predictiv, June 2025.
# Export data from Predictiv via Data -> Download all data

# Last updated: Ailidh Finlayson, 17th December 2025

# Set up -------------------------------------------------------------------------------------------
# Load packages
library(tidyverse)
library(data.table)

# Point to the data folder
PATH_PROJ <- dirname(rstudioapi::getActiveDocumentContext()$path) # If this file to the same folder as your data, otherwise use
# PATH_PROJ <- "G:/.shortcut-targets-by-id/1xeoIymK6XBPXKQis_PMj3VkFOXVxbjbm/BIT project data/[Project Data UKXXXXXX.XXX] Project Name/"

# Load results
data <- setDT(read.csv(paste0(PATH_PROJ, "/", "[FILE NAME GOES HERE].csv")))

materialsPage <- 5 # Replace 5 with the page number of the RCT materials in your survey

# Cleaning for tests and duplicates ----------------------------------------------------------------
cat("\ntotal number of responses in your data set:\t", data[, .N])

# Check for and remove any entries where pid == test
# These will be any test entries end-to-end through Cint or through the 'Test survey' button on Predictiv
cat("\n'test' pid:\t", data[pid == "test", .N], "or",
    data[pid == "test", round(100*.N/nrow(data))], "%",
    "\n\tremoving leaves N =", data[pid != "test", .N])
data <- data[pid != "test"]

# Check for and remove duplicate 'ip_address' that isn't blank, retaining the first entry and
# dropping subsequent duplicates; IP addresses are hashed
cat("non-blank duplicated ip_address:\t", data[duplicated(ip_address) & ip_address != "", .N], "or",
    data[duplicated(ip_address) & ip_address != "", round(100*.N/nrow(data))], "%",
    "\n\tremoving leaves N =", data[!(duplicated(ip_address) & ip_address != ""), .N])
data <- data[!(duplicated(ip_address) & ip_address != "")]

# Check number of unique responses
cat("\nnumber of unique participants:\t\t", data[, .N])

# Check treatment allocation on unique responses
data[, table(treatment, useNA = "always")]

# Remove ineligible and inattentive participants ---------------------------------------------------
# Check for and remove ineligible participants
data <- data[!(is.na(eligible))] # Remove participants who didn't answer the questions to determine their eligibility
cat("\nN ineligible:\t\t", data[eligible == 0, .N], "or",
    data[eligible == 0, round(100*.N/nrow(data))], "%",
    "\nIR:\t\t\t", data[eligible == 1, round(100*.N/nrow(data))], "%", # If your IR doesn't match Cint, update Cint
    "\n\tremoving leaves N =", data[eligible == 1, .N])
data <- data[eligible == 1]

# Check for and remove inattentive participants
data <- data[!(is.na(attention))] # Remove participants who didn't answer the attention check
cat("\nN inattentive:\t\t", data[attention == 0, .N], "or",
    data[attention == 0, round(100*.N/nrow(data))], "%",
    "\n\tremoving leaves N =", data[attention == 1, .N])
data <- data[attention == 1]

# Recoding Cint variables --------------------------------------------------------------------------
# Age
# Qualification: AGE
data[, table(age, useNA = "always")]
data[, ageCat := cut(as.numeric(age), breaks = c(17, 24, 34, 44, 54, 64, Inf),
                     labels = c("18 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 and over"))]
data[, table(ageCat, useNA = "always")]

# Gender
# Qualification: GENDER_PLUS
data[, table(gender, useNA = "always")]
data[, gender := recode(as.numeric(gender), "Male", "Female", "Other")]
data[, table(gender, useNA = "always")]

# UK region
# Qualification: Fulcrum_Region_UK_NUTS_I
data[, table(location, useNA = "always")]
data[, location := as.numeric(location)]
data[, locationCat := case_when(location %in% c(4, 5, 12) ~ "North",
                                location %in% c(2, 8, 9) ~ "South and East",
                                location == 3 ~ "London",
                                location %in% c(10, 7, 6) ~ "Wales, Scotland and Northern Ireland",
                                location %in% c(1, 11) ~ "Midlands",
                                TRUE ~ NA_character_)]
data[, table(data$locationCat, useNA = "always")]
data[, locationFull := recode(as.numeric(location),
                              "East Midlands",
                              "East of England",
                              "London",
                              "North East",
                              "North West",
                              "Northern Ireland",
                              "Scotland",
                              "South East",
                              "South West",
                              "Wales",
                              "West Midlands",
                              "Yorkshire and the Humber")]
data[, table(data$locationFull, useNA = "always")]

# Income
# Qualification: STANDARD_HHI
## Note to Windows users: You'll see "£" instead of "?" (this standalone pound symbol appears as a
## question-mark to Mac users).
## Note to Mac users: You'll see just a pound symbol.
data[, table(income, useNA = "always")]
data[, incomeCat := cut(as.numeric(income), breaks = c(-Inf, 13, 24, Inf),
                        labels = c("Less than £40,000", "£40,000 and over", "Prefer not to say"))]
data[, table(data$incomeCat, useNA = "always")]
data[, incomeFull := recode(as.numeric(income),
                            "Less than £5,000",
                            "£5,000 to £9,999",
                            "£10,000 to £14,999",
                            "£15,000 to £17,499",
                            "£17,500 to £19,999",
                            "£20,000 to £22,499",
                            "£22,500 to £24,999",
                            "£25,000 to £27,499",
                            "£27,500 to £29,999",
                            "£30,000 to £32,499",
                            "£32,500 to £34,999",
                            "£35,000 to £37,499",
                            "£37,500 to £39,999",
                            "£40,000 to £42,499",
                            "£42,500 to £44,999",
                            "£45,000 to £47,499",
                            "£47,500 to £49,999",
                            "£50,000 to £54,999",
                            "£55,000 to £59,999",
                            "£60,000 to £64,999",
                            "£65,000 to £69,999",
                            "£70,000 to £74,999",
                            "£75,000 to £99,999",
                            "£100,000 and above",
                            "Prefer not to answer")]
data[, table(incomeFull, useNA = "always")]

# Education
# Qualification: STANDARD_EDUCATION_v2
data[, table(education, useNA = "always")]
data[, educationCat := cut(as.numeric(education), breaks = c(-Inf, 0, 5, Inf),
                           labels = c("None of the above", "No degree", "Degree"))]
data[, table(educationCat, useNA = "always")]
data[ education == -3105, education := 9] # None of the above
data[, educationFull := recode(education,
                               "Below O-level / GCSE",
                               "O-levels / GCSEs or equivalent",
                               "A-levels or equivalent",
                               "Further qualification (between high school and university)",
                               "Completed some university, but no degree",
                               "University degree",
                               "Master's or professional degree",
                               "Post graduate: PhD",
                               "None of the above")]
data[, table(data$educationFull, useNA = "always")]

# Ethnicity
# Qualification: STANDARD_UK_ETHNICITY
data[, table(ethnicity, useNA = "always")]
data[, ethnicityFull := recode(as.numeric(ethnicity),
                               "White",
                               "Traveller/Irish Traveller",
                               "Asian or Asian British: Indian",
                               "Asian or Asian British: Pakistani",
                               "Asian or Asian British: Bangladeshi",
                               "Asian or Asian British: Chinese",
                               "Asian or Asian British: Other Asian",
                               "Black or Black British",
                               "Mixed or Multiple",
                               "Other")]
data[, table(ethnicityFull, useNA = "always")]
data[, ethnicityCat := case_when(ethnicityFull == "Traveller/Irish Traveller" ~ "White",
                                 str_detect(ethnicityFull, "Asian") ~ "Asian",
                                 ethnicityFull == "Black or Black British" ~ "Black",
                                 ethnicityFull == "Mixed or Multiple" ~ "Other",
                                 TRUE ~ ethnicityFull)]
data[, table(ethnicityCat, useNA = "always")]

# Urbanicity
# Qualification: STANDARD_URBAN_RURAL
data[, table(urban, useNA = "always")]
data[, urbanCat := recode(as.numeric(urban), "Urban", "Suburban", "Rural")]
data[, table(urbanCat, useNA = "always")]

# Employment status
# Qualification: STANDARD_EMPLOYMENT
data[, table(employment, useNA = "always")]
data[, employmentFull := recode(as.numeric(employment),
                                "Employed full-time",
                                "Employed part-time",
                                "Self-employed full-time",
                                "Self-employed part-time",
                                "Active military",
                                "Inactive military/Veteran",
                                "Temporarily unemployed",
                                "Full-time homemaker",
                                "Retired",
                                "Student",
                                "Disabled",
                                "Prefer not to say",
                                "Parental leave",
                                "Leave of absence",
                                "Unable to work",
                                "Other type of paid work",
                                "Bringing up a family",
                                "Unpaid full-time carer",
                                "Working unpaid",
                                "In-between jobs / looking for work",
                                "Pursuing own goals")]
data[, table(employmentFull, useNA = "always")]
data[, employmentCat := case_when(employmentFull %in% c("Employed full-time",
                                                        "Employed part-time",
                                                        "Self-employed full-time",
                                                        "Self-employed part-time",
                                                        "Active military",
                                                        "Parental leave",
                                                        "Leave of absence",
                                                        "Other type of paid work") ~ "Employed",
                                  employmentFull %in% c("Temporarily unemployed",
                                                        "In-between jobs / looking for work") ~ "Unemployed",
                                  employmentFull %in% c("Inactive military/Veteran",
                                                        "Full-time homemaker",
                                                        "Bringing up a family",
                                                        "Retired",
                                                        "Student",
                                                        "Disabled",
                                                        "Unable to work",
                                                        "Working unpaid",
                                                        "Unpaid full-time carer",
                                                        "Pursuing own goals") ~ "Inactive")]
data[, table(data$employmentCat, useNA = "always")]

# Differential attrition --------------------------------------------------
# Differential attrition is checked by running a logistic regression of
# completion on treatment
# The model should use the same covariates you will use in your primary analysis
# and run the same comparisons as in your primary comparison
data[, completedBin := ifelse(current_page == "completed", 1, 0)]
data[, currentpage_numeric := as.numeric(ifelse(current_page == "completed",
                                                max(data[current_page != "completed",
                                                         as.numeric(current_page)]) + 1,
                                                current_page))]

reg_vars <- c("ageCat", "gender", "locationCat", "incomeCat", "ethnicityCat") # Update with relevant covariates
data[, (reg_vars) := lapply(.SD, as.factor), .SDcols = reg_vars]
data[currentpage_numeric >= materialsPage,
     summary(glm(completedBin ~ as.factor(treatment) + ageCat + gender + locationCat + incomeCat + ethnicityCat,
                 family = "binomial"))] # Update with relevant covariates

# Also look at a raw descriptive table of attrition by treatment arm
data[, table(treatment, completedBin)]
data[, prop.table(table(treatment, completedBin), margin=1)]

# Subset to complete data --------------------------------------------------------------------------
# Check for and remove incomplete data
cat("\nN dropped out:\t\t", data[current_page != "completed", .N], "or",
    data[current_page != "completed", round(100*.N/nrow(data))], "%",
    "\n\tremoving leaves N =", data[current_page == "completed", .N])
data <- data[current_page == "completed"]

# Time spent ---------------------------------------------------------------------------------------
# Total time spent in the survey
cat("\nmedian interview time: \t", data[, floor(median(interview_time) / 60)], "minutes and ",
    data[, round(median(interview_time) %% 60, 0)], "seconds") # If this doesn't match your intro screen and Cint, update both

data[, .(N_respondents = .N,
         median_time = median(interview_time/60)),
     by = treatment][order(treatment)] # Number of respondents and median time spent by treatment
data[, summary(lm(interview_time ~ as.factor(treatment)))] # Does completion time vary by treatment?

# Check average time in minutes by supplier. Is anyone speeding through?
data[, .(N_respondents = .N,
         mean_time = mean(interview_time/60, na.rm = TRUE),
         median_time = median(interview_time/60, na.rm = TRUE)),
     by = supname][order(median_time)]

# Time spent on RCT materials
data[, materialsTime := get(paste0("page", materialsPage, "_interview_time"))]
data[, median(materialsTime)] # Median time spent on RCT materials in seconds
data[, .(N_respondents = .N,
         median_time = median(materialsTime)),
     by = treatment][order(treatment)] # Number of respondents and median time spent by treatment
data[, summary(lm(materialsTime ~ as.factor(treatment)))] # Does this vary by treatment?

# Remove speeders ----------------------------------------------------------------------------------
# We remove speeders who complete the survey in less than 1/3 of the median time for that treatment arm
data[, medTime := median(interview_time), by = treatment] # Calculate median time for each treatment
cat("\nN speeders:\t\t", data[interview_time < (medTime / 3), .N], "or",
    data[interview_time < (medTime / 3), round(100*.N/nrow(data))], "%",
    "\n\tremoving leaves N =", data[!(interview_time < (medTime / 3)), .N])
data <- data[!(interview_time < (medTime / 3))]

# Check missingness on treatment on Cint variables -------------------------------------------------
VARS <- c("age", "gender", "location", "income", "ethnicity", "education", "urban", "employment")
data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = VARS] # Check by variable

cat("\nN with missing data from Cint:\t\t", data[, length(which(rowSums(is.na(.SD)) > 0)), .SDcols = VARS],
    "or", data[, round(length(which(rowSums(is.na(.SD)) > 0)) / .N * 100, 0), .SDcols = VARS], "%",
    "\n\tremoving leaves N =", data[, .N - length(which(rowSums(is.na(.SD)) > 0)), .SDcols = VARS])
data <- na.omit(data, cols = VARS)

# Final data set summary ---------------------------------------------------------------------------
cat("\nFINAL N: \t\t", data[, .N])

data[, .(N_respondents = .N),
     by = treatment][order(treatment)] # Number of respondents by treatment

# Recode device used - calculated based on screensize recorded on Predictiv
data[, deviceCat := ifelse(screen_width < screen_height & screen_width < 768, "Mobile",
                           ifelse(screen_height < screen_width & screen_height < 768, "Mobile",
                                  "Tablet or Laptop"))]
data[, table(deviceCat, useNA = "always")]


# Remove unnecessary columns -----------------------------------------------------------------------
# These variables are usually only used for cleaning or internal use only; feel free to edit this
# list to keep any of these in your data if they're relevant to your analysis
REMOVE <- c("status", "rid", "pid", "risn", "supname", "ip_address", "qrid", "start_date",
            "submitted_at", "current_page", "screen_width", "screen_height", "referer", "turtle",
            "location", "income", "education", "employment", "ethnicity", "urban", "browser",
            "oenc2", "eligible", "AttCheck1_P1", "AttCheck1_P2", "AttCheck1_F1", "AttCheck1_F2",
            "AttCheck1_F3", "AttCheck1Passed", "AttCheck2_P1", "AttCheck2_P2", "AttCheck2_F1",
            "AttCheck2_F2", "AttCheck2_F3", "AttCheck2Passed", "attention", "completedBin",
            "currentpage_numeric", grep("_interview_time", names(data), value = TRUE))

data <- data[, .SD, .SDcols = !REMOVE]

# Check column names
names(data)

# Check data ---------------------------------------------------------------------------------------
# Things to check for: is everyone responding to questions where they should or do you have any
# NA's, check distribution for numeric variables
summary(data)
View(data)

# Save cleaned data --------------------------------------------------------------------------------
write.csv(data, paste0(PATH_PROJ, "/", "cleaned data.csv"))
