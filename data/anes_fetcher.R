## ------- ADD URL/SOURCE OF DATA HERE -------

# https://electionstudies.org/data-center/

## ------- WHAT DATA IS THIS -------

# American National Election Studies:
# All cross-section cases and variables for select questions from the 
# ANES Time Series studies conducted since 1948. 

#################### GRAB DATA ####################
# load library
library(tidyverse)
library(here)

# add codes to grab data here
anes2020 <- read_csv(here("data/anes_timeseries_cdf_csv_20220916.csv"))
anes2024 <- read_csv(here("data/anes_timeseries_2024_csv_20250808.csv"))

anes_data <- bind_rows(anes2020, anes2024)

# add codes to view data

glimpse(anes_data)

anes_data

# codebook

anes_codebook <- tribble(
  ~variable,   ~label,
  "VERSION", "Version Number Of Release",
  "VCF9999", "Weight: post-election weight full sample",
  "VCF0013", "Completion - Post-election (flag)",
  "VCF0014", "Completion - Pre-election (flag)",
  "VCF0018a", "Language of Interview - Pre",
  "VCF0018b", "Language of Interview - Post",
  "VCF0070a", "Interviewer Gender - Pre",
  "VCF0070b", "Interviewer Gender - Post",
  "VCF0072a", "Interviewer Ethnicity - Pre",
  "VCF0072b", "Interviewer Ethnicity - Post",
  "VCF0127a", "Household - Who Belongs to Union 8-category",
  "VCF0127b", "Household - Who Belongs to Union 4-category",
  "VCF0302", "Party Identification of Respondent - Initial Party ID Response",
  "VCF0502a", "Which Major Party is More Conservative 4-Category",
  "VCF0705", "Vote for President - Major Parties and Other",
  "VCF0706", "Vote and Nonvote - President",
  "VCF0712", "Timing of Respondent’s Presidential Vote Decision",
  "VCF0731", "Respondent Discuss Politics with Family and Friends",
  "VCF0733", "How Often in the Last Week Respondent Discussed Politics",
  "VCF0736", "Vote for U.S. House - Party",
  "VCF0748", "Voted on Election Day or Before",
  "VCF0824", "If Compelled to Choose Liberal or Conservative",
  "VCF0842", "Environmental Regulation Scale",
  "VCF0846", "Is Religion Important to Respondent",
  "VCF0849", "Liberal-Conservative Position 1984 - COLLAPSED",
  "VCF0878", "Should Gays/Lesbians Be Able to Adopt Children",
  "VCF0900", "Congressional District of Residence",
  "VCF0901b", "State Postal Abbrev",
  "VCF0902", "Type of U.S. House Race",
  "VCF0903", "Is House Incumbent Running",
  "VCF0904", "Is House Incumbent Opposed",
  "VCF0905", "Number of Candidates in U.S. House Race",
  "VCF1015", "Number of Days Pre-Election IW Conducted Before Election",
  "VCF1016", "Number of Days Post-Election IW Conducted After Election",
  "VCF9001", "Length of Residence in Community",
  "VCF9002", "Length of Residence in Home",
  "VCF9008", "Which Party Would Best Handle Pollution and Protect Environment",
  "VCF9022", "Voter Strength of Preference - Presidential Candidate",
  "VCF9023", "Nonvoter Preference - Presidential Candidate",
  "VCF9025", "Vote for Governor - Party",
  "VCF9027", "Vote in Previous Presidential Election - Party",
  "VCF9031", "Contacted By Anyone Other than Parties",
  "VCF9054", "Senate Race in State",
  "VCF9055", "Type of U.S. Senate Race",
  "VCF9056", "Thermometer - Senate Democratic Candidate",
  "VCF9057", "Thermometer - Senate Republican Candidate",
  "VCF9060", "Thermometer - Senator in State with Senate Race",
  "VCF9069", "Strength Approve/Disapprove Running U.S. House Incumbent",
  "VCF9205", "Which Party Would Do Better Handling the Nation’s Economy",
  "VCF9217", "Approve/Disapprove President’s Handling of Foreign Relations",
  "VCF9218", "Approve/Disapprove President’s Handling of Health Care",
  "VCF9221", "CSES: Government Performance Over Last Few Years",
  "VCF9225", "Has Unemployment Gotten Better, Same, Worse in Past Year",
  "VCF9226", "Strength: Unemployment Better/Worse",
  "VCF9227", "Gap Between Rich and Poor vs. 20 Years Ago",
  "VCF9228", "Degree: Larger/Smaller Income Gap",
  "VCF9229", "Unemployment Outlook Next 12 Months",
  "VCF9231", "Favor/Oppose Limits on Imports",
  "VCF9234", "Abortion Placement - Democratic Presidential Candidate",
  "VCF9235", "Abortion Placement - Republican Presidential Candidate",
  "VCF9237", "Strength Favor/Oppose Death Penalty for Murder",
  "VCF9238", "Gun Laws: Easier, Harder, or Same",
  "VCF9239", "Importance of Gun Control Issue",
  "VCF9240", "CSES: Left-Right Self Placement (11-pt)",
  "VCF9241", "CSES: Left-Right Democratic Party Placement",
  "VCF9242", "CSES: Left-Right Republican Party Placement",
  "VCF9243", "Born-Again Christian Identification",
  "VCF9244", "How Often Can Respondent Trust People",
  "VCF9250", "CSES: Voting Makes a Big Difference (5-pt)",
  "VCF9265", "Pre-Election: Did Respondent Vote in Primary",
  "VCF9266", "Hispanic Rs: Politics Info From Spanish or English Media",
  "VCF9275", "In American Politics, Do Blacks Have Too Much/Right/Little Influence",
  "VCF9282", "Is Respondent Living with Family Members"
)


#################### SAVE DATASET INTO RDA ####################

save(anes_data, anes_codebook, file = here("data/anes_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda
