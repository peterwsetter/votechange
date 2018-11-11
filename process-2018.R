#' Process and merge 2018 election results
#' Input:
#' - Indiana.csv
#' - Iowa.xls (File available but not possible to open with readxl)
#' - Minnesota.txt (As of 2018-11-11, not up-to-date)
#' - Michigan.xls
#' - North_Carolina.xlsx
#' - Ohio.xlsx
#' - Pennsylvania.CSV
#' To add:
#' - Arizona
#' - Florida
#' - New Hampshire
#' - Texas
#' - Wisconsin
#' Output: state_results_2018.rds

library(dplyr)

`%~%` <- function(lhs, rhs) grepl(rhs, lhs)

#' - Indiana.csv
read.csv('../2018-elections/raw-returns/Indiana.csv',
         stringsAsFactors = FALSE) %>% 
  filter(Office.Category == 'US Representative') %>% 
  mutate(house_district = paste0('IN-', Jurisdiction.Name),
         party = case_when(Political.Party %~% '^D' ~ 'D',
                           Political.Party %~% '^R' ~ 'R',
                           TRUE ~ 'O')
         ) %>% 
  select(house_district,
         county_name = Reporting.County.Name,
         candidate = Name.on.Ballot,
         party,
         county_votes = Total.Votes
         ) %>% 
  group_by(house_district, candidate, party) %>% 
  summarize(candidate_votes = sum(county_votes, na.rm = TRUE)) ->
  indiana_results

#' - Iowa.xls
#readxl::read_xlsx('../2018-elections/raw-returns/Iowa.xls') %>% 
#  View()

#' - Minnesota.txt
#' Getting data directly from Minnesota website

read.table(url('https://electionresults.sos.state.mn.us/Results/MediaResult/115?mediafileid=24'),
           sep = ';',
           stringsAsFactors = FALSE) %>% 
  select(
    #State
    state = V1, 
    #County ID (if applicable)
    county = V2,
    #Precinct name (if applicable)
    precinct = V3,
    #Office ID
    office = V4,
    #Office Name 
    office_name = V5,
    #District*
    district_num = V6,
    #Candidate Order Code
    candidate_order = V7,
    #Candidate Name (First/Last/Suffix all in one field)
    candidate = V8,
    #Suffix (not used)
    #Incumbent Code (not used)
    #Party Abbreviation
    party_abbrev = V11,
    #Number of Precincts reporting
    #Total number of precincts voting for the office
    #Votes for Candidate
    vote_subtotal = V14
    #Percentage of Votes for Candidate out of Total Votes for Office
    #Total number of votes for Office in area
  ) %>% 
  filter(office_name %~% 'U.S. Representative') %>% 
  mutate(district = paste0(state, '-', district_num),
         party = case_when(party_abbrev %~% '^D' ~ 'D',
                           party_abbrev %~% '^R' ~ 'R',
                           TRUE ~ 'Other')) %>% 
  group_by(district, candidate, party) %>% 
  summarize(candidate_votes = sum(vote_subtotal)) ->
  minnesota_results

#' - Michigan.xls
#' - North_Carolina.xlsx
#' - Ohio.xlsx
#' - Pennsylvania.CSV
