#' Process and merge 2018 election results
#' Input:
#' - Indiana.csv
#' - Minnesota.txt (As of 2018-11-11, not up-to-date)
#' - Pennsylvania.CSV
#' - house-elections-2018.csv
#' Output: state_results_2018.rds

library(dplyr)

`%~%` <- function(lhs, rhs) grepl(rhs, lhs)

#' - Indiana.csv
read.csv('../2018-elections/raw-returns/Indiana.csv',
         stringsAsFactors = FALSE) %>% 
  filter(Office.Category == 'US Representative') %>% 
  mutate(state = 'Indiana',
         party = case_when(Political.Party %~% '^D' ~ 'D',
                           Political.Party %~% '^R' ~ 'R',
                           TRUE ~ 'O')
         ) %>%
  select(state,
         district = Jurisdiction.Name,
         county_name = Reporting.County.Name,
         candidate = Name.on.Ballot,
         party,
         county_votes = Total.Votes
         ) %>% 
  group_by(state, district, candidate, party) %>% 
  summarize(candidate_votes = sum(county_votes, na.rm = TRUE)) ->
  indiana_results

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
    district = V6,
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
  mutate(state = 'Minnesota',
         district = as.character(district),
         party = case_when(party_abbrev %~% '^D' ~ 'D',
                           party_abbrev %~% '^R' ~ 'R',
                           TRUE ~ 'Other')) %>% 
  group_by(state, district, candidate, party) %>% 
  summarize(candidate_votes = sum(vote_subtotal)) ->
  minnesota_results

#' - Pennsylvania.CSV
read.csv('../2018-elections/raw-returns/Pennsylvania.CSV',
         stringsAsFactors = FALSE) %>% 
  filter(Office.Name == 'Representative in Congress') %>% 
  mutate(state = 'Pennsylvania',
         district = stringr::str_extract(District.Name, '^\\d{1,2}'),
         candidate_last = stringr::str_replace(
           string = Candidate.Name,
           pattern = ',.*$',
           replacement = ''
         ),
         candidate_firstmiddle = stringr::str_replace(
           string = Candidate.Name,
           pattern = '^.*,',
           replacement = ''
         ),
         candidate = paste(candidate_firstmiddle, candidate_last) %>% 
           stringr::str_replace(
             pattern = 'JR',
             replacement = ''
           ),
         party = case_when(Party.Name %~% '^D' ~ 'D',
                           Party.Name %~% '^R' ~ 'R',
                           TRUE ~ 'O'),
         votes = readr::parse_number(Votes)
         ) %>% 
  group_by(state, district, candidate, party) %>% 
  summarize(candidate_votes = sum(votes)) ->
  pennsylvania_results

# Other results
# Data collected from Open Secrets and CNN
read.csv('data/house-elections-2018.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(party = case_when(party %~% '^D' ~ 'D',
                           party %~% '^R' ~ 'R',
                          TRUE ~ 'O'),
         district = as.character(district)) %>% 
  filter(!is.na(votes)) %>% 
  select(state, district, candidate, party, candidate_votes = votes) ->
  other_results

bind_rows(
  indiana_results,
  minnesota_results,
  pennsylvania_results,
  other_results
) %>% 
  mutate(year = 2018) ->
  house_district_2018

save(house_district_2018, file = 'data/house_district_2018.rda')
