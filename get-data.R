#' Copyright Peter W Setter 2018
#' Get data for analysis
#' Data to obtain:
#' - 2016 Congressional District Vote Totals
#' - 2018 Congressional District Vote Totals
#' - 2016 Presidential Vote Totals by District
#' - 538 Trump Scores

# Libraries
library(dplyr)
library(rvest)

## 2016 Results are available as an R package from MIT Election Data and Science Lab
if(!('elections' %in% installed.packages()[, 'Package'])) {
  devtools::install_github('MEDSL/elections')
}

## Preliminary 2018 results are available in txt files also from MEDSL
system('git clone https://github.com/MEDSL/2018-elections.git')

## Pull the most recent results
system('cd 2018-elections && git pull')

########
## Get Trump Scores for House
trump_scores_html <- read_html('https://projects.fivethirtyeight.com/congress-trump-score/house/')

trump_scores_html %>% 
  html_table(fill = TRUE) ->
  trump_score_list

trump_scores_house <- trump_score_list[[2]] 

colnames(trump_scores_house) <- c(
  'member',
  'member_last',
  'party',
  'district',
  'trump_score',
  'trump_margin',
  'predicted_score',
  'trump_plus_minus'
)

trump_scores_house %>% 
  mutate(trump_score = readr::parse_number(trump_score)) %>% 
  select(member, member_last, district, 
         trump_score, trump_margin, trump_plus_minus) ->
  trump_scores_house

save(trump_scores_house, file = 'data/trump_scores_house.rda')
