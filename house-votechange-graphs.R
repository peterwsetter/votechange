#' Copyright Peter W Setter 2018
#' Create data products of the House Vote Change
#' Input:
#' - house_district_2018.rda
#' - trump_scores_hosue.rda
#' - elections package
#' Output:
#' - votechange_state_graph.rda
#' - votechange_state_slideshow.rda
#' - votechange_trumpscore_graph.rda
#' - votechange_trumpdiff_graph.rda 

## Setup
load('../elections/data/house_precincts_2016.rda')
load('../elections/data/presidential_precincts_2016.rda')
load('data/house_district_2018.rda')
load('data/trump_scores_house.rda')

# States to include in the analysis
CURRENT_STATES <- c('Minnesota', 'Indiana', 'Pennsylvania',
                    'Iowa', 'Michigan', 'North Carolina',
                    'Ohio', 'Arizona',
                    'Florida', 'Texas', 'Virginia',
                    'New Hampshire', 'Wisconsin')

# Exclude these districts
# Lacked candidates from both parties in 2016
#   --> change in vote extreme outliers
EXCLUDE_DISTRICTS <- c('WI-3', 'TX-34')

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(slickR)

# Custom functions
`%~%` <- function(lhs, rhs) grepl(rhs, lhs)

`%!in%` <- function(x,y)!(`%in%`(x,y))

votechange_chart <- list(
  geom_col(position = 'dodge'),
  geom_text(aes(label = label_text,
                hjust = 1),
            #source: https://stackoverflow.com/a/40250493
            position = position_dodge(width = .9),
            color = 'white'
  ),
  scale_fill_manual(values = c('#0015BC', '#FF0000'),
                    name = 'Party'),
  theme_classic(),
  theme(legend.position = 'bottom'),
  scale_y_continuous(labels = scales::percent_format()),
  coord_flip()
)

#####
## Compare the Total Votes for Each Party
#' The 2016 House results from `election` are listed by precincts. 
#' This data was aggregated for candidate totals in the district. 
#' One flaw in this dataset is that Minnesota's Democractic-Farm-Labor party 
#' isn't labeled as "democratic". Before summarizing, party labels will be 
#' adjusted to include the DFL. 

house_precincts_2016 %>% 
  filter(year == 2016,
         office == 'US House') %>% 
  # Standardize party labels
  mutate(party = case_when(party %~% '^[D,d]' ~ 'D',
                           party %~% '^[R,r]' ~ 'R',
                           TRUE ~ 'O'),
         candidate = candidate_normalized
  ) %>% 
  group_by(year, state, district, candidate, party) %>% 
  summarize(candidate_votes = sum(votes)) %>% 
  group_by(year, state, district, party) %>% 
  filter(candidate_votes == max(candidate_votes)) %>% 
  ungroup() ->
  house_district_2016

state_abbrevs <- data_frame(
  state = state.name,
  state_abbrev = state.abb
)

house_district_2016 %>% 
  # Just include states 
  filter(state %in% CURRENT_STATES) %>% 
  bind_rows(house_district_2018) %>% 
  inner_join(state_abbrevs,
             by = c('state' = 'state')) %>% 
  mutate(state_district = paste0(state_abbrev,'-', district)) ->
  house_district

house_district %>% 
  group_by(state, party, year) %>% 
  summarize(party_votes = sum(candidate_votes)) %>% 
  group_by(state, party) %>% 
  filter(n() == 2) %>% 
  arrange(year, .by_group = TRUE) %>% 
  summarize(votechange_prop = round(last(party_votes)/first(party_votes), 2)) %>% 
  filter(party != 'O') %>% 
  mutate(label_text = paste0(round(votechange_prop*100), '%')) %>% 
  ggplot(aes(state, votechange_prop, 
             fill = party)) +
  votechange_chart +
  labs(title = 'Votes Cast for House Candidates:\n2018 Relative to 2016',
       y = 'House Vote Total 2018/2016',
       x = NULL) ->
  votechange_state_graph

save(votechange_state_graph,
     file = 'data-products/votechange_state_graph.rda')

#votechange_state_graph

# Create slideshow with the change at the district level

house_district %>% 
  group_by(state, district, state_district, party, year) %>% 
  summarize(party_votes = sum(candidate_votes)) %>% 
  group_by(state, district, state_district, party) %>% 
  filter(n() == 2) %>% 
  arrange(year, .by_group = TRUE) %>% 
  summarize(votechange_prop = round(last(party_votes)/first(party_votes), 2)) ->
  district_votechange

district_votechange %>%
  filter(party != 'O',
         state != 'Pennsylvania',
         state_district %!in% EXCLUDE_DISTRICTS) %>% 
  group_by(state_district) %>% 
  filter(n() == 2) %>% 
  mutate(label_text = paste0(round(votechange_prop*100), '%'),
         district = as.integer(district)) %>% 
  group_by(state) %>% 
  nest() %>% 
  mutate(graphs = purrr::map2(state, data,
                              ~ggplot(.y, aes(factor(district),
                                              votechange_prop, 
                                              fill = party)) +
                                votechange_chart +
                                labs(title = .x,
                                     x = NULL,
                                     y = 'House Vote Total 2018/2016')
  ) 
  ) ->
  graph_df

# Arizona
graph_df %>% 
  filter(state == 'Arizona') %>% 
  pull(graphs) ->
  arizona_votechange

graph_df %>% 
  filter(state == 'Texas') %>% 
  pull(graphs) ->
  texas_votechange

save(arizona_votechange, texas_votechange,
     file = 'data-products/arizona_texas.rda')

# All graphs for slideshow
graph_df %>% 
  pull(graphs) %>% 
  purrr::map(function(gr) svglite::xmlSVG(show(gr), standalone = TRUE)) %>% 
  purrr::map_chr(function(sv) paste0('data:image/svg+xml;utf8,', as.character(sv))) ->
  state_votechange_slideshow

save(state_votechange_slideshow, 
     file = 'data-products/state_votechange_slideshow.rda')

#slickR::slickR(state_votechange_slideshow)


#####
## Compare to Trump Score
district_votechange %>% 
  filter(party != 'O',
         state != 'Pennsylvania') %>% 
  group_by(state_district) %>% 
  filter(n() == 2) %>% 
  filter(state_district %!in% EXCLUDE_DISTRICTS) %>% 
  inner_join(trump_scores_house,
             by = c('state_district' = 'district')) ->
  vc_ts

# Lines for visualization
vc_ts %>% 
  group_by(state_district, trump_plus_minus) %>% 
  summarize(upper = max(votechange_prop),
            lower = min(votechange_prop)) %>% 
  mutate(party = 'R') %>% 
  select(-trump_plus_minus) ->
  diff_line_ts

# Create interactive graph
vc_ts %>% 
  left_join(diff_line_ts,
            by = c('state_district' = 'state_district',
                   'party' = 'party'))  %>% 
  ggplot(aes(trump_plus_minus, votechange_prop, color = party,
             text = state_district), inherit.aes = FALSE) +
  geom_linerange(aes(ymin = lower, ymax = upper), 
                 color = 'gray', alpha = 0.5) +
  geom_point() +
  scale_color_manual(values = c('#0015BC', '#FF0000'),
                     name = 'Party') +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  labs(lab = 'House Vote Total 2018 Relative to 2016 vs 538 Trump +/-',
       x = 'Trump +/-',
       y = 'House Vote Total 2018/2016') ->
  votechange_trumpscore_graph

save(votechange_trumpscore_graph,
     file = 'data-products/votechange_trumpscore_graph.rda')

#plotly::ggplotly(votechange_trumpscore_graph, tooltip = c('text', 'trump_plus_minus', 'votechange_prop'))

vc_ts %>% 
  ggplot(aes(trump_plus_minus, votechange_prop)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~party) +
  labs(title = 'Trump +/- Had Nonproportional Impact',
       x = 'Trump +/-',
       y = 'House Vote Total 2018/2016') ->
  trump_score_model_graph

save(trump_score_model_graph, file = 'data-products/trump_score_model_graph.rda')

vc_ts %>% 
  group_by(party) %>% 
  nest() %>% 
  mutate(fit = map(data, ~lm(votechange_prop ~ trump_plus_minus,
                             data = .))) %>% 
  unnest(fit %>% map(broom::tidy)) %>%
  select(party, term, estimate) %>% 
  spread(key = term, value = estimate) %>% 
  mutate(
    # Expected prop if Trump +/- = 0
    baseline = `(Intercept)`*100, 
    # Change in prop with change in Trump +/-
    trump_change = trump_plus_minus*100 
         ) %>% 
  select(party, baseline, trump_change) ->
  trump_score_model

save(trump_score_model, file = 'data-products/trump_score_model.rda')  

#####
## Compare Change in House Votes Cast to Difference 
##   Between Republician House and Presidential in 2016

house_precincts_2016 %>% 
  filter(office == 'US House') %>% 
  distinct(state_postal, district, county_name) ->
  county_district_lookup

presidential_precincts_2016 %>% 
  filter(office == 'US President',
         candidate_normalized == 'trump') %>% 
  select(state_postal, county_name, votes) %>% 
  left_join(county_district_lookup,
            by = c('state_postal' = 'state_postal',
                   'county_name' = 'county_name')) %>% 
  mutate(state_district = paste0(state_postal, '-', district)) %>% 
  group_by(state_district) %>% 
  summarize(prez_votes = sum(votes)) ->
  trump_district_2016

house_district_2016 %>% 
  filter(party == 'R') %>% 
  inner_join(state_abbrevs,
             by = c('state' = 'state')) %>% 
  mutate(state_district = paste0(state_abbrev, '-', district)) %>% 
  ungroup() %>% 
  select(state_district, house_votes = candidate_votes) ->
  gop_district_2016

trump_district_2016 %>% 
  inner_join(gop_district_2016,
             by = c('state_district' = 'state_district')) %>%
  mutate(prez_diff = (house_votes - prez_votes)/prez_votes) %>%
  select(prez_diff, state_district) ->
  house_trump_diff

vc_ts %>% 
  # Exclude Texas and Arizona because of error in election dataset
  filter(state %!in% c('Texas', 'Arizona')) %>% 
  left_join(diff_line_ts,
            by = c('state_district' = 'state_district',
                   'party' = 'party'))  %>% 
  left_join(house_trump_diff,
            by = c('state_district' = 'state_district')) ->
  votechange_trumpdiff

votechange_trumpdiff %>% 
  ggplot(aes(prez_diff, votechange_prop, color = party,
             text = state_district), inherit.aes = FALSE) +
  geom_point() +
  scale_color_manual(values = c('#0015BC', '#FF0000'),
                     name = 'Party') +
  geom_linerange(aes(ymin = lower, ymax = upper), color = 'gray') +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  labs(x = 'Percent Difference Between GOP House and Presidential Votes',
       y = 'House Vote Total 2018/2016') ->
  votechange_trumpdiff_graph

save(votechange_trumpdiff_graph,
     file = 'data-products/votechange_trumpdiff_graph.rda')

#plotly::ggplotly(votechange_trumpdiff_graph, tooltip = c('text', 'trump_plus_minus', 'votechange_prop'))

votechange_trumpdiff %>% 
  ggplot(aes(prez_diff, votechange_prop)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~party) +
  labs(title = 'Votes Cast Was Not Related to \nDifference with Presidential',
       x = 'Percent Difference Between GOP House and Presidential Votes',
       y = 'House Vote Total 2018/2016') ->
  trump_diff_model_graph

save(trump_diff_model_graph, file = 'data-products/trump_diff_model_graph.rda')


votechange_trumpdiff %>% 
  group_by(party) %>% 
  nest() %>% 
  mutate(fit = map(data, ~lm(votechange_prop ~ prez_diff,
                             data = .))) %>% 
  unnest(fit %>% map(broom::tidy)) %>% 
  View()

#' There does not appear to be a relationship here
