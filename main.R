library(readr)
library(readxl)
library(lubridate)
library(magrittr)
library(dplyr)
library(stringr)
library(purrr)

source('run_machine_learning.R')

# Set paths
data_folder <- "data/"
file_historical_matches <- paste0(data_folder, "historical_matches.csv")
file_future_matches <- paste0(data_folder, "future_matches.csv")
file_fifa_rankings <- paste0(data_folder, "fifa_ranking.csv")

# Read input files
historical_matches <- read_csv(file_historical_matches)
future_matches <- read_csv(file_future_matches)
fifa_rankings <- read_csv(file_fifa_rankings)

# Functions to translate FIFA country names to match data country names
translation_encoding <- data.frame(
  'pattern' = c('\\<U\\+0441\\>', ' and '),
  'replacement' = c('c', ' & '),
  stringsAsFactors = FALSE
)

translation_countries <- data.frame(
  'pattern'=c('China PR', 'Republic of Ireland', "C.*te d'Ivoire", 'E.*uador',
              'FYR Macedonia', 'Korea DPR', 'Northern Ireland', 'El Salvador',
              'Korea Republic', 'St. Kitts & Nevis', 'USA',
              'Cape Verde Islands'),
  'replacement'=c('China', 'Ireland', 'Ivory Coast', 'Ecuador', 'Macedonia',
                  'North Korea', 'North. Ireland', 'Salvador', 'South Korea',
                  'St. Kitts/Nevis', 'United States ', 'Cape Verde'),
  stringsAsFactors = FALSE)

fix_country_name <- function(string) {
  for (i in translation_encoding %>% nrow() %>% seq()) {
    string %<>% str_replace(translation_encoding$pattern[i],
                            translation_encoding$replacement[i])
  }
  for (i in translation_countries %>% nrow() %>% seq()) {
    string %<>% str_replace(translation_countries$pattern[i],
                            translation_countries$replacement[i])
  }
  return(string)
}

# Prep FIFA rankings (parse date and rename countries)
fifa_rankings %<>%
  mutate(date_ranking = date(parse_date_time(date, orders='dmy')),
         country_name = fix_country_name(country_name)) %>%
  select(-date, -country_code)

matches <- bind_rows(historical_matches, future_matches)

# Prep data matches (create additional variables)
matches %<>%
  mutate(date = date(make_datetime(year=year, month=month, day=day)),
         result = sign(local_score - visit_score),
         local_match = local_team == country,
         visit_match = visit_team == country,
         local_team = fix_country_name(local_team),
         visit_team = fix_country_name(visit_team),
         match_id = row_number(),
         friendly = match_type=='Friendly')

# Merge game data with fifa data (first for local teams, then for visit teams)
matches %<>% inner_join(fifa_rankings, by=c('local_team'='country_name')) %>%
  filter(date > date_ranking) %>%
  group_by(match_id) %>%
  summarise_each(funs(last)) %>%
  rename(local_rank=rank,
         local_points=points,
         local_previous_points=previous_points) %>%
  select(-date_ranking)

matches %<>% inner_join(fifa_rankings, by=c('visit_team'='country_name')) %>%
  filter(date > date_ranking) %>%
  group_by(match_id) %>%
  summarise_each(funs(last)) %>%
  rename(visit_rank=rank,
         visit_points=points,
         visit_previous_points=previous_points) %>%
  select(-date_ranking)

# create useful variables
matches %<>%
  mutate(diff_rank=local_rank - visit_rank,
         diff_points=local_points - visit_points,
         local_change=local_points - local_previous_points,
         visit_change=visit_points - visit_previous_points)

# Create list of match results separated for local and visit teams
matches_local <- matches %>%
  select(date, result, matches('local'), matches('visit'))
colnames(matches_local) <- colnames(matches_local) %>%
  str_replace('local_', '') %>%
  str_replace('visit', 'opponent')

matches_visit <- matches %>%
  select(date, result, matches('local'), matches('visit'))
colnames(matches_visit) <- colnames(matches_visit) %>%
  str_replace('visit_', '') %>%
  str_replace('local', 'opponent')

matches_single <- bind_rows(matches_local, matches_visit) %>%
  arrange(date, team)

# Function to create summary of historical results of a team
create_summary <- function(data, date_i, team_i, type) {
  n_hist <- 3
  summary <- data %>%
    filter(team==team_i, date < date_i) %>%
    top_n(n_hist, date) %>%
    summarise(mean_score=mean(score),
              mean_result=mean(result),
              mean_opponent_score=mean(opponent_score),
              mean_opponent_rank=mean(opponent_rank),
              mean_opponent_points=mean(opponent_points),
              last_score=last(score),
              last_result=last(result),
              last_opponent_score=last(opponent_score),
              last_opponent_rank=last(opponent_rank),
              last_opponent_points=last(opponent_points),
              complete=n()>=n_hist
              )

  colnames(summary) <- paste(type, colnames(summary), sep='_')
  return(summary)
}

# Calculate historical results and merge with game data
matches %<>%
  by_row(function(x) {create_summary(matches_single, x$date, x$local_team,
                                     'local')}, .collate='cols') %>%
  by_row(function(x) {create_summary(matches_single, x$date, x$visit_team,
                                     'visit')}, .collate='cols') %>%
  filter(local_complete1, visit_complete1)

# Create predictions on historical data to test the performance of the model
y <- matches %>%
  filter(!is.na(result)) %>%
  select(result) %>%
  map(as.factor) %>%
  unlist()

x <- matches %>%
  filter(!is.na(result)) %>%
  select(matches('_rank'), matches('_points'), matches('_match'),
         matches('_change'), matches('_last_'), matches('_mean_'), friendly)

subset <- (y %>% length * 0.7) %>% round %>% seq
output <- run_machine_learning(x=x, y=y, subset=subset, model='nn',
                               hidden=c(10), hidden_dropout_ratios=c(0.2),
                               epochs=100, activation='MaxoutWithDropout')

print(output$hitrate_train)
print(output$hitrate_test)
print(output$crosstable)

# create predictions on future data
y <- matches %>%
  select(result) %>%
  map(as.factor) %>%
  unlist()

x <- matches %>%
  select(matches('_rank'), matches('_points'), matches('_match'),
         matches('_change'), matches('_last_'), matches('_mean_'), friendly)

subset <- matches %>% filter(!is.na(result)) %>% nrow %>% seq
output <- run_machine_learning(x=x, y=y, subset=subset, model='nn',
                               hidden=c(10), hidden_dropout_ratios=c(0.2),
                               epochs=100, activation='MaxoutWithDropout')

predictions <- output$votes_test %>%
  select(p1, p0, p.1) %>%
  rename(p_local_win=p1, p_draw=p0, p_visit_win=p.1)

predictions <- bind_cols(future_matches, predictions)
write_csv(predictions, path=paste0(data_folder, 'predictions.csv'))
