library(readr)
library(readxl)
library(lubridate)
library(magrittr)
library(dplyr)
library(stringr)
library(purrr)

library(vfmodels)

source('settings.R')
source('functions.R')

file_data_matches <- paste0(data_folder, "data_matches.csv")
file_fifa_rankings <- paste0(data_folder, "fifa_ranking.csv")

data_matches <- read_csv(file_data_matches)
fifa_rankings <- read_csv(file_fifa_rankings)

# prep fifa rankings
fifa_rankings %<>%
  mutate(date_ranking = date(parse_date_time(date, orders='dmy')),
         country_name = fix_country_name(country_name)) %>%
  select(-date, -country_code)

# prep data_matches_games
data_matches_games %<>%
  mutate(date = date(make_datetime(year=year, month=month, day=day)),
         result = sign(local_score - visit_score),
         local_match = local_team == country,
         visit_match = visit_team == country,
         local_team = fix_country_name(local_team),
         visit_team = fix_country_name(visit_team),
         match_id = row_number(),
         friendly = match_type=='Friendly')

# merge game data with fifa data
data_matches_games %<>% inner_join(fifa_rankings, by=c('local_team'='country_name')) %>%
  filter(date > date_ranking) %>%
  group_by(match_id) %>%
  summarise_each(funs(last)) %>%
  rename(local_rank=rank,
         local_points=points,
         local_previous_points=previous_points) %>%
  select(-date_ranking)
data_matches_games %<>% inner_join(fifa_rankings, by=c('visit_team'='country_name')) %>%
  filter(date > date_ranking) %>%
  group_by(match_id) %>%
  summarise_each(funs(last)) %>%
  rename(visit_rank=rank,
         visit_points=points,
         visit_previous_points=previous_points) %>%
  select(-date_ranking)

# create useful variables
data_matches_games %<>%
  mutate(diff_rank=local_rank - visit_rank,
         diff_points=local_points - visit_points,
         local_change=local_points - local_previous_points,
         visit_change=visit_points - visit_previous_points)

# add info about previous result
data_games <- data_matches_games %>%
  select(date, local_team, visit_team, local_score, visit_score, result)
data1 <- data_games %>%
  select(date, local_team, local_score, result, visit_score) %>%
  rename(team=local_team,
         score=local_score,
         score_against=visit_score)
data2 <- data_games %>%
  select(date, visit_team, visit_score, result, local_score) %>%
  mutate(result=-result) %>%
  rename(team=visit_team,
         score=visit_score,
         score_against=local_score)
data_games2 <- bind_rows(data1, data2) %>%
  arrange(date, team)

create_summary <- function(data, date_i, team_i, type) {
  summary <- data %>%
    filter(team==team_i, date < date_i) %>%
    top_n(3, date) %>%
    summarise(avg_score=mean(score),
              avg_result=mean(result),
              avg_score_against=mean(score_against),
              previous_score=last(score),
              previous_result=last(result),
              previous_score_against=last(score_against)
              )
  colnames(summary) <- paste(type, colnames(summary), sep='_')
  return(summary)
}

data_matches_games %<>% by_row(function(x) {create_summary(data_games2, x$date, x$local_team, 'local')}, .collate='cols') %>%
  by_row(function(x) {create_summary(data_games2, x$date, x$visit_team, 'visit')}, .collate='cols') %>%
  na.omit()

# make predictions
y <- data_matches_games %>% select(result) %>% map(as.factor) %>% unlist()
x <- data_matches_games %>% select(matches('_rank'), matches('_points'), matches('_match'), matches('_change'),
                                   matches('_previous_'), matches('_avg_'), friendly)
subset <- (data_matches_games %>% nrow() * 0.7) %>% round %>% seq
output <- run_machine_learning(x=x, y=y, subset=subset, model='nn',
                             hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')

print(output$hitrate_train)
print(output$hitrate_test)
print(output$crosstable)

# merge game data with fifa data
match_codes %<>%
  mutate(date=lubridate::today(),
         friendly=FALSE,
         match_id=row_number(),
         local_match=FALSE,
         visit_match=FALSE)
match_codes$local_match[match_codes$`TEAM 1`=='France'] <- TRUE
match_codes$visit_match[match_codes$`TEAM 2`=='France'] <- TRUE
match_codes %<>% left_join(fifa_rankings, by=c('TEAM 1'='country_name')) %>%
  filter(date > date(date_ranking)) %>%
  group_by(match_id) %>%
  summarise_each(funs(last)) %>%
  rename(local_rank=rank,
         local_points=points,
         local_previous_points=previous_points) %>%
  select(-date_ranking)
# change country name of italy (bad, bad data set)
match_codes$`TEAM 2` %<>% str_replace('Italia', 'Italy')
match_codes %<>% inner_join(fifa_rankings, by=c('TEAM 2'='country_name')) %>%
  filter(date > date(date_ranking)) %>%
  group_by(match_id) %>%
  summarise_each(funs(last)) %>%
  rename(visit_rank=rank,
         visit_points=points,
         visit_previous_points=previous_points) %>%
  select(-date_ranking)

# create useful variables
match_codes %<>%
  mutate(diff_rank=local_rank - visit_rank,
         diff_points=local_points - visit_points,
         local_change=local_points - local_previous_points,
         visit_change=visit_points - visit_previous_points)

# add info about previous result
match_codes %<>% by_row(function(x) {create_summary(data_games2, today(), x$`TEAM 1`, 'local')}, .collate='cols') %>%
  by_row(function(x) {create_summary(data_games2, today(), x$`TEAM 2`, 'visit')}, .collate='cols')

# model_results2 <- run_machine_learning(x=x, y=y)
x2 <- bind_rows(x, match_codes) %>% select(-date, -matches('TEAM'), -`MATCH NUMBER`, -match_id)
y2 <- c(as.character(y), rep(NA, nrow(match_codes))) %>% as.factor()
subset2 <- seq(1:nrow(x))
output2 <- run_machine_learning(x=x2, y=y2, subset=subset2, model='nn',
                     hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
predictions <- output2$votes_test
colnames(predictions) <- paste0('win_', colnames(predictions))

submission <- bind_cols(match_codes %>% select(matches('MATCH NUMBER')), predictions)
write_csv(submission, path=paste0(data_folder, 'Submission_', name, '.csv'))


#### voorspel doelpunten thuisteam
y_local <- data_matches_games %>% select(local_score) %>% unlist() %>% as.factor()
output_local <- run_machine_learning(x=x, y=y_local, subset=subset, model='nn',
                                hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
print(output_local$hitrate_train)
print(output_local$hitrate_test)
print(output_local$crosstable)

y_local2 <- c(as.character(y_local), rep(NA, nrow(match_codes))) %>% as.factor()
output_local2 <- run_machine_learning(x=x2, y=y_local2, subset=subset2, model='nn',
                                     hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
predictions_local <- output_local2$votes_test
colnames(predictions_local) <- paste0('local_', colnames(predictions_local))


#### voorspel doelpunten uitteam
y_visit <- data_matches_games %>% select(visit_score) %>% unlist() %>% as.factor()
output_visit <- run_machine_learning(x=x, y=y_visit, subset=subset, model='nn',
                                     hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
print(output_visit$hitrate_train)
print(output_visit$hitrate_test)
print(output_visit$crosstable)

y_visit2 <- c(as.character(y_visit), rep(NA, nrow(match_codes))) %>% as.factor()
output_visit2 <- run_machine_learning(x=x2, y=y_visit2, subset=subset2, model='nn',
                                      hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
predictions_visit <- output_visit2$votes_test
colnames(predictions_visit) <- paste0('visit_', colnames(predictions_visit))

submission2 <- bind_cols(submission, predictions_local %>% data.frame(), predictions_visit %>% data.frame())
write_csv(submission2, path=paste0(data_folder, 'full_prediction.csv'))

# hitrate win
test <- bind_cols(output_local$predictions_test %>% data.frame(), output_visit$predictions_test %>% data.frame())
colnames(test) <- c('local_score', 'visit_score')
test %<>% mutate_each(funs(as.character)) %>% mutate_each(funs(as.numeric))
test %<>% mutate(win=sign(local_score - visit_score))
print(mean(test$win==y[-subset] %>% as.character() %>% as.numeric()))

# hitrate score
test %<>% mutate(score=paste(local_score, visit_score, sep='-'))
print(mean(test$score==y_combined[-subset] %>% as.character()))

#### voorspel uitslag in één model
y_combined <- data_matches_games %>% transmute(result=paste(local_score, visit_score, sep='-')) %>% unlist %>% as.factor()
output_combined <- run_machine_learning(x=x, y=y_combined, subset=subset, model='nn',
                                     hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
predictions_combined <- strsplit(as.character(output_combined$predictions), split='-') %>%
  unlist() %>%
  as.numeric() %>%
  matrix(ncol=2, byrow=TRUE) %>%
  data.frame()
predictions_combined$result <- sign(predictions_combined[,1] - predictions_combined[,2])

# hitrates win
print(mean(as.character(predictions_combined$result)[subset]==as.character(y)[subset]))
print(mean(as.character(predictions_combined$result)[-subset]==as.character(y)[-subset]))
# hitrates scores
print(output_combined$hitrate_train)
print(output_combined$hitrate_test)

y_combined2 <- c(as.character(y_combined), rep(NA, nrow(match_codes))) %>% as.factor()
output_combined2 <- run_machine_learning(x=x2, y=y_combined2, subset=subset2, model='nn',
                                        hidden=c(10), hidden_dropout_ratios=c(0.2), epochs=100, activation='MaxoutWithDropout')
predictions_combined2 <- output_combined2$votes_test
colnames(predictions_combined2) <- paste0('result_', colnames(predictions_combined2))

submission3 <- bind_cols(submission2, predictions_combined2)
write_csv(submission3, path=paste0(data_folder, 'full_prediction_2.csv'))

#### voorspel poule rankings
schema <- read_csv(file.path(data_folder, 'schema.csv'))
join_match <- function(row) {
  t <- match_codes %>% filter(`TEAM 1` %in% c(row)) %>% filter(`TEAM 2` %in% c(row)) %>%
    select(`MATCH NUMBER`, `TEAM 1`, `TEAM 2`)
  t %<>% inner_join(submission, by='MATCH NUMBER')
  if (row$local_team==t$`TEAM 1`) {
    local_points=3*t$`win 1`+t$`win 0`
    visit_points=3*t$`win -1`+t$`win 0`
  } else {
    local_points=3*t$`win -1`+t$`win 0`
    visit_points=3*t$`win 1`+t$`win 0`
  }
  return(data.frame(local_points, visit_points))
}
schema %<>% by_row(join_match, .collate='cols')

countries <- data.frame(country=unique(c(schema$local_team, schema$visit_team)), stringsAsFactors = FALSE)
count_points <- function(country) {
  schema %>% filter(local_team==country) %>% select(local_points1) %>% sum() +
    schema %>% filter(visit_team==country) %>% select(visit_points1) %>% sum()
}
countries$points <- map_dbl(countries$country, count_points)
get_group <- function(country) {
  schema %>% filter(local_team==country) %>% select(group) %>% slice(1) %>% as.character()
}
countries %<>% mutate(group= map_chr(countries$country, get_group))
countries %<>% arrange(group, -points)
countries %<>% mutate(rank=rep(1:4, 6))
countries %<>% mutate(rank_code=paste0(group, rank))
