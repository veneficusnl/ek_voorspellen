library(readr)
library(readxl)
library(lubridate)
library(magrittr)
library(dplyr)
library(stringr)
library(purrr)
library(vfmodels)

source('settings.R')

file_data_matches <- paste0(data_folder, "Data_Matches_", name, ".xlsx")
file_fifa_rankings <- paste0(data_folder, "fifa_ranking.csv")
file_country_codes <- paste0(data_folder, "country_codes.csv")
file_match_codes <- paste0(data_folder, "Match_Codes_", name, ".xlsx")

simplify <- function(string) {
  string %>% str_to_lower() %>% str_replace(' ', '_')
}

translation_encoding <- data.frame(
  'pattern' = c('\\<U\\+0441\\>', ' and '),
  'replacement' = c('c', ' & '),
  stringsAsFactors = FALSE
)

translation_countries <- data.frame(
  'pattern'=c('China PR', 'Republic of Ireland', "C.*te d'Ivoire", 'E.*uador', 'FYR Macedonia', 'Korea DPR',
              'Northern Ireland', 'El Salvador', 'Korea Republic', 'St. Kitts & Nevis', 'USA', 'Cape Verde Islands'),
  'replacement'=c('China', 'Ireland', 'Ivory Coast', 'Ecuador', 'Macedonia', 'North Korea', 'North. Ireland',
                  'Salvador', 'South Korea', 'St. Kitts/Nevis', 'United States ', 'Cape Verde'),
  stringsAsFactors = FALSE)

fix_country_name <- function(string) {
  for (i in translation_encoding %>% nrow() %>% seq()) {
    string %<>% str_replace(translation_encoding$pattern[i], translation_encoding$replacement[i])
  }
  for (i in translation_countries %>% nrow() %>% seq()) {
    string %<>% str_replace(translation_countries$pattern[i], translation_countries$replacement[i])
  }
  return(string)
}

data_matches_games <- read_excel(file_data_matches, sheet=1)
colnames(data_matches_games) %<>% map_chr(simplify)
data_matches_type <- read_excel(file_data_matches, sheet=2)
colnames(data_matches_type) %<>% map_chr(simplify)

match_codes <- read_excel(file_match_codes, sheet=1)

fifa_rankings <- read_csv(file_fifa_rankings)

country_codes <- read_csv(file_country_codes)

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
x <- data_matches_games %>% select(matches('_rank'), matches('_points'), matches('_match'), matches('_change'), matches('_previous_'), matches('_avg_'))
subset <- (data_matches_games %>% nrow() * 0.7) %>% round %>% seq
model_results <- run_machine_learning(x=x, y=y, subset=subset)
results <- c(model_results$model$predicted %>% as.character() %>% as.numeric(), model_results$model$test$predicted %>% as.character() %>% as.numeric())
set <- c(rep('train', length(subset)), rep('test', length(y)-length(subset)))
data_results <- data_matches_games %>% bind_cols(data.frame(prediction=results)) %>% bind_cols(data.frame(set=set))
hitrate_train <- data_results %>% mutate(correct=result==prediction) %>% filter(set=='train') %>% select(correct) %>% unlist() %>% mean()
hitrate_test <- data_results %>% mutate(correct=result==prediction) %>% filter(set=='test') %>% select(correct) %>% unlist() %>% mean()
crosstable <- table(data_results$result, data_results$prediction)

print(hitrate_train)
print(hitrate_test)
print(crosstable)

# merge game data with fifa data
match_codes %<>%
  mutate(date=lubridate::today(),
         friendly=FALSE,
         match_id=row_number(),
         local_match=FALSE,
         visit_match=FALSE)
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

model_results2 <- run_machine_learning(x=x, y=y)
predictions <- predict(model_results2$model, match_codes, type='prob')
submission <- bind_cols(match_codes %>% select(matches('MATCH NUMBER')), predictions %>% data.frame())
colnames(submission)[2:4] <- paste('win', colnames(predictions))
write_csv(submission, path=paste0(data_folder, 'Submission_', name, '.csv'))


#### voorspel doelpunten
y_local <- data_matches_games %>% select(local_score) %>% unlist() %>% as.factor()
model_local_score <- run_machine_learning(x=x, y=y_local)
predictions_local <- predict(model_local_score$model, match_codes, type='prob')

y_visit <- data_matches_games %>% select(visit_score) %>% unlist() %>% as.factor()
model_visit_score <- run_machine_learning(x=x, y=y_visit)
predictions_visit <- predict(model_visit_score$model, match_codes, type='prob')

submission2 <- bind_cols(submission, predictions_local %>% data.frame(), predictions_visit %>% data.frame())
colnames(submission2)[5:(4+ncol(predictions_local))] <- paste('local', colnames(predictions_local))
colnames(submission2)[(5+ncol(predictions_local)):ncol(submission2)] <- paste('visit', colnames(predictions_visit))
write_csv(submission2, path=paste0(data_folder, 'full_prediction.csv'))
