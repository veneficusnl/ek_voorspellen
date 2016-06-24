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
