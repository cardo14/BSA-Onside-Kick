library(nflfastR)
library(tidyverse)
all_data <- load_pbp(seasons = TRUE) # get data from all nfl seasons since 1999
names(all_data)
special_teams <- filter(all_data, special_teams_play == 1) # get all special teams plays
kickoffs <- filter(special_teams, play_type == "kickoff") # get all kickoffs
onside_kicks <- kickoffs[str_detect(unlist(kickoffs$desc) ,"kicks onside"), ] # get all onside kicks
successful_onside_kicks <- filter(onside_kicks, own_kickoff_recovery == 1) # get all onside kicks that were successful
# onside_kick_formation <- onside_kicks[((str_detect(unlist(onside_kicks$desc) ,"(Onside Kick formation)")) , ]
# get all onside kicks that are not surprises (either attempted with an onside kick formation or 
# less than 8 minutes in game and trailing
non_surprise_onside_kicks <- onside_kicks[((str_detect(unlist(onside_kicks$desc) ,"(Onside Kick formation)")) | 
                                             ((onside_kicks$game_seconds_remaining < 480) 
                                              & (onside_kicks$score_differential > 0))), ]
surprise_onside_kicks <- onside_kicks[!((str_detect(unlist(onside_kicks$desc) ,"(Onside Kick formation)")) | 
                                          ((onside_kicks$game_seconds_remaining < 480) 
                                           & (onside_kicks$score_differential > 0))), ]
# What is the success rate of non-surprise onside kicks?
non_surprise_success_rate <- round(nrow(filter(non_surprise_onside_kicks, 
                                                        own_kickoff_recovery == 1))/nrow(non_surprise_onside_kicks), digits = 2)
# What is the success rate of surprise onside kicks?
surprise_success_rate <- round(nrow(filter(surprise_onside_kicks, 
                                           own_kickoff_recovery == 1))/nrow(surprise_onside_kicks), digits = 2)
# Are onside kicks helpful?
mean(onside_kicks$epa)
median(onside_kicks$epa)

mean(non_surprise_onside_kicks$epa)
median(non_surprise_onside_kicks$epa)

mean(surprise_onside_kicks$epa)
median(surprise_onside_kicks$epa)

# Onside kicks in neutral situations(between 20% and 80% win probability)
neutral_situation_onside_kicks <- onside_kicks[onside_kicks$wp >= .2 & onside_kicks$wp <= .8, ]

# Are neutral situation onside kicks successful?
mean(neutral_situation_onside_kicks$epa)
median(neutral_situation_onside_kicks$epa)

# Success rate of neutral situation onside kicks
neutral_success_rate <- round(nrow(filter(neutral_situation_onside_kicks, 
            own_kickoff_recovery == 1))/nrow(neutral_situation_onside_kicks), digits = 2)

# On average, does recovering an onside kick or giving one up have a larger impact?
mean(onside_kicks[onside_kicks$epa > 0, ]$epa) # Filter greater than 0 to avoid situations where gaining field position is bad
mean(successful_onside_kicks$epa)




### Kicker Stuff
# frequency table of all kickers with successful onside kicks since 2001
best_kickers <- table(successful_onside_kicks$kicker_player_name)
sort(best_kickers)
# Create dataframe of kicker statistics
kicker_df <- as.tibble(as.data.frame(best_kickers))

# Update dataframe with all kickers that have attempted onside kicks
kicker_df <- full_join(x =as.tibble(as.data.frame(table(onside_kicks$kicker_player_name))),
                y = kicker_df, by = "Var1")
# Rename columns to make sense
kicker_df <- kicker_df %>%
  rename(c(Name = Var1, Attempts = Freq.x, Successful = Freq.y))
# Update NA values to 0 successful onside kicks
kicker_df[is.na(kicker_df)] <- 0
# Add onside kick successful percentage
kicker_df <- kicker_df %>%
  mutate("Percentage" = Successful/Attempts)
# Get total percentage
onside_kick_overall_rate <- round(sum(kicker_df$Successful)/sum(kicker_df$Attempts), digits = 2)


# Making plots
barplot(c(onside_kick_overall_rate, non_surprise_success_rate, neutral_success_rate),
        main = "Onside Kick Recovery Rates",
        xlab = "Game Situation",
        ylab = "Recovery Rate",
        names.arg = c("Overall", "Expected", "Surprise"),
        col = c('red', 'darkred', 'darkgreen'))
abline(h = 0.4, col = 'black', lty = 2, lwd = 4)

# Back to kicker stuff

kicker_df_over_10_attempts <- kicker_df %>%
  filter(Attempts >= 10)

kicker_df_over_10_attempts <- kicker_df_over_10_attempts %>%
  arrange(-Percentage, -Attempts)
library(gt)
library(scales)
library(viridis)
# Best kickers plot
kicker_df_over_10_attempts %>%
  gt() %>%
  tab_header(
    title = "Best Onside Kickers in the NFL",
    subtitle = "Since 2001, min 10 attempts"
  ) %>%
  fmt_percent(columns = Percentage, decimals = 0) %>%
  data_color(columns = Percentage, scales::col_quantile(viridis(30), c(0, 0.7)))


# Worst kickers plot
kicker_df_over_10_attempts <- kicker_df_over_10_attempts %>%
  arrange(Percentage, -Attempts)

kicker_df_over_10_attempts %>%
  gt() %>%
  tab_header(
    title = "Worst Onside Kickers in the NFL",
    subtitle = "Since 2001, min 10 attempts"
  ) %>%
  fmt_percent(columns = Percentage, decimals = 0) %>%
  data_color(columns = Percentage, scales::col_quantile(viridis(30), c(0, 0.7)))


# Kicker Z-Scores
z_stat <- function(p, p0, n) {
  (p - p0)/sqrt((p0 * (1 - p0))/n)
}
# z_stat(p = 10/23, p0 = onside_kick_overall_rate, n = 23)
z_score <- numeric(0)
for (i in seq_along(kicker_df$Name)) {
  z_score <- c(z_score, z_stat(p = kicker_df[i, 4], 
                                p0 = onside_kick_overall_rate,
                                n = kicker_df[i, 2]))
}
z_score <- unlist(z_score)
kicker_df$z_score <- round(unname(z_score), digits = 2)

# Best Kickers By Z-Score
kicker_df_over_10_attempts <- kicker_df %>%
  filter(Attempts >= 10) %>%
  arrange(-z_score)
# Best kickers plot
kicker_df_over_10_attempts %>%
  gt() %>%
  tab_header(
    title = "Best Onside Kickers in the NFL by Z-Score",
    subtitle = "Since 2001, min 10 attempts"
  ) %>%
  fmt_percent(columns = Percentage, decimals = 0) %>%
  fmt_number(columns = z_score) %>%
  data_color(columns = z_score, scales::col_quantile(viridis(30), c(-4, 4)))

# Worst Kickers By Z-Score
kicker_df_over_10_attempts <- arrange(kicker_df_over_10_attempts, z_score)
# Worst Kickers Plot
kicker_df_over_10_attempts %>%
  gt() %>%
  tab_header(
    title = "Worst Onside Kickers in the NFL by Z-Score",
    subtitle = "Since 2001, min 10 attempts"
  ) %>%
  fmt_percent(columns = Percentage, decimals = 0) %>%
  fmt_number(columns = z_score) %>%
  data_color(columns = z_score, scales::col_quantile(viridis(30), c(-4, 4)))

### Rule Changes
## Get onside kicks before and after 2018
onside_kicks_after_2018 <- onside_kicks %>%
  filter(season >= 2018)
onside_kicks_before_2018 <- onside_kicks %>%
  filter(season < 2018)
# Calculate difference in recovery rate
post_2018_success_rate <- nrow(filter(onside_kicks_after_2018, own_kickoff_recovery == 1))/
  nrow(onside_kicks_after_2018)
pre_2018_success_rate <- nrow(filter(onside_kicks_before_2018, own_kickoff_recovery == 1))/
  nrow(onside_kicks_before_2018)
# Get neutral situation onside kicks
post_2018_neutral <- onside_kicks_after_2018[onside_kicks_after_2018$wp >= .2 & 
                                               onside_kicks_after_2018$wp <= .8, ]
pre_2018_neutral <-  onside_kicks_before_2018[onside_kicks_before_2018$wp >= .2 & 
                                                onside_kicks_before_2018$wp <= .8, ]
# Calculate recovery rate in neutral situations
post_18_neutral_rate <- nrow(filter(post_2018_neutral, own_kickoff_recovery == 1))/
  nrow(post_2018_neutral)
pre_2018_neutral_rate <- nrow(filter(pre_2018_neutral, own_kickoff_recovery == 1))/
  nrow(pre_2018_neutral)

# Make plots displaying difference
par(mfrow=c(1,2))
barplot(c(pre_2018_success_rate, post_2018_success_rate),
        main = "Overall Recovery Rate",
        xlab = "Seasons",
        ylab = "Recovery Rate",
        names.arg = c("Pre-2018", "2018 and After"),
        col = c('darkgreen', 'skyblue'))
barplot(c(pre_2018_neutral_rate, post_18_neutral_rate),
        main = "Recovery Rate in Neutral Situations",
        xlab = "Seasons",
        ylab = "Recovery Rate",
        names.arg = c("Pre-2018", "2018 and After"),
        col = c('darkgreen', 'skyblue'))


#### Building the model
# library(caret)
# library(broom)
# # create a list of 80% of the rows in the original dataset we can use for training
# validation_index <- createDataPartition(onside_kicks$play_id, p=0.80, list=FALSE)
# # select 20% of the data for validation
# validation <- onside_kicks[-validation_index,]
# # use the remaining 80% of data to training and testing the models
# dataset <- onside_kicks[validation_index,]
# # select relevant columns
# dataset <- select(dataset, game_seconds_remaining, kick_distance, defteam_timeouts_remaining, def_wp, score_differential, 
#                   own_kickoff_recovery)
# # split into input and output
# x <- dataset[, 1:(ncol(dataset) - 1)]
# y <- dataset[, ncol(dataset)]
# plot(y)
# Generate a linear model to predict kickoff recovery

# Select relevant columns
onside_kicks_filtered <- select(onside_kicks, game_seconds_remaining, kick_distance, defteam_timeouts_remaining, def_wp, score_differential, 
                                          own_kickoff_recovery)
# Create linear model
linear <- lm(data = onside_kicks_filtered, own_kickoff_recovery ~ def_wp)
onside_kicks$predictions <- predict(linear)
lm_accuracy <- sum((round(onside_kicks$predictions) == onside_kicks$own_kickoff_recovery))/nrow(onside_kicks)

# Create logistic model
logistic <- glm(data = onside_kicks_filtered, own_kickoff_recovery ~ def_wp, family = binomial())
onside_kicks$predictions <- predict(logistic, type = "response")
logistic_accuracy <- sum((round(onside_kicks$predictions) == onside_kicks$own_kickoff_recovery))/nrow(onside_kicks)
sum(onside_kicks[onside_kicks$predictions > 0.5, ]$own_kickoff_recovery)/
  length(onside_kicks[onside_kicks$predictions > 0.5, ]$own_kickoff_recovery)
